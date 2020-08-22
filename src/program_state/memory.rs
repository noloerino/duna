use super::datatypes::*;
use crate::arch::*;
use std::collections::HashMap;
use std::marker::PhantomData;

type VirtPN = usize;
type PhysPN = usize;
type PageOffset = usize;

/// Represents a page of memory.
/// TODO implement default value
struct MemPage {
    /// To ensure the simulator doesn't waste space allocating the full size of the page,
    /// this backing store allocates doublewords lazily.
    backing: HashMap<PageOffset, DataDword>,
}

impl MemPage {
    fn new() -> MemPage {
        MemPage {
            backing: HashMap::new(),
        }
    }

    fn set_byte(&mut self, offs: PageOffset, value: DataByte) {
        let dword_offs = offs ^ 0b111;
        let byte_offs = offs & 0b111;
        let old_dword: u64 = self
            .backing
            .get(&dword_offs)
            .map(Clone::clone)
            .unwrap_or_else(DataDword::zero)
            .into();
        // Zero out byte so we can set it
        let shamt = byte_offs * 8;
        let mask = !(0xFFu64 << shamt);
        let val: u8 = value.into();
        let new_dword = (old_dword & mask) | ((val as u64) << shamt);
        self.backing.insert(dword_offs, new_dword.into());
    }

    fn get_byte(&self, offs: PageOffset) -> DataByte {
        let dword_offs = offs ^ 0b111;
        let byte_offs = offs & 0b111;
        let val: u64 = self
            .backing
            .get(&dword_offs)
            .map(Clone::clone)
            .unwrap_or_else(DataDword::zero)
            .into();
        // Shift and let truncating happen automatically with cast
        DataByte::from((val >> (byte_offs * 8)) as u8)
    }

    fn set_half(&mut self, offs: PageOffset, value: DataHalf) {
        let half: u16 = value.into();
        self.set_byte(offs, (half as u8).into());
        self.set_byte(offs + 1, ((half >> 8) as u8).into());
    }

    fn get_half(&self, offs: PageOffset) -> DataHalf {
        let lower: u8 = self.get_byte(offs).into();
        let upper: u8 = self.get_byte(offs + 1).into();
        DataHalf::from(((upper as u16) << 8) | (lower as u16))
    }

    fn set_word(&mut self, offs: PageOffset, value: DataWord) {
        let word: u32 = value.into();
        self.set_half(offs, (word as u16).into());
        self.set_half(offs + 2, ((word >> 16) as u16).into());
    }

    fn get_word(&self, offs: PageOffset) -> DataWord {
        let lower: u16 = self.get_half(offs).into();
        let upper: u16 = self.get_half(offs + 2).into();
        DataWord::from(((upper as u32) << 16) | (lower as u32))
    }

    fn set_doubleword(&mut self, offs: PageOffset, value: DataDword) {
        // Check alignment
        let lsb = offs & 0b111;
        let lower_offs = (offs >> 3) << 3;
        if lsb == 0 {
            self.backing.insert(lower_offs, value);
        } else {
            let val: u64 = value.into();
            // Get the two dwords that the value crosses
            let mut lower_val: u64 = self
                .backing
                .get(&lower_offs)
                .map(Clone::clone)
                .unwrap_or_else(DataDword::zero)
                .into();
            let lsb_shamt = (8 - lsb) * 8;
            // Shift to zero upper bytes to be replaced
            // e.g. if offset is 1, we need to replace upper 7 bytes
            lower_val = (lower_val << lsb_shamt) >> lsb_shamt;
            self.backing.insert(
                lower_offs,
                // e.g. if offset is 1, we want to only shift by 1 byte
                DataDword::from(lower_val | (val << (lsb * 8))),
            );
            let upper_offs = lower_offs + 8;
            let mut upper_val: u64 = self
                .backing
                .get(&upper_offs)
                .map(Clone::clone)
                .unwrap_or_else(DataDword::zero)
                .into();
            let msb_shamt = lsb * 8;
            // Shift to zero lower bytes to be replaced
            // e.g. if offset is 1, we need to replace lower 1 byte
            upper_val = (upper_val >> msb_shamt) << msb_shamt;
            self.backing.insert(
                upper_offs,
                // e.g. if offset is 1, we want to only store top 1 byte
                DataDword::from(upper_val | (val >> ((8 - lsb) * 8))),
            );
        }
    }

    fn get_doubleword(&self, offs: PageOffset) -> DataDword {
        // Check alignment
        let lsb = offs & 0b111;
        let lower_offs = (offs >> 3) << 3;
        if lsb == 0 {
            self.backing
                .get(&lower_offs)
                .map(Clone::clone)
                .unwrap_or_else(DataDword::zero)
        } else {
            // Get the two dwords that the value crosses
            let lower_val: u64 = self
                .backing
                .get(&lower_offs)
                .map(Clone::clone)
                .unwrap_or_else(DataDword::zero)
                .into();
            let upper_offs = lower_offs + 8;
            let upper_val: u64 = self
                .backing
                .get(&upper_offs)
                .map(Clone::clone)
                .unwrap_or_else(DataDword::zero)
                .into();
            // Shift components by needed number of bytes
            // For example, if the offset is 1, we right shift the lower dword by 1B to chop off the
            // byte at offset 0, and left shift the upper dword by 7 to eliminate all but the lowest
            // byte in it.
            DataDword::from((upper_val << ((8 - lsb) * 8)) | (lower_val >> (lsb * 8)))
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct MemFault<T: ByteAddress> {
    pub user_vaddr: T,
    pub cause: MemFaultCause,
}

impl<T: ByteAddress> MemFault<T> {
    /// Indicates a page fault at the provided address.
    pub fn pagefault_at_addr(user_vaddr: T) -> Self {
        MemFault {
            user_vaddr,
            cause: MemFaultCause::PageFault,
        }
    }

    /// Indicates a segfault at the provided address.
    pub fn segfault_at_addr(user_vaddr: T) -> Self {
        MemFault {
            user_vaddr,
            cause: MemFaultCause::SegFault,
        }
    }

    /// Indicates a bus error at the provided address.
    pub fn buserror_at_addr(user_vaddr: T) -> Self {
        MemFault {
            user_vaddr,
            cause: MemFaultCause::BusError,
        }
    }

    /// Checks that the provided address matches the desired alignment, raising a BusError if not.
    pub fn check_aligned(user_vaddr: T, width: DataWidth) -> Result<(), Self> {
        if !user_vaddr.is_aligned_to(width) {
            Err(Self::buserror_at_addr(user_vaddr))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum MemFaultCause {
    PageFault,
    SegFault,
    BusError,
}

/// Trait to define a virtual memory abstraction.
///
/// All direct memory operations are byte-addressed; behavior for unaligned accesses is left up to
/// the implementor.
///
/// Read/write operations should not directly map pages; they should instead produce a pagefault,
/// and the OS should perform the mapping independently.
///
/// Read operations will return either the requested data or a MemFault.
/// Write operations will return a result with an empty value or a MemFault.
pub trait Memory<T: ByteAddress> {
    /// Checks whether the provided virtual address is mapped.
    fn is_mapped(&self, addr: T) -> bool;

    /// Raises a page fault if the page is unmapped.
    fn fault_if_unmapped(&self, addr: T) -> Result<(), MemFault<T>> {
        if self.is_mapped(addr) {
            Ok(())
        } else {
            Err(MemFault::pagefault_at_addr(addr))
        }
    }

    /// Maps a page containing the provided address to physical memory if it is not already mapped.
    /// If the page cannot be mapped, then a MemFault is returned.
    /// If the page was successfully mapped or already mapped, then Ok(()) is returned.
    fn map_page(&mut self, addr: T) -> Result<(), MemFault<T>>;

    /// Stores an 8-bit byte to memory.
    fn set_byte(&mut self, addr: T, value: DataByte) -> Result<(), MemFault<T>>;
    /// Reads an 8-bit byte from memory.
    fn get_byte(&self, addr: T) -> Result<DataByte, MemFault<T>>;
    /// Stores a 16-bit halfword to memory.
    fn set_half(&mut self, addr: T, value: DataHalf) -> Result<(), MemFault<T>>;
    /// Reads a 16-bit halfword from memory.
    fn get_half(&self, addr: T) -> Result<DataHalf, MemFault<T>>;
    /// Stores a 32-bit word to memory.
    fn set_word(&mut self, addr: T, value: DataWord) -> Result<(), MemFault<T>>;
    /// Reads a 32-bit word from memory.
    fn get_word(&self, addr: T) -> Result<DataWord, MemFault<T>>;
    /// Stores a double word to memory.
    fn set_doubleword(&mut self, addr: T, value: DataDword) -> Result<(), MemFault<T>>;
    /// Reads a double word from memory.
    fn get_doubleword(&self, addr: T) -> Result<DataDword, MemFault<T>>;

    fn set(&mut self, addr: T, value: DataEnum) -> Result<(), MemFault<T>> {
        match value {
            DataEnum::Byte(v) => self.set_byte(addr, v),
            DataEnum::Half(v) => self.set_half(addr, v),
            DataEnum::Word(v) => self.set_word(addr, v),
            DataEnum::DoubleWord(v) => self.set_doubleword(addr, v),
        }
    }

    fn get(&self, addr: T, w: DataWidth) -> Result<DataEnum, MemFault<T>> {
        Ok(match w {
            DataWidth::Byte => DataEnum::Byte(self.get_byte(addr)?),
            DataWidth::Half => DataEnum::Half(self.get_half(addr)?),
            DataWidth::Word => DataEnum::Word(self.get_word(addr)?),
            DataWidth::DoubleWord => DataEnum::DoubleWord(self.get_doubleword(addr)?),
        })
    }
}

/// A simple memory backed by a hashmap; all addresses except the null address are considered to be
/// paged in.
/// Reads to uninitialized addresses always return 0.
/// Faults occur on unaligned accesses or accesses to the null pointer.
pub struct SimpleMemory<T: ByteAddress> {
    store: MemPage,
    _phantom: PhantomData<T>,
}

impl<T: ByteAddress> SimpleMemory<T> {
    pub fn new() -> SimpleMemory<T> {
        SimpleMemory {
            store: MemPage::new(),
            _phantom: PhantomData,
        }
    }
}

impl<T: ByteAddress> Memory<T> for SimpleMemory<T> {
    fn is_mapped(&self, addr: T) -> bool {
        addr.bits() != 0
    }

    fn map_page(&mut self, addr: T) -> Result<(), MemFault<T>> {
        if addr.bits() == 0 {
            Err(MemFault::<T>::segfault_at_addr(addr))
        } else {
            Ok(())
        }
    }

    fn set_byte(&mut self, addr: T, value: DataByte) -> Result<(), MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Byte)?;
        self.fault_if_unmapped(addr)?;
        self.store.set_byte(addr.bits() as usize, value);
        Ok(())
    }

    fn get_byte(&self, addr: T) -> Result<DataByte, MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Byte)?;
        self.fault_if_unmapped(addr)?;
        Ok(self.store.get_byte(addr.bits() as usize))
    }

    fn set_half(&mut self, addr: T, value: DataHalf) -> Result<(), MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Half)?;
        self.fault_if_unmapped(addr)?;
        self.store.set_half(addr.bits() as usize, value);
        Ok(())
    }

    fn get_half(&self, addr: T) -> Result<DataHalf, MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Half)?;
        self.fault_if_unmapped(addr)?;
        Ok(self.store.get_half(addr.bits() as usize))
    }

    fn set_word(&mut self, addr: T, value: DataWord) -> Result<(), MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Word)?;
        self.fault_if_unmapped(addr)?;
        self.store.set_word(addr.bits() as usize, value);
        Ok(())
    }

    fn get_word(&self, addr: T) -> Result<DataWord, MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Word)?;
        self.fault_if_unmapped(addr)?;
        Ok(self.store.get_word(addr.bits() as usize))
    }

    fn set_doubleword(&mut self, addr: T, value: DataDword) -> Result<(), MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::DoubleWord)?;
        self.fault_if_unmapped(addr)?;
        self.store.set_doubleword(addr.bits() as usize, value);
        Ok(())
    }

    fn get_doubleword(&self, addr: T) -> Result<DataDword, MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::DoubleWord)?;
        self.fault_if_unmapped(addr)?;
        Ok(self.store.get_doubleword(addr.bits() as usize))
    }
}

impl<T: ByteAddress> Default for SimpleMemory<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Maps a virtual page to a physical one.
#[derive(Copy, Clone)]
struct PTEntry {
    /// The virtual page number.
    pub vpn: VirtPN,
    /// The physical page number.
    pub ppn: PhysPN,
}

/// A memory with a fully associative LRU linear page table.
/// The zero page is never considered mapped, and all accesses must be aligned.
pub struct LinearPagedMemory<T: ByteAddress> {
    page_offs_len: usize,
    page_count: usize,
    page_table: Vec<PTEntry>,
    phys_mem: HashMap<PhysPN, MemPage>,
    paged_out: HashMap<VirtPN, MemPage>,
    _phantom: PhantomData<T>,
}

impl<T: ByteAddress> LinearPagedMemory<T> {
    /// Initializes the memory. The number of pages is computed from the physical address and
    /// page sizes.
    /// * phys_addr_len: The number of bits in a physical address. The number of bytes of available
    ///                  physical memory is given by 2 to the power of this number.
    /// * page_offs_len: The number of bits needed to index a page. The number of bytes in a page is
    ///                  likewise 2 to the power of this number.
    pub fn new(phys_addr_len: usize, page_offs_len: usize) -> Self {
        // TODO add assertions on page table parameters to ensure nothing is invalid
        let page_count = 1 << (phys_addr_len - page_offs_len);
        LinearPagedMemory {
            page_offs_len,
            page_count,
            page_table: Vec::with_capacity(page_count),
            phys_mem: HashMap::new(),
            paged_out: HashMap::new(),
            _phantom: PhantomData,
        }
    }

    fn get_vpn(&self, addr: T) -> VirtPN {
        let bits = addr.bits();
        (bits >> self.page_offs_len) as usize
    }

    fn get_page_offs(&self, addr: T) -> PageOffset {
        // the page offset comes from the lower page_offs_len bits
        let bits = addr.bits();
        let lsb_mask = !((-1i64 as u64) << self.page_offs_len);
        (bits & lsb_mask) as usize
    }

    fn lowest_free_ppn(&self) -> Option<PhysPN> {
        for i in 0..self.page_count {
            if !self.page_table.iter().any(|&e| e.ppn == i) {
                return Some(i);
            }
        }
        None
    }

    /// Gets a reference the page in which this address occurs.
    /// Raises a page fault if it is unmapped.
    ///
    /// Note: This should page fault instead of segfaulting on an access to the null page, as the
    /// OS should make a call to map_page to handle the page fault. Only then should the segfault
    /// occur.
    fn get_page(&self, addr: T) -> Result<&MemPage, MemFault<T>> {
        let vpn = self.get_vpn(addr);
        if let Some(pte) = self.page_table.iter().find(|&e| e.vpn == vpn) {
            Ok(self.phys_mem.get(&pte.ppn).unwrap())
        } else {
            Err(MemFault::pagefault_at_addr(addr))
        }
    }

    /// Gets a mutable reference to the page in which this address occurs.
    /// Raises a page fault if it is unmapped.
    ///
    /// Note: This should page fault instead of segfaulting on an access to the null page, as the
    /// OS should make a call to map_page to handle the page fault. Only then should the segfault
    /// occur.
    fn get_mut_page(&mut self, addr: T) -> Result<&mut MemPage, MemFault<T>> {
        let vpn = self.get_vpn(addr);
        if let Some(pte) = self.page_table.iter().find(|&e| e.vpn == vpn) {
            Ok(self.phys_mem.get_mut(&pte.ppn).unwrap())
        } else {
            Err(MemFault::pagefault_at_addr(addr))
        }
    }
}

impl<T: ByteAddress> Memory<T> for LinearPagedMemory<T> {
    /// Checks whether the provided virtual address is mapped.
    /// The 0 page is never considered mapped.
    fn is_mapped(&self, addr: T) -> bool {
        self.get_page(addr).is_ok()
    }

    /// Maps a page to memory if it is not already mapped, evicting other pages if necessary.
    /// Since we're following an LRU policy, this page is placed at the front of the vec, which
    /// we couldn't do if we were an actual OS but we're not so who cares.
    ///
    /// The 0 page cannot be mapped, and any attempt to do so will cause a segfault.
    fn map_page(&mut self, addr: T) -> Result<(), MemFault<T>> {
        let vpn = self.get_vpn(addr);
        if vpn == 0 {
            return Err(MemFault::<T>::segfault_at_addr(addr));
        }
        // get a new ppn
        if let Some(idx) = self.page_table.iter().position(|&e| e.vpn == vpn) {
            // check if page already mapped
            // remove and move to front of vec
            let e = self.page_table.remove(idx);
            self.page_table.insert(0, e);
        } else {
            // create new entry
            // check if eviction is needed
            if self.page_table.len() == self.page_count {
                let evicted = self.page_table.pop().unwrap();
                self.paged_out
                    .insert(evicted.vpn, self.phys_mem.remove(&evicted.ppn).unwrap());
            }
            let ppn = self.lowest_free_ppn().unwrap();
            // check if entry was previously paged out
            let page = self.paged_out.remove(&vpn).unwrap_or_else(MemPage::new);
            let e = PTEntry { vpn, ppn };
            self.page_table.insert(0, e);
            self.phys_mem.insert(ppn, page);
        }
        Ok(())
    }

    fn set_byte(&mut self, addr: T, value: DataByte) -> Result<(), MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Byte)?;
        let offs = self.get_page_offs(addr);
        self.get_mut_page(addr)?.set_byte(offs, value);
        Ok(())
    }

    fn get_byte(&self, addr: T) -> Result<DataByte, MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Byte)?;
        let offs = self.get_page_offs(addr);
        Ok(self.get_page(addr)?.get_byte(offs))
    }

    fn set_half(&mut self, addr: T, value: DataHalf) -> Result<(), MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Half)?;
        let offs = self.get_page_offs(addr);
        self.get_mut_page(addr)?.set_half(offs, value);
        Ok(())
    }

    fn get_half(&self, addr: T) -> Result<DataHalf, MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Half)?;
        let offs = self.get_page_offs(addr);
        Ok(self.get_page(addr)?.get_half(offs))
    }

    fn set_word(&mut self, addr: T, value: DataWord) -> Result<(), MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Word)?;
        let offs = self.get_page_offs(addr);
        self.get_mut_page(addr)?.set_word(offs, value);
        Ok(())
    }

    fn get_word(&self, addr: T) -> Result<DataWord, MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Word)?;
        let offs = self.get_page_offs(addr);
        Ok(self.get_page(addr)?.get_word(offs))
    }

    fn set_doubleword(&mut self, addr: T, value: DataDword) -> Result<(), MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::DoubleWord)?;
        let offs = self.get_page_offs(addr);
        self.get_mut_page(addr)?.set_doubleword(offs, value);
        Ok(())
    }

    fn get_doubleword(&self, addr: T) -> Result<DataDword, MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::DoubleWord)?;
        let offs = self.get_page_offs(addr);
        Ok(self.get_page(addr)?.get_doubleword(offs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Ensures that unaligned loads/stores on a MemPage succeed.
    #[test]
    fn test_unaligned_mempage_rw() {
        let mut mem = MemPage::new();
        // This address is not dword-aligned
        mem.set_doubleword(0xFFFF_FFF0, 0xFFFF_FFFF_FFFF_FFFFu64.into());
        mem.set_doubleword(0xFFFF_FFF8, 0xEEEE_EEEE_EEEE_EEEEu64.into());
        // This should overwrite parts of both previous words
        mem.set_doubleword(0xFFFF_FFF1, 0xABCD_ABCD_ABCD_ABCDu64.into());
        assert_eq!(
            mem.get_doubleword(0xFFFF_FFF1),
            0xABCD_ABCD_ABCD_ABCDu64.into()
        );
        // Assuming little endian, the upper bytes of the lower address were set
        assert_eq!(
            mem.get_doubleword(0xFFFF_FFF0),
            0xCDAB_CDAB_CDAB_CD_FFu64.into()
        );
        // Assuming little endian, the lower bytes of the upper address were set
        assert_eq!(
            mem.get_doubleword(0xFFFF_FFF8),
            0xEEEE_EEEE_EEEE_EEABu64.into()
        );
    }

    /// Tests page faults and basic memory mapping operations.
    #[test]
    fn test_linear_pt() {
        // 4 KiB page size, 1 MiB physical memory
        let mut mem = LinearPagedMemory::<ByteAddr32>::new(20, 12);
        let good_addr: ByteAddr32 = 0xFFFF_EF00u32.into();
        let val: DataDword = 0xDEAD_BEEF_CAFE_0000u64.into();
        // Should pagefault when it's unmapped
        assert_eq!(
            mem.set_doubleword(good_addr, val).unwrap_err(),
            MemFault::pagefault_at_addr(good_addr)
        );
        // Trying to map 0xFFFF_EF00 will give the page starting from 0xFFFF_E000 (chop off the
        // lower 12 bits)
        assert!(mem.map_page(good_addr).is_ok());
        // Memory set should now succeed
        mem.set_doubleword(good_addr, val).unwrap();
        assert_eq!(mem.get_doubleword(good_addr).unwrap(), val);
        // Memory set at page start should succeed as well
        mem.set_byte(0xFFFF_E000u32.into(), 0xABu8.into()).unwrap();
        // The page should be zerod when paged in, so only the lowest byte we just set is returned
        assert_eq!(mem.get_word(0xFFFF_E000u32.into()).unwrap(), 0xAB.into());
        // Accessing an unaligned address should pagefault
        assert_eq!(
            mem.get_word(0xFFFF_E001u32.into()).unwrap_err(),
            MemFault::buserror_at_addr(0xFFFF_E001u32.into())
        );
        // Attempting to access a lower address should still incur a page fault
        assert_eq!(
            mem.get_byte(0xFFFF_DFFFu32.into()).unwrap_err(),
            MemFault::pagefault_at_addr(0xFFFF_DFFFu32.into())
        );
        // Attempting to access the next page should also incur a page fault
        assert_eq!(
            mem.get_word(0xFFFF_F000u32.into()).unwrap_err(),
            MemFault::pagefault_at_addr(0xFFFF_F000u32.into())
        );
        // Finally, attempting to deref a null pointer is always a pagefault and attempting to map
        // the zero page is a segfault
        assert_eq!(
            mem.get_word(0u32.into()).unwrap_err(),
            MemFault::pagefault_at_addr(0u32.into())
        );
        assert_eq!(
            mem.map_page(0u32.into()).unwrap_err(),
            MemFault::segfault_at_addr(0u32.into())
        );
    }
}
