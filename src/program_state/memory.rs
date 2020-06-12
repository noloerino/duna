use super::datatypes::*;
use std::collections::HashMap;
use std::marker::PhantomData;

type VirtPN = usize;
type PhysPN = usize;
type PageOffset = usize;

/// Represents a page of memory.
/// TODO implement default value
pub struct MemPage {
    /// To ensure the simulator doesn't waste space allocating the full size of the page,
    /// this backing store allocates bytes lazily.
    /// TODO do something more efficient like a u64
    backing: HashMap<PageOffset, DataByte>,
}

impl MemPage {
    fn new() -> MemPage {
        MemPage {
            backing: HashMap::new(),
        }
    }

    fn set_byte(&mut self, offs: PageOffset, value: DataByte) {
        self.backing.insert(offs, value);
    }

    fn get_byte(&self, offs: PageOffset) -> DataByte {
        if let Some(v) = self.backing.get(&offs) {
            *v
        } else {
            DataByte::from(0u8)
        }
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
        let dword: u64 = value.into();
        self.set_word(offs, (dword as u32).into());
        self.set_word(offs + 4, ((dword >> 32) as u32).into());
    }

    fn get_doubleword(&self, offs: PageOffset) -> DataDword {
        let lower: u32 = self.get_word(offs).into();
        let upper: u32 = self.get_word(offs + 4).into();
        DataDword::from(((upper as u64) << 32) | (lower as u64))
    }
}

#[derive(Debug)]
pub struct MemFault<T: ByteAddress> {
    user_vaddr: T,
    cause: MemFaultCause,
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

#[derive(Debug)]
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
    /// If the page cannot be mapped, then a TermCause is returned.
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
            page_count: page_count,
            page_table: Vec::with_capacity(page_count),
            phys_mem: HashMap::new(),
            paged_out: HashMap::new(),
            _phantom: PhantomData,
        }
    }

    fn split_addr(&self, addr: T) -> (VirtPN, PageOffset) {
        // the page offset comes from the lower page_offs_len bits
        // the vpn is the rest
        let bits = addr.bits();
        let lsb_mask = !((-1i64 as u64) << self.page_offs_len);
        (
            (bits >> self.page_offs_len) as usize,
            (bits & lsb_mask) as usize,
        )
    }

    fn lowest_free_ppn(&self) -> Option<PhysPN> {
        for i in 0..self.page_count {
            if !self.page_table.iter().any(|&e| e.ppn == i) {
                return Some(i);
            }
        }
        None
    }

    fn get_page(&self, vpn: VirtPN) -> Option<&MemPage> {
        if let Some(idx) = self.page_table.iter().position(|&e| e.vpn == vpn) {
            let e = self.page_table.get(idx).unwrap();
            self.phys_mem.get(&e.ppn)
        } else {
            None
        }
    }

    fn get_mut_page(&mut self, vpn: VirtPN) -> Option<&mut MemPage> {
        if let Some(idx) = self.page_table.iter().position(|&e| e.vpn == vpn) {
            let e = self.page_table.get(idx).unwrap();
            self.phys_mem.get_mut(&e.ppn)
        } else {
            None
        }
    }
}

impl<T: ByteAddress> Memory<T> for LinearPagedMemory<T> {
    /// Checks whether the provided virtual address is mapped.
    /// The 0 page is never considered mapped.
    fn is_mapped(&self, addr: T) -> bool {
        let (vpn, _) = self.split_addr(addr);
        vpn != 0 && self.page_table.iter().any(|&e| e.vpn == vpn)
    }

    /// Maps a page to memory if it is not already mapped, evicting other pages if necessary.
    /// Since we're following an LRU policy, this page is placed at the front of the vec, which
    /// we couldn't do if we were an actual OS but we're not so who cares.
    ///
    /// The 0 page cannot be mapped, and any attempt to do so will cause a segfault.
    fn map_page(&mut self, addr: T) -> Result<(), MemFault<T>> {
        let (vpn, _) = self.split_addr(addr);
        if vpn == 0 {
            return Err(MemFault::<T>::segfault_at_addr(addr));
        }
        // get a new ppn
        let ppn = self.lowest_free_ppn().unwrap();
        let page_table = &mut self.page_table;
        Ok(
            if let Some(idx) = page_table.iter().position(|&e| e.vpn == vpn) {
                // check if page already mapped
                // remove and move to front of vec
                let e = page_table.remove(idx);
                page_table.insert(0, e);
            } else {
                // create new entry
                // check if eviction is needed
                if page_table.len() == self.page_count {
                    let evicted = page_table.pop().unwrap();
                    self.paged_out
                        .insert(evicted.vpn, self.phys_mem.remove(&evicted.ppn).unwrap());
                }
                // check if entry was previously paged out
                let page = self.paged_out.remove(&vpn).unwrap_or(MemPage::new());
                let e = PTEntry { vpn, ppn };
                page_table.insert(0, e);
                self.phys_mem.insert(ppn, page);
            },
        )
    }

    fn set_byte(&mut self, addr: T, value: DataByte) -> Result<(), MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Byte)?;
        self.fault_if_unmapped(addr)?;
        let (vpn, offs) = self.split_addr(addr);
        self.get_mut_page(vpn).unwrap().set_byte(offs, value);
        Ok(())
    }

    fn get_byte(&self, addr: T) -> Result<DataByte, MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Byte)?;
        self.fault_if_unmapped(addr)?;
        let (vpn, offs) = self.split_addr(addr);
        Ok(self.get_page(vpn).unwrap().get_byte(offs))
    }

    fn set_half(&mut self, addr: T, value: DataHalf) -> Result<(), MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Half)?;
        self.fault_if_unmapped(addr)?;
        let (vpn, offs) = self.split_addr(addr);
        self.get_mut_page(vpn).unwrap().set_half(offs, value);
        Ok(())
    }

    fn get_half(&self, addr: T) -> Result<DataHalf, MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Half)?;
        self.fault_if_unmapped(addr)?;
        let (vpn, offs) = self.split_addr(addr);
        Ok(self.get_page(vpn).unwrap().get_half(offs))
    }

    fn set_word(&mut self, addr: T, value: DataWord) -> Result<(), MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Word)?;
        self.fault_if_unmapped(addr)?;
        let (vpn, offs) = self.split_addr(addr);
        Ok(self.get_mut_page(vpn).unwrap().set_word(offs, value))
    }

    fn get_word(&self, addr: T) -> Result<DataWord, MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::Word)?;
        self.fault_if_unmapped(addr)?;
        let (vpn, offs) = self.split_addr(addr);
        Ok(self.get_page(vpn).unwrap().get_word(offs))
    }

    fn set_doubleword(&mut self, addr: T, value: DataDword) -> Result<(), MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::DoubleWord)?;
        self.fault_if_unmapped(addr)?;
        let (vpn, offs) = self.split_addr(addr);
        Ok(self.get_mut_page(vpn).unwrap().set_doubleword(offs, value))
    }

    fn get_doubleword(&self, addr: T) -> Result<DataDword, MemFault<T>> {
        MemFault::<T>::check_aligned(addr, DataWidth::DoubleWord)?;
        self.fault_if_unmapped(addr)?;
        let (vpn, offs) = self.split_addr(addr);
        Ok(self.get_page(vpn).unwrap().get_doubleword(offs))
    }
}
