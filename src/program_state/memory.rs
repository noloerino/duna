use super::datatypes::*;
use super::phys::*;
use crate::arch::*;
use std::collections::HashMap;
use std::marker::PhantomData;

type VirtPn = usize;

/// Maps a virtual page to a physical one.
#[derive(Copy, Clone)]
struct PtEntry {
    pub valid: bool,
    /// The virtual page number.
    pub vpn: VirtPn,
    /// The physical page number.
    pub ppn: PhysPn,
}

/// Represents an operation that updates the page table.
pub struct PteUpdate {
    /// The location of the entry in the page table.
    pte_loc: usize,
    /// The swap location that the evicted page is placed at. None if this operation isn't
    /// an eviction.
    swap_loc: Option<usize>,
    old: PtEntry,
    new: PtEntry,
}

/// Represents the result of a page table lookup.
/// The diffs vec represents the sequence of page table updates that occurred.
/// The returned PPN and offset are the result of the lookup.
pub struct PteLookupData {
    diffs: Vec<PteUpdate>,
    ppn: PhysPn,
    offs: PageOffs,
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

/// Trait to define a page table abstraction.
///
/// All operations will return a sequence of diffs on success, and a pagefault on failure.
pub trait PageTable<T: ByteAddress> {
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

    fn apply_update(&mut self, update: &PteUpdate);

    fn revert_update(&mut self, update: &PteUpdate);

    /// Maps a page containing the provided address to physical memory if it is not already mapped.
    /// If the page cannot be mapped, then a MemFault is returned.
    /// If the page was newly mapped successfully, then a sequence of updates will be returned.
    /// If the page was already mapped, then the sequence will be empty.
    fn map_page(&self, addr: T) -> Result<Vec<PteUpdate>, MemFault<T>>;

    /// Looks up the page at the associated address.
    fn lookup_page(&self, addr: T) -> Result<PteLookupData, MemFault<T>>;
}

/// A simple memory of a single page; all addresses except the null address are considered to be
/// paged in. The physical and virtual address spaces are the same size.
///
/// Reads to uninitialized addresses always return 0.
/// Faults occur on unaligned accesses or accesses to the null pointer.
pub struct AllMappedPt<T: ByteAddress>;

impl<T: ByteAddress> AllMappedPt<T> {
    pub fn new() -> Self {
        AllMappedPt {}
    }
}

impl<T: ByteAddress> PageTable<T> for AllMappedPt<T> {
    fn is_mapped(&self, addr: T) -> bool {
        addr.bits() != 0
    }

    fn apply_update(&mut self, update: &PteUpdate) {}

    fn revert_update(&mut self, update: &PteUpdate) {}

    fn map_page(&mut self, addr: T) -> Result<Vec<PteUpdate>, MemFault<T>> {
        if self.is_mapped(addr) {
            Err(MemFault::<T>::segfault_at_addr(addr))
        } else {
            Ok(vec![])
        }
    }

    fn lookup_page(&self, addr: T) -> Result<PteLookupData, MemFault<T>> {
        if self.is_mapped(addr) {
            Ok(PteLookupData {
                diffs: vec![],
                ppn: 0,
                offs: addr.bits(),
            })
        } else {
            Err(MemFault::<T>::segfault_at_addr(addr))
        }
    }
}

impl<T: ByteAddress> Default for AllMappedPt<T> {
    fn default() -> Self {
        Self::new()
    }
}

/*
/// A memory with a fully associative LRU linear page table.
/// The zero page is never considered mapped, and all accesses must be aligned.
pub struct LinearPagedMemory<T: ByteAddress> {
    page_offs_len: usize,
    page_count: usize,
    page_table: Vec<PtEntry>,
    phys_mem: HashMap<PhysPn, MemPage>,
    paged_out: HashMap<VirtPn, MemPage>,
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

    fn get_vpn(&self, addr: T) -> VirtPn {
        let bits = addr.bits();
        (bits >> self.page_offs_len) as usize
    }

    fn get_page_offs(&self, addr: T) -> PageOffs {
        // the page offset comes from the lower page_offs_len bits
        let bits = addr.bits();
        let lsb_mask = !((-1i64 as u64) << self.page_offs_len);
        (bits & lsb_mask) as usize
    }

    fn lowest_free_ppn(&self) -> Option<PhysPn> {
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

impl<T: ByteAddress> PageTable<T> for LinearPagedMemory<T> {
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
            let e = PtEntry { vpn, ppn };
            self.page_table.insert(0, e);
            self.phys_mem.insert(ppn, page);
        }
        Ok(())
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
*/
