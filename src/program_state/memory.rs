use super::datatypes::*;
use super::phys::*;
use super::priv_s::PrivDiff;
use super::program::StateDiff;
use crate::arch::*;
use std::collections::HashMap;
use std::marker::PhantomData;

type VirtPn = usize;

/// Maps a virtual page to a physical one.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct PtEntry {
    pub valid: bool,
    /// The virtual page number.
    pub vpn: VirtPn,
    /// The physical page number.
    pub ppn: PhysPn,
}

impl Default for PtEntry {
    fn default() -> PtEntry {
        PtEntry {
            valid: false,
            vpn: 0,
            ppn: 0,
        }
    }
}

/// Represents an operation that updates the page table.
pub enum PtUpdate {
    /// An update to an entry in the page table.
    Entry {
        /// The location of the entry in the page table.
        pte_loc: usize,
        /// The swap location that the evicted page is placed at. None if this operation isn't
        /// an eviction.
        swap_loc: Option<usize>,
        old: PtEntry,
        new: PtEntry,
    },
    /// An update to replacement policy metadata.
    Replacement(ReplacementUpdate),
    /// A page was evicted and placed in the swapfile.
    SwapAdd { vpn: VirtPn, data: MemPage },
    /// An entry in the free page bitmap was flipped.
    BitmapFlip(usize),
}

impl PtUpdate {
    pub fn into_state_diff<F: ArchFamily<T>, T: MachineDataWidth>(self) -> StateDiff<F, T> {
        StateDiff::Priv(PrivDiff::PtUpdate(self))
    }
}

#[derive(Debug, PartialEq)]
pub enum ReplacementUpdate {
    /// Represents an increment to a clock hand, which can be used to represent either clock or
    /// FIFO replacement policies. In the case of a clock-based algorithm, this should be used in
    /// conjunction with a PtUpdate to modify the entry's "used" bit.
    ClockTick,
}

/// Represents the result of a page table lookup.
/// The diffs vec represents the sequence of page table updates that occurred.
/// The returned PPN and offset are the result of the lookup.
pub struct PtLookupData {
    pub diffs: Vec<PtUpdate>,
    pub ppn: PhysPn,
    pub offs: PageOffs,
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
    fn apply_update(&mut self, update: &PtUpdate);

    fn revert_update(&mut self, update: &PtUpdate);

    /// Maps a page and updates the state of the page table. The resulting updates are not saved
    /// anywhere, as they are instantly applied.
    ///
    /// This method should be invoked in instances like the loading of the program, where we wish
    /// to initialize pages for the stack and static segments, and don't care about the intermediate
    /// steps taken.
    fn force_map_page(&mut self, vaddr: T) -> Result<(), MemFault<T>> {
        self.map_page(vaddr)?
            .iter()
            .map(|update| self.apply_update(update));
        Ok(())
    }

    /// Maps a page containing the provided address to physical memory if it is not already mapped.
    /// If the page cannot be mapped, then a MemFault is returned.
    /// If the page was newly mapped successfully, then a sequence of updates will be returned.
    ///
    /// If the page was already mapped, then this function will panic. Any updates to state (clock
    /// bits, SCL, etc.) must be performed through lookup_page.
    fn map_page(&self, vaddr: T) -> Result<Vec<PtUpdate>, MemFault<T>>;

    /// Looks up the page at the associated address. Raises a page fault if not found.
    ///
    /// If the page was already mapped, then the sequence will be empty unless touching a page
    /// updates state, like with a second chance list.
    fn lookup_page(&self, vaddr: T) -> Result<PtLookupData, MemFault<T>>;
}

/// A simple memory of a single page; all addresses except the null address are considered to be
/// paged in. The physical and virtual address spaces are the same size.
///
/// Reads to uninitialized addresses always return 0.
/// Faults occur on accesses to the null pointer.
pub struct AllMappedPt<T: ByteAddress> {
    _phantom: PhantomData<T>,
}

impl<T: ByteAddress> AllMappedPt<T> {
    pub fn new() -> Self {
        AllMappedPt {
            _phantom: PhantomData,
        }
    }
}

impl<T: ByteAddress> PageTable<T> for AllMappedPt<T> {
    fn apply_update(&mut self, _update: &PtUpdate) {
        panic!("Attempted to apply an update, but AllMappedPt should not produce any updates");
    }

    fn revert_update(&mut self, _update: &PtUpdate) {
        panic!("Attempted to revert an update, but AllMappedPt should not produce any updates");
    }

    fn map_page(&self, vaddr: T) -> Result<Vec<PtUpdate>, MemFault<T>> {
        if vaddr.bits() == 0 {
            Err(MemFault::<T>::segfault_at_addr(vaddr))
        } else {
            Ok(vec![])
        }
    }

    fn lookup_page(&self, vaddr: T) -> Result<PtLookupData, MemFault<T>> {
        let bits = vaddr.bits();
        if bits != 0 {
            Ok(PtLookupData {
                diffs: vec![],
                ppn: 0,
                offs: bits as usize,
            })
        } else {
            Err(MemFault::<T>::segfault_at_addr(vaddr))
        }
    }
}

impl<T: ByteAddress> Default for AllMappedPt<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// A memory with a fully associative FIFO linear page table.
///
/// The page table is represented sparsely to save memory, as otherwise a page table for a 32-bit
/// address space with 4 KiB pages would require 2 ^ (32 - 12) entries.
///
/// The zero page is never considered mapped, and all accesses must be aligned.
pub struct FifoLinearPt<T: ByteAddress> {
    virt_pg_count: usize,
    phys_pg_count: usize,
    page_offs_len: usize,
    page_table: HashMap<VirtPn, PtEntry>,
    // TODO move swap file to physical state?
    swapfile: HashMap<VirtPn, MemPage>,
    // Checks the next index to evict.
    fifo_ctr: VirtPn,
    _phantom: PhantomData<T>,
}

impl<T: ByteAddress> FifoLinearPt<T> {
    /// Initializes the memory. The number of pages is computed from the physical address and
    /// page sizes.
    /// * phys_pn_bits: The number of bits needed to address a physical page. The number of pages of
    ///                 available physical memory is given by 2 to the power of this number.
    /// * pg_ofs_bits:  The number of bits needed to index a page. The number of bytes in a page is
    ///                 likewise 2 to the power of this number.
    pub fn new(phys_pn_bits: usize, pg_ofs_bits: usize) -> Self {
        FifoLinearPt {
            virt_pg_count: 1 << (<T as ByteAddress>::bitlen() - pg_ofs_bits),
            phys_pg_count: 1 << phys_pn_bits,
            page_offs_len: pg_ofs_bits,
            page_table: HashMap::new(),
            swapfile: HashMap::new(),
            fifo_ctr: 0,
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
}

impl<T: ByteAddress> PageTable<T> for FifoLinearPt<T> {
    fn apply_update(&mut self, update: &PtUpdate) {}

    fn revert_update(&mut self, update: &PtUpdate) {}

    fn map_page(&self, addr: T) -> Result<Vec<PtUpdate>, MemFault<T>> {
        /*
        let vpn = self.get_vpn(addr);
        if vpn == 0 {
            return Err(MemFault::<T>::segfault_at_addr(addr));
        }
        let old_pte = if let Some(pte) = self.page_table.get(&vpn) {
            if pte.valid && pte.vpn == vpn {
                // If page already mapped, panic
                panic!(
                    "Attempted to map already-mapped page at VPN {:?}, with existing PTE {:?}",
                    vpn, pte
                );
            }
            pte
        } else {
            PtEntry::default()
        };
        */
        let diffs = Vec::<PtUpdate>::new();
        /*
        Entry {
            /// The location of the entry in the page table.
            pte_loc: usize,
            /// The swap location that the evicted page is placed at. None if this operation isn't
            /// an eviction.
            swap_loc: Option<usize>,
            old: PtEntry,
            new: PtEntry,
        },
        /// An update to replacement policy metadata.
        Replacement(ReplacementUpdate),
        /// A page was evicted and placed in the swapfile.
        SwapAdd{vpn: VirtPn, data: MemPage}
        */
        /*
        let idx = self.fifo_ctr;
        // Create new entry and check if eviction is needed
        if self.page_table.len() == self.page_count {
            let evicted = self.page_table.pop().unwrap();
            self.paged_out
                .insert(evicted.vpn, self.phys_mem.remove(&evicted.ppn).unwrap());
        }
        let ppn = self.lowest_free_ppn().unwrap();
        // TODO check swapfile
        let page = self.swapfile.remove(&vpn).unwrap_or_else(MemPage::new);
        let e = PtEntry {
            valid: true,
            vpn,
            ppn,
        };
        self.page_table.insert(0, e);
        self.phys_mem.insert(ppn, page);*/
        Ok(vec![])
    }

    fn lookup_page(&self, vaddr: T) -> Result<PtLookupData, MemFault<T>> {
        let vpn = self.get_vpn(vaddr);
        if vpn != 0 {
            if let Some(pte) = self.page_table.get(&vpn) {
                if pte.valid {
                    assert!(vpn == pte.vpn);
                    let offs = self.get_page_offs(vaddr);
                    return Ok(PtLookupData {
                        diffs: vec![],
                        ppn: pte.ppn,
                        offs,
                    });
                }
            }
            Err(MemFault::<T>::pagefault_at_addr(vaddr))
        } else {
            Err(MemFault::<T>::segfault_at_addr(vaddr))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Ensures that the single-page memory has stuff "mapped" properly.
    #[test]
    fn test_all_mapped() {
        let pt = AllMappedPt::<ByteAddr32>::new();
        let npe: ByteAddr32 = DataWord::zero().into();
        assert!(pt.lookup_page(npe).is_err());
        let addr1: ByteAddr32 = DataWord::from(0xFFFF_F000u32).into();
        let lookup1 = pt.lookup_page(addr1).unwrap();
        assert!(lookup1.diffs.is_empty());
        assert_eq!(lookup1.ppn, 0);
        assert_eq!(lookup1.offs, 0xFFFF_F000);
        let addr2: ByteAddr32 = DataWord::from(0xC000_0000u32).into();
        let lookup2 = pt.lookup_page(addr2).unwrap();
        assert!(lookup2.diffs.is_empty());
        assert_eq!(lookup2.ppn, 0);
        assert_eq!(lookup2.offs, 0xC000_0000);
    }

    /// Ensures that unaligned loads/stores on a MemPage succeed.
    #[test]
    fn test_unaligned_mempage_rw() {
        let mut mem = MemPage::new(Endianness::Little, 32);
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

    /*
    /// Tests page faults and basic memory mapping operations.
    #[test]
    fn test_linear_pt() {
        // 4 KiB page size, 1 MiB physical memory
        let mut mem = FifoLinearPt::<ByteAddr32>::new(20, 12);
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
    */
}
