use super::bitmap::Bitmap;
use super::datatypes::*;
use super::phys::*;
use super::priv_s::PrivDiff;
use super::program::StateDiff;
use crate::arch::*;
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;

type VirtPn = usize;

/// Maps a virtual page to a physical one.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct PtEntry {
    pub valid: bool,
    /// The physical page number.
    pub ppn: PhysPn,
}

impl Default for PtEntry {
    fn default() -> PtEntry {
        PtEntry {
            valid: false,
            ppn: 0,
        }
    }
}

/// Represents an operation that updates the page table.
#[derive(Debug)]
pub enum PtUpdate {
    /// An update to an entry in the page table.
    Entry {
        vpn: VirtPn,
        old: PtEntry,
        new: PtEntry,
    },
    /// An update to replacement policy metadata.
    Replacement(ReplacementUpdate),
    /// A page was evicted and placed in the swapfile.
    SwapAdd { vpn: VirtPn, pte: PtEntry },
    /// A page was removed from the swapfile.
    SwapRemove { vpn: VirtPn, pte: PtEntry },
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

impl fmt::Debug for PtLookupData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "PtLookupData {{ diffs: <{} elements>, ppn: {}, offs: {} }}",
            self.diffs.len(),
            self.ppn,
            self.offs
        )
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

/// Trait to define a page table abstraction.
///
/// All operations will return a sequence of diffs on success, and a pagefault on failure.
pub trait PageTable<T: ByteAddress> {
    fn apply_update(&mut self, mem: &mut PhysMem, update: &PtUpdate);

    fn revert_update(&mut self, mem: &mut PhysMem, update: &PtUpdate);

    /// Maps a page and updates the state of the page table. The resulting updates are not saved
    /// anywhere, as they are instantly applied.
    ///
    /// This method should be invoked in instances like the loading of the program, where we wish
    /// to initialize pages for the stack and static segments, and don't care about the intermediate
    /// steps taken.
    fn force_map_page(&mut self, mem: &mut PhysMem, vaddr: T) -> Result<(), MemFault<T>> {
        for update in self.map_page(vaddr)? {
            self.apply_update(mem, &update);
        }
        Ok(())
    }

    /// Maps a page containing the provided address to physical memory if it is not already mapped.
    /// If the page cannot be mapped, then a MemFault is returned.
    /// If the page was newly mapped successfully, then a sequence of updates will be returned.
    ///
    /// If the page was already mapped, then this function will panic. Any updates to state (clock
    /// bits, SCL, etc.) must be performed through lookup_page.
    fn map_page(&self, vaddr: T) -> Result<Vec<PtUpdate>, MemFault<T>>;

    /// Unmaps the page containing the provided address.
    /// An implementor may choose to make this a noop, as in the case where all pags are considered
    /// mapped for simplicity.
    fn unmap_page(&self, vaddr: T) -> Vec<PtUpdate>;

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
    fn apply_update(&mut self, _mem: &mut PhysMem, _update: &PtUpdate) {
        panic!("Attempted to apply an update, but AllMappedPt should not produce any updates");
    }

    fn revert_update(&mut self, _mem: &mut PhysMem, _update: &PtUpdate) {
        panic!("Attempted to revert an update, but AllMappedPt should not produce any updates");
    }

    fn map_page(&self, vaddr: T) -> Result<Vec<PtUpdate>, MemFault<T>> {
        if vaddr.bits() == 0 {
            Err(MemFault::<T>::segfault_at_addr(vaddr))
        } else {
            Ok(vec![])
        }
    }

    fn unmap_page(&self, _vaddr: T) -> Vec<PtUpdate> {
        vec![]
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
    page_offs_len: usize,
    page_table: HashMap<VirtPn, PtEntry>,
    freemap: Bitmap,
    // TODO move swap file to physical state?
    swapfile: HashMap<VirtPn, (PtEntry, MemPage)>,
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
        let phys_pg_count = 1 << phys_pn_bits;
        FifoLinearPt {
            page_offs_len: pg_ofs_bits,
            page_table: HashMap::new(),
            freemap: Bitmap::new(phys_pg_count),
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
    fn apply_update(&mut self, mem: &mut PhysMem, update: &PtUpdate) {
        use PtUpdate::*;
        match update {
            &Entry { vpn, new, .. } => {
                self.page_table.insert(vpn, new);
            }
            Replacement(update) => match update {
                ReplacementUpdate::ClockTick => {
                    self.fifo_ctr += 1;
                }
            },
            &SwapAdd { vpn, pte } => {
                self.swapfile.insert(vpn, (pte, mem[&pte.ppn].clone()));
            }
            SwapRemove { vpn, pte } => {
                // The PTE should contain the physical page that we're mapping to
                mem.insert(pte.ppn, self.swapfile.remove(vpn).unwrap().1);
            }
            &BitmapFlip(n) => {
                self.freemap.flip(n);
            }
        }
    }

    fn revert_update(&mut self, _mem: &mut PhysMem, _update: &PtUpdate) {}

    fn map_page(&self, addr: T) -> Result<Vec<PtUpdate>, MemFault<T>> {
        // This isn't really FIFO if pages aren't mapped in vaddr order...
        let vpn = self.get_vpn(addr);
        if vpn == 0 {
            return Err(MemFault::<T>::segfault_at_addr(addr));
        }
        let old_pte = if let Some(pte) = self.page_table.get(&vpn) {
            if pte.valid {
                // If page already mapped, panic
                panic!(
                    "Attempted to map already-mapped page at VPN {:?}, with existing PTE {:?}",
                    vpn, pte
                );
            }
            *pte
        } else {
            PtEntry::default()
        };
        let mut diffs = Vec::<PtUpdate>::new();
        let ppn = if let Some(ppn) = self.freemap.get_lowest_zero() {
            // Mark page as used
            diffs.push(PtUpdate::BitmapFlip(ppn));
            ppn
        } else {
            let old_pte = self.page_table.get(&self.fifo_ctr).unwrap();
            // Send old page to swap and claim its ppn instead
            diffs.push(PtUpdate::SwapAdd {
                vpn: self.fifo_ctr,
                pte: *old_pte,
            });
            // Advance FIFO counter
            diffs.push(PtUpdate::Replacement(ReplacementUpdate::ClockTick));
            // TODO handle case where middle page gets unmapped
            // or VAS is smaller than phys AS
            old_pte.ppn
        };
        // Check for page in swapfile
        if self.swapfile.contains_key(&vpn) {
            diffs.push(PtUpdate::SwapRemove {
                vpn,
                pte: PtEntry { valid: true, ppn },
            })
        }
        diffs.push(PtUpdate::Entry {
            vpn,
            old: old_pte,
            new: PtEntry { valid: true, ppn },
        });
        println!("Mapping entry for VPN {:X} to PPN {}", vpn, ppn);
        Ok(diffs)
    }

    fn unmap_page(&self, addr: T) -> Vec<PtUpdate> {
        // This screws up FIFO a little because if we unmap a page in the middle of the VAS
        // then that page should be evicted later...
        let mut diffs = Vec::new();
        let vpn = self.get_vpn(addr);
        if let Some(old_pte) = self.page_table.get(&vpn) {
            assert!(old_pte.valid);
            diffs.push(PtUpdate::Entry {
                vpn,
                old: *old_pte,
                new: PtEntry {
                    valid: false,
                    ..*old_pte
                },
            });
            diffs.push(PtUpdate::BitmapFlip(old_pte.ppn));
        }
        diffs
    }

    fn lookup_page(&self, vaddr: T) -> Result<PtLookupData, MemFault<T>> {
        let vpn = self.get_vpn(vaddr);
        if vpn != 0 {
            if let Some(pte) = self.page_table.get(&vpn) {
                if pte.valid {
                    let offs = self.get_page_offs(vaddr);
                    println!("Found entry for VPN {:0X} (PPN {})", vpn, pte.ppn);
                    return Ok(PtLookupData {
                        diffs: vec![],
                        ppn: pte.ppn,
                        offs,
                    });
                }
            }
        }
        Err(MemFault::<T>::pagefault_at_addr(vaddr))
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

    /// Tests page faults and basic page table lookups.
    #[test]
    fn test_linear_pt() {
        // 4 KiB page size, 1 MiB physical memory
        let mut pt = FifoLinearPt::<ByteAddr32>::new(8, 12);
        let mut dummy_mem = Default::default();
        let good_addr: ByteAddr32 = 0xFFFF_EF00u32.into();
        // Should pagefault when it's unmapped
        assert_eq!(
            pt.lookup_page(good_addr).unwrap_err(),
            MemFault::pagefault_at_addr(good_addr)
        );
        // Trying to map 0xFFFF_EF00 will give the page starting from 0xFFFF_E000 (chop off the
        // lower 12 bits)
        assert!(pt.force_map_page(&mut dummy_mem, good_addr).is_ok());
        // Lookup should now succeed
        assert!(pt.lookup_page(good_addr).is_ok());
        // Lookup at page start should succeed as well
        assert!(pt.lookup_page(0xFFFF_E000u32.into()).is_ok());
        // Attempting to access a lower address should still incur a page fault
        assert_eq!(
            pt.lookup_page(0xFFFF_DFFFu32.into()).unwrap_err(),
            MemFault::pagefault_at_addr(0xFFFF_DFFFu32.into())
        );
        // Attempting to access the next page should also incur a page fault
        assert_eq!(
            pt.lookup_page(0xFFFF_F000u32.into()).unwrap_err(),
            MemFault::pagefault_at_addr(0xFFFF_F000u32.into())
        );
        // Finally, attempting to deref a null pointer is always a pagefault and attempting to map
        // the zero page is a segfault
        assert_eq!(
            pt.force_map_page(&mut dummy_mem, 0u32.into()).unwrap_err(),
            MemFault::segfault_at_addr(0u32.into())
        );
        assert_eq!(
            pt.lookup_page(0u32.into()).unwrap_err(),
            MemFault::pagefault_at_addr(0u32.into())
        );
    }
}
