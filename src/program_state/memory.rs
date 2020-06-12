use super::datatypes::*;
use crate::arch::*;
use std::collections::HashMap;
use std::marker::PhantomData;

#[derive(Debug)]
pub struct PageFault<T: ByteAddress> {
    user_vaddr: T,
}

impl<T: ByteAddress> PageFault<T> {
    /// Indicates a pagefault at the provided address.
    pub fn at_addr(user_vaddr: T) -> Self {
        PageFault { user_vaddr }
    }
}

/// Trait to define a virtual memory abstraction.
///
/// All direct memory operations are byte-addressed; behavior for unaligned accesses is left up to
/// the implementor.
///
/// Read/write operations should not directly map pages; they should instead produce a pagefault,
/// and the OS should perform the mapping independently.
///
/// Read operations will return either the requested data or a PageFault.
/// Write operations will return a result with an empty value or a PageFault.
pub trait Memory<T: ByteAddress> {
    /// Checks whether the provided virtual address is mapped.
    fn is_mapped(&self, addr: T) -> bool;

    /// Faults if the page is unmapped.
    fn fault_if_unmapped(&self, addr: T) -> Result<(), PageFault<T>> {
        if self.is_mapped(addr) {
            Ok(())
        } else {
            Err(PageFault::at_addr(addr))
        }
    }

    /// Maps a page to physical memory if it is not already mapped. Returns a reference to the page.
    fn map_page(&mut self, vpn: VirtPN);

    /// Stores an 8-bit byte to memory.
    fn set_byte(&mut self, addr: T, value: DataByte) -> Result<(), PageFault<T>>;
    /// Reads an 8-bit byte from memory.
    fn get_byte(&self, addr: T) -> Result<DataByte, PageFault<T>>;
    /// Stores a 16-bit halfword to memory.
    fn set_half(&mut self, addr: T, value: DataHalf) -> Result<(), PageFault<T>>;
    /// Reads a 16-bit halfword from memory.
    fn get_half(&self, addr: T) -> Result<DataHalf, PageFault<T>>;
    /// Stores a 32-bit word to memory.
    fn set_word(&mut self, addr: T, value: DataWord) -> Result<(), PageFault<T>>;
    /// Reads a 32-bit word from memory.
    fn get_word(&self, addr: T) -> Result<DataWord, PageFault<T>>;
    /// Stores a double word to memory.
    fn set_doubleword(&mut self, addr: T, value: DataDword) -> Result<(), PageFault<T>>;
    /// Reads a double word from memory.
    fn get_doubleword(&self, addr: T) -> Result<DataDword, PageFault<T>>;

    fn set(&mut self, addr: T, value: DataEnum) -> Result<(), PageFault<T>> {
        match value {
            DataEnum::Byte(v) => self.set_byte(addr, v),
            DataEnum::Half(v) => self.set_half(addr, v),
            DataEnum::Word(v) => self.set_word(addr, v),
            DataEnum::DoubleWord(v) => self.set_doubleword(addr, v),
        }
    }

    fn get(&self, addr: T, w: DataWidth) -> Result<DataEnum, PageFault<T>> {
        Ok(match w {
            DataWidth::Byte => DataEnum::Byte(self.get_byte(addr)?),
            DataWidth::Half => DataEnum::Half(self.get_half(addr)?),
            DataWidth::Word => DataEnum::Word(self.get_word(addr)?),
            DataWidth::DoubleWord => DataEnum::DoubleWord(self.get_doubleword(addr)?),
        })
    }
}

/// A simple memory backed by a word-addressed hashmap.
/// Never pagefaults. Reads to uninitialized addresses always return 0.
pub struct SimpleMemory<T: ByteAddress> {
    store: HashMap<<T as ByteAddress>::WordAddress, DataWord>,
}

impl<T: ByteAddress> SimpleMemory<T> {
    pub fn new() -> SimpleMemory<T> {
        SimpleMemory {
            store: HashMap::new(),
        }
    }
}

impl<T: ByteAddress> Memory<T> for SimpleMemory<T> {
    fn is_mapped(&self, addr: T) -> bool {
        addr.bits() != 0
    }

    fn map_page(&mut self, _vpn: VirtPN) {}

    fn set_byte(&mut self, addr: T, value: DataByte) -> Result<(), PageFault<T>> {
        let offs = addr.get_word_offset();
        let word_val = self
            .store
            .get(&addr.to_word_address())
            .map(Clone::clone)
            .unwrap_or(DataWord::zero());
        self.set_word(addr, word_val.set_byte(offs, value))
    }

    fn get_byte(&self, addr: T) -> Result<DataByte, PageFault<T>> {
        let offs = addr.get_word_offset();
        Ok(self.get_word(addr)?.get_byte(offs))
    }

    fn set_half(&mut self, addr: T, value: DataHalf) -> Result<(), PageFault<T>> {
        let n: u16 = value.into();
        self.set_byte(addr, (n as u8).into())?;
        self.set_byte(addr.plus_1(), ((n >> 8) as u8).into())?;
        Ok(())
    }

    fn get_half(&self, addr: T) -> Result<DataHalf, PageFault<T>> {
        let lower: u8 = self.get_byte(addr)?.into();
        let upper: u8 = self.get_byte(addr.plus_1())?.into();
        Ok(DataHalf::from(
            ((upper as u16) << 8) as u16 | (lower as u16),
        ))
    }

    fn set_word(&mut self, addr: T, value: DataWord) -> Result<(), PageFault<T>> {
        self.store.insert(addr.to_word_address(), value);
        Ok(())
    }

    fn get_word(&self, addr: T) -> Result<DataWord, PageFault<T>> {
        Ok(self
            .store
            .get(&addr.to_word_address())
            .map(Clone::clone)
            .unwrap_or(DataWord::zero()))
    }

    fn set_doubleword(&mut self, addr: T, value: DataDword) -> Result<(), PageFault<T>> {
        let lower_word_addr = addr.to_word_address();
        let upper_word_addr = addr.plus_4().to_word_address();
        let upper_value = value.get_upper_word();
        let lower_value = value.get_lower_word();
        self.store.insert(upper_word_addr, upper_value);
        self.store.insert(lower_word_addr, lower_value);
        Ok(())
    }

    fn get_doubleword(&self, addr: T) -> Result<DataDword, PageFault<T>> {
        let lower_word_addr = addr.to_word_address();
        let upper_word_addr = addr.plus_4().to_word_address();
        let upper_value = self
            .store
            .get(&upper_word_addr)
            .map(Clone::clone)
            .unwrap_or(DataWord::from(0));
        let lower_value = self
            .store
            .get(&lower_word_addr)
            .map(Clone::clone)
            .unwrap_or(DataWord::from(0));
        Ok(DataDword::from_words(lower_value, upper_value))
    }
}

type VirtPN = usize;
type PhysPN = usize;
type PageOffset = usize;

/// Maps a virtual page to a physical one.
#[derive(Copy, Clone)]
struct PTEntry {
    /// The virtual page number.
    pub vpn: VirtPN,
    /// The physical page number.
    pub ppn: PhysPN,
}

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
            let default_value = DataByte::from(0u8);
            default_value
        }
    }
}

/// A memory backed by a fully associative LRU linear page table.
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
    /// Returns the page, or None on failure.
    fn map_page(&mut self, vpn: VirtPN) {
        // get a new ppn
        let ppn = self.lowest_free_ppn().unwrap();
        let page_table = &mut self.page_table;
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
        }
    }

    fn set_byte(&mut self, addr: T, value: DataByte) -> Result<(), PageFault<T>> {
        self.fault_if_unmapped(addr)?;
        let (vpn, offs) = self.split_addr(addr);
        self.get_mut_page(vpn).unwrap().set_byte(offs, value);
        Ok(())
    }

    fn get_byte(&self, addr: T) -> Result<DataByte, PageFault<T>> {
        self.fault_if_unmapped(addr)?;
        let (vpn, offs) = self.split_addr(addr);
        Ok(self.get_page(vpn).unwrap().get_byte(offs))
    }

    fn set_half(&mut self, addr: T, value: DataHalf) -> Result<(), PageFault<T>> {
        // set lsb, then msb
        self.set_byte(addr, (u16::from(value) as u8).into())?;
        self.set_byte(addr.plus_1(), ((u16::from(value) >> 8) as u8).into())
    }

    fn get_half(&self, addr: T) -> Result<DataHalf, PageFault<T>> {
        let lsb: u8 = self.get_byte(addr)?.into();
        let msb: u8 = self.get_byte(addr.plus_1())?.into();
        let full = ((msb as u16) << 8) | (lsb as u16);
        Ok(DataHalf::from(full))
    }

    fn set_word(&mut self, addr: T, value: DataWord) -> Result<(), PageFault<T>> {
        self.set_half(addr, (u32::from(value) as u16).into())?;
        self.set_half(
            addr.plus_1().plus_1(),
            ((u32::from(value) >> 16) as u16).into(),
        )
    }

    fn get_word(&self, addr: T) -> Result<DataWord, PageFault<T>> {
        let lsb: u16 = self.get_half(addr)?.into();
        let msb: u16 = self.get_half(addr.plus_1().plus_1())?.into();
        let full = ((msb as u32) << 16) | (lsb as u32);
        Ok(DataWord::from(full))
    }

    fn set_doubleword(&mut self, addr: T, value: DataDword) -> Result<(), PageFault<T>> {
        self.set_word(addr, (u64::from(value) as u32).into())?;
        self.set_word(addr.plus_4(), ((u64::from(value) >> 32) as u32).into())
    }

    fn get_doubleword(&self, addr: T) -> Result<DataDword, PageFault<T>> {
        let lsb: u32 = self.get_word(addr)?.into();
        let msb: u32 = self.get_word(addr.plus_4())?.into();
        let full = ((msb as u64) << 32) | (lsb as u64);
        Ok(DataDword::from(full))
    }
}
