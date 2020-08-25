//! Represents the physical devices accessible to the machine, such as the disk and RAM.

use super::datatypes::*;
use super::program::StateDiff;
use crate::arch::*;
use std::collections::HashMap;

/// Indexes into physical memory. Constrained by the number of pages.
pub type PhysPn = usize;
/// Indexes into a page. Constrained by the size of the page.
pub type PageOffs = usize;

pub struct PhysState {
    /// Physical memory, indexed by physical page numbers.
    pub phys_mem: Vec<MemPage>,
}

#[derive(Debug, Copy, Clone)]
pub enum PhysDiff {
    /// Updates the data stored at the specified location. Assumes the ppn exists.
    MemSet {
        ppn: PhysPn,
        offs: PageOffs,
        diff: DataEnumDiff,
    },
}

impl PhysDiff {
    pub fn into_state_diff<F: ArchFamily<T>, T: MachineDataWidth>(self) -> StateDiff<F, T> {
        StateDiff::Phys(self)
    }
}

impl Default for PhysState {
    /// Produces a state with a memory with a 64-bit physical address space.
    /// Since pages are sparsely represented, we don't lose much runtime memory from allocating
    /// this many pages.
    fn default() -> Self {
        PhysState::new(Endianness::default(), 1, 64)
    }
}

impl PhysState {
    pub fn new(endianness: Endianness, pg_count: usize, pg_ofs_bits: usize) -> Self {
        PhysState {
            phys_mem: (0..pg_count)
                .map(|_| MemPage::new(endianness, pg_ofs_bits))
                .collect(),
        }
    }

    pub fn apply_diff(&mut self, diff: &PhysDiff) {
        match diff {
            PhysDiff::MemSet { ppn, offs, diff } => self.phys_mem[*ppn].set(*offs, diff.new_val()),
        }
    }

    pub fn revert_diff(&mut self, diff: &PhysDiff) {
        match diff {
            PhysDiff::MemSet { ppn, offs, diff } => self.phys_mem[*ppn].set(*offs, diff.old_val()),
        }
    }

    pub fn memory_get(&self, ppn: PhysPn, offs: PageOffs, width: DataWidth) -> DataEnum {
        self.phys_mem[ppn].get(offs, width)
    }

    // should really find a way type parameterize instead of using an enum
    pub fn memory_set(&self, ppn: PhysPn, offs: PageOffs, data: DataEnum) -> PhysDiff {
        let page = &self.phys_mem[ppn];
        use DataEnumDiff::*;
        PhysDiff::MemSet {
            ppn,
            offs,
            diff: match data {
                DataEnum::Byte(new) => Byte {
                    old: page.get_byte(offs),
                    new,
                },
                DataEnum::Half(new) => Half {
                    old: page.get_half(offs),
                    new,
                },
                DataEnum::Word(new) => Word {
                    old: page.get_word(offs),
                    new,
                },
                DataEnum::DoubleWord(new) => DoubleWord {
                    old: page.get_doubleword(offs),
                    new,
                },
            },
        }
    }
}

/// Represents the order that bytes in a word are stored.
/// See https://en.wikipedia.org/wiki/Endianness for more information.
#[derive(Copy, Clone)]
pub enum Endianness {
    Big,
    Little,
}

impl Default for Endianness {
    fn default() -> Self {
        Endianness::Little
    }
}

/// Represents a page of memory.
/// TODO implement default value (currently 0)
pub struct MemPage {
    endianness: Endianness,
    /// The address of the last addressable byte in the page. We can't just use the size of the page
    /// in case it's set to 2^64, in which case it would overflow usize on 64-bit systems.
    largest_idx: usize,
    /// To ensure the simulator doesn't waste space allocating the full size of the page,
    /// this backing store allocates bytes lazily.
    ///
    /// Eventually this should be modified for perf optimizations.
    backing: HashMap<PageOffs, u8>,
}

impl Default for MemPage {
    /// Produces a little-endian page of 4KiB.
    fn default() -> Self {
        MemPage::new(Endianness::Little, 12)
    }
}

impl MemPage {
    pub(crate) fn new(endianness: Endianness, ofs_bits: usize) -> MemPage {
        let largest_idx = if ofs_bits == 64 {
            (-1isize) as usize
        } else {
            (1usize).wrapping_shl(ofs_bits as u32) - 1
        };
        println!(
            "INIT: ofs_bits {:?}, largest_idx {:?}",
            ofs_bits, largest_idx
        );
        MemPage {
            endianness,
            largest_idx,
            backing: HashMap::new(),
        }
    }

    pub(crate) fn set_byte(&mut self, offs: PageOffs, value: DataByte) {
        assert!(offs <= self.largest_idx);
        self.backing.insert(offs, value.into());
    }

    pub(crate) fn get_byte(&self, offs: PageOffs) -> DataByte {
        println!("offs: {:?}, largest: {:?}", offs, self.largest_idx);
        assert!(offs <= self.largest_idx);
        (*self.backing.get(&offs).unwrap_or(&0)).into()
    }

    pub(crate) fn set_half(&mut self, offs: PageOffs, value: DataHalf) {
        let half: u16 = value.into();
        match self.endianness {
            Endianness::Big => {
                self.set_byte(offs, ((half >> 8) as u8).into());
                self.set_byte(offs + 1, (half as u8).into());
            }
            Endianness::Little => {
                self.set_byte(offs, (half as u8).into());
                self.set_byte(offs + 1, ((half >> 8) as u8).into());
            }
        }
    }

    pub(crate) fn get_half(&self, offs: PageOffs) -> DataHalf {
        let lower: u8 = self.get_byte(offs).into();
        let upper: u8 = self.get_byte(offs + 1).into();
        match self.endianness {
            Endianness::Big => DataHalf::from(((lower as u16) << 8) | (upper as u16)),
            Endianness::Little => DataHalf::from(((upper as u16) << 8) | (lower as u16)),
        }
    }

    pub(crate) fn set_word(&mut self, offs: PageOffs, value: DataWord) {
        let word: u32 = value.into();
        match self.endianness {
            Endianness::Big => {
                self.set_half(offs, ((word >> 16) as u16).into());
                self.set_half(offs + 2, (word as u16).into());
            }
            Endianness::Little => {
                self.set_half(offs, (word as u16).into());
                self.set_half(offs + 2, ((word >> 16) as u16).into());
            }
        }
    }

    pub(crate) fn get_word(&self, offs: PageOffs) -> DataWord {
        let lower: u16 = self.get_half(offs).into();
        let upper: u16 = self.get_half(offs + 2).into();
        match self.endianness {
            Endianness::Big => DataWord::from(((lower as u32) << 16) | (upper as u32)),
            Endianness::Little => DataWord::from(((upper as u32) << 16) | (lower as u32)),
        }
    }

    /// Note: if you're invoking this function, duna assumes that your word size is 64-bit, meaning
    /// endianness will apply to the whole 64 bits rather than to each 32-bit chunk.
    pub(crate) fn set_doubleword(&mut self, offs: PageOffs, value: DataDword) {
        let dword: u64 = value.into();
        match self.endianness {
            Endianness::Big => {
                self.set_word(offs, ((dword >> 32) as u32).into());
                self.set_word(offs + 4, (dword as u32).into());
            }
            Endianness::Little => {
                self.set_word(offs, (dword as u32).into());
                self.set_word(offs + 4, ((dword >> 32) as u32).into());
            }
        }
    }

    /// Note: if you're invoking this function, duna assumes that your word size is 64-bit, meaning
    /// endianness will apply to the whole 64 bits rather than to each 32-bit chunk.
    pub(crate) fn get_doubleword(&self, offs: PageOffs) -> DataDword {
        let lower: u32 = self.get_word(offs).into();
        let upper: u32 = self.get_word(offs + 4).into();
        match self.endianness {
            Endianness::Big => DataDword::from(((lower as u64) << 32) | (upper as u64)),
            Endianness::Little => DataDword::from(((upper as u64) << 32) | (lower as u64)),
        }
    }

    pub(crate) fn set(&mut self, offs: PageOffs, value: DataEnum) {
        match value {
            DataEnum::Byte(v) => self.set_byte(offs, v),
            DataEnum::Half(v) => self.set_half(offs, v),
            DataEnum::Word(v) => self.set_word(offs, v),
            DataEnum::DoubleWord(v) => self.set_doubleword(offs, v),
        }
    }

    pub(crate) fn get(&self, offs: PageOffs, w: DataWidth) -> DataEnum {
        match w {
            DataWidth::Byte => DataEnum::Byte(self.get_byte(offs)),
            DataWidth::Half => DataEnum::Half(self.get_half(offs)),
            DataWidth::Word => DataEnum::Word(self.get_word(offs)),
            DataWidth::DoubleWord => DataEnum::DoubleWord(self.get_doubleword(offs)),
        }
    }
}
