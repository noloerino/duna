use crate::instruction::*;
use std::cmp::{max, min};
use std::collections::HashMap;
use std::ops::Add;

#[derive(Debug, Copy, Clone)]
/// A bit vector that can fit inside 32 bits. Used to represent instruction fields.
pub struct BitStr32 {
    value: u32,
    pub len: u8,
}

impl BitStr32 {
    pub const fn new(value: u32, len: u8) -> BitStr32 {
        BitStr32 {
            // Mask off upper bits
            value: value & ((1 << len as u32) - 1),
            len,
        }
    }

    pub const fn concat(self, o: BitStr32) -> BitStr32 {
        BitStr32::new((self.value << o.len as u32) | o.value, self.len + o.len)
    }

    /// Extracts a the bits between start and end, inclusive.
    pub fn slice(self, start: u8, end: u8) -> BitStr32 {
        let high = max(start, end);
        let low = min(start, end);
        let len = high - low + 1;
        BitStr32::new(self.value >> low as u32, len)
    }

    /// Extracts the bit at index i.
    pub const fn index(self, i: u8) -> BitStr32 {
        BitStr32::new(self.value >> i as u32, 1)
    }

    pub fn as_i32(self) -> i32 {
        let sign_mask = !((1 << self.len as u32) - 1);
        (if self.index(self.len - 1).value == 1 {
            self.value | sign_mask
        } else {
            self.value
        }) as i32
    }

    pub const fn as_u32(self) -> u32 {
        self.value
    }
}

impl Add for BitStr32 {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        self.concat(other)
    }
}

#[allow(dead_code)]
#[derive(Copy, Clone, PartialEq)]
pub enum IRegister {
    Zero = 0,
    Ra,
    Sp,
    Gp,
    Tp,
    T0,
    T1,
    T2,
    Fp,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    T3,
    T4,
    T5,
    T6,
}

impl IRegister {
    pub const fn to_bit_str(self) -> BitStr32 {
        BitStr32::new(self as u32, 5)
    }
    pub const fn to_usize(self) -> usize {
        self as usize
    }
}

#[derive(Debug, Copy, Clone)]
struct ByteAddress {
    addr: u32,
}

impl ByteAddress {
    const fn to_word_address(self) -> WordAddress {
        self.addr >> 2
    }

    const fn get_word_offset(self) -> u8 {
        (self.addr & 0b11) as u8
    }
}

pub type WordAddress = u32;
pub type Word = u32;

const REGFILE_SIZE: usize = 32;

pub struct RegFile {
    store: [Word; REGFILE_SIZE],
}

impl RegFile {
    fn new() -> RegFile {
        RegFile {
            store: [0; REGFILE_SIZE],
        }
    }

    pub fn set(&mut self, rd: IRegister, val: Word) {
        if rd != IRegister::Zero {
            self.store[rd.to_usize()] = val;
        }
    }

    pub fn read(&self, rs: IRegister) -> Word {
        self.store[rs.to_usize()]
    }
}

pub struct Memory {
    store: HashMap<WordAddress, Word>,
}

impl Memory {
    fn new() -> Memory {
        Memory {
            store: HashMap::new(),
        }
    }

    pub fn set_word(&mut self, addr: WordAddress, value: Word) {
        self.store.insert(addr, value);
    }

    pub fn get_word(&self, addr: WordAddress) -> Word {
        if let Some(&v) = self.store.get(&addr) {
            v
        } else {
            0
        }
    }
}

pub struct ProgramState {
    pub pc: WordAddress,
    pub regfile: RegFile,
    pub memory: Memory,
}

impl ProgramState {
    pub fn new() -> ProgramState {
        ProgramState {
            pc: 0,
            regfile: RegFile::new(),
            memory: Memory::new(),
        }
    }

    pub fn apply_inst(&mut self, inst: &ConcreteInst) {
        self.apply_diff(&(*inst.eval)(self));
    }

    pub fn apply_diff(&mut self, diff: &StateChange) {
        self.pc = diff.new_pc;
        match diff.change_target {
            StateChangeTarget::RegChange(reg) => self.regfile.set(reg, diff.new_value),
            StateChangeTarget::MemChange(addr) => self.memory.set_word(addr, diff.new_value),
        }
    }
}

#[derive(Copy, Clone)]
pub enum StateChangeTarget {
    RegChange(IRegister),
    MemChange(WordAddress),
}

pub struct StateChange {
    pub old_pc: WordAddress,
    pub new_pc: WordAddress,
    pub change_target: StateChangeTarget,
    pub old_value: Word,
    pub new_value: Word,
}

impl StateChange {
    fn new(
        state: &ProgramState,
        new_pc: WordAddress,
        tgt: StateChangeTarget,
        new: Word,
    ) -> StateChange {
        StateChange {
            old_pc: state.pc,
            new_pc: new_pc,
            change_target: tgt,
            old_value: match tgt {
                StateChangeTarget::RegChange(reg) => state.regfile.read(reg),
                StateChangeTarget::MemChange(addr) => state.memory.get_word(addr),
            },
            new_value: new,
        }
    }

    fn new_pc_p4(state: &ProgramState, tgt: StateChangeTarget, new: Word) -> StateChange {
        StateChange::new(state, state.pc + 4, tgt, new)
    }

    pub fn reg_write_op(
        state: &ProgramState,
        new_pc: WordAddress,
        reg: IRegister,
        val: Word,
    ) -> StateChange {
        StateChange::new(state, new_pc, StateChangeTarget::RegChange(reg), val)
    }

    pub fn reg_write_pc_p4(state: &ProgramState, reg: IRegister, val: Word) -> StateChange {
        StateChange::new_pc_p4(state, StateChangeTarget::RegChange(reg), val)
    }

    pub fn mem_write_op(state: &ProgramState, addr: WordAddress, val: Word) -> StateChange {
        StateChange::new_pc_p4(state, StateChangeTarget::MemChange(addr), val)
    }
}

#[cfg(test)]
mod test {
    use crate::program_state::BitStr32;

    #[test]
    fn test_bv_as_i32() {
        let bv = BitStr32::new(-4i32 as u32, 12);
        assert_eq!(bv.as_i32(), -4);
        let wrap = BitStr32::new(-1i32 as u32, 12);
        assert_eq!(wrap.as_i32(), -1);
    }

    #[test]
    fn test_bv_truncate() {
        let all_ones = 0xFFFF_FFFF;
        let bv = BitStr32::new(all_ones, 12);
        assert_eq!(bv.as_u32(), 0xFFF);
        assert_eq!(bv.as_i32(), all_ones as i32);
    }
}
