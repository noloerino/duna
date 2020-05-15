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
        // Mask off upper bits
        // Shift by 32 is undef behavior, so need to use checked version; sadly it's not const fn
        // let truncated = value & !(u32::max_value().checked_shl(len as u32).unwrap_or(0));
        let shamt = (32 - len) as u32;
        let truncated = (value << shamt) >> shamt;
        BitStr32 {
            value: truncated,
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

    /// Zero pads the LSB of this BitStr32.
    pub const fn zero_pad_lsb(self) -> BitStr32 {
        let shamt = (32 - self.len) as u32;
        BitStr32::new(self.value << shamt, 32)
    }

    /// Sign extends the value and stores it in a DataWord.
    pub fn to_sgn_data_word(self) -> DataWord {
        let sign_mask = u32::max_value() << self.len as u32;
        DataWord::from(if self.index(self.len - 1).value == 1 {
            self.value | sign_mask
        } else {
            self.value
        })
    }

    pub fn as_i32(self) -> i32 {
        i32::from(self.to_sgn_data_word())
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

#[derive(Debug, Copy, Clone, PartialEq)]
/// Represents a 32-bit word of data that can be stored in a register.
/// This struct is used in place of a typealias to force call sites to make clear
/// whether desired behavior is that of a signed or unsigned number.
/// Note that Rust casts between int types of the same size is specified to be a no-op.
/// https://doc.rust-lang.org/nomicon/casts.html
pub struct DataWord {
    value: u32,
}

impl DataWord {
    /// Returns a DataWord of zero.
    pub const fn zero() -> DataWord {
        DataWord { value: 0 }
    }

    /// Returns the result of taking the 2's complement negation of the DataWord.
    pub const fn neg(self) -> DataWord {
        DataWord {
            value: !self.value + 1,
        }
    }

    pub const fn to_bit_str(self, size: u8) -> BitStr32 {
        BitStr32::new(self.value, size)
    }
}

impl From<u32> for DataWord {
    fn from(value: u32) -> DataWord {
        DataWord { value }
    }
}

impl From<i32> for DataWord {
    fn from(value: i32) -> DataWord {
        DataWord {
            value: value as u32,
        }
    }
}

impl From<DataWord> for u32 {
    fn from(value: DataWord) -> u32 {
        value.value
    }
}

impl From<DataWord> for i32 {
    fn from(value: DataWord) -> i32 {
        value.value as i32
    }
}

#[allow(dead_code)]
#[derive(Copy, Clone, PartialEq)]
pub enum IRegister {
    ZERO = 0,
    RA,
    SP,
    GP,
    TP,
    T0,
    T1,
    T2,
    FP,
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

pub const S0: IRegister = IRegister::FP;

impl IRegister {
    pub const fn to_bit_str(self) -> BitStr32 {
        BitStr32::new(self as u32, 5)
    }
    pub const fn to_usize(self) -> usize {
        self as usize
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ByteAddress {
    addr: u32,
}

impl ByteAddress {
    pub const fn to_word_address(self) -> WordAddress {
        self.addr >> 2
    }

    pub const fn get_word_offset(self) -> u8 {
        (self.addr & 0b11) as u8
    }
}

impl From<u32> for ByteAddress {
    fn from(value: u32) -> ByteAddress {
        ByteAddress { addr: value }
    }
}

impl From<i32> for ByteAddress {
    fn from(value: i32) -> ByteAddress {
        ByteAddress { addr: value as u32 }
    }
}

impl From<ByteAddress> for u32 {
    fn from(value: ByteAddress) -> u32 {
        value.addr
    }
}

impl From<ByteAddress> for i32 {
    fn from(value: ByteAddress) -> i32 {
        value.addr as i32
    }
}

pub type WordAddress = u32;

const REGFILE_SIZE: usize = 32;

pub struct RegFile {
    store: [DataWord; REGFILE_SIZE],
}

impl RegFile {
    fn new() -> RegFile {
        RegFile {
            store: [DataWord::zero(); REGFILE_SIZE],
        }
    }

    pub fn set(&mut self, rd: IRegister, val: DataWord) {
        if rd != IRegister::ZERO {
            self.store[rd.to_usize()] = val;
        }
    }

    pub fn read(&self, rs: IRegister) -> DataWord {
        self.store[rs.to_usize()]
    }
}

pub struct Memory {
    store: HashMap<WordAddress, DataWord>,
}

impl Memory {
    fn new() -> Memory {
        Memory {
            store: HashMap::new(),
        }
    }

    pub fn set_word(&mut self, addr: WordAddress, value: DataWord) {
        self.store.insert(addr, value);
    }

    pub fn get_word(&self, addr: WordAddress) -> DataWord {
        if let Some(&v) = self.store.get(&addr) {
            v
        } else {
            DataWord::zero()
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
        match diff.change_type {
            StateChangeType::Reg(reg, WordChange { new_value, .. }) => {
                self.regfile.set(reg, new_value)
            }
            StateChangeType::Mem(addr, WordChange { new_value, .. }) => {
                self.memory.set_word(addr, new_value)
            }
            StateChangeType::NoRegOrMem => (),
        }
    }

    #[allow(dead_code)]
    pub fn revert_diff(&mut self, diff: &StateChange) {
        self.pc = diff.old_pc;
        match diff.change_type {
            StateChangeType::Reg(reg, WordChange { old_value, .. }) => {
                self.regfile.set(reg, old_value)
            }
            StateChangeType::Mem(addr, WordChange { old_value, .. }) => {
                self.memory.set_word(addr, old_value)
            }
            StateChangeType::NoRegOrMem => (),
        }
    }
}

#[derive(Copy, Clone)]
enum StateChangeType {
    NoRegOrMem,
    Reg(IRegister, WordChange),
    Mem(WordAddress, WordChange),
}

#[derive(Copy, Clone)]
struct WordChange {
    old_value: DataWord,
    new_value: DataWord,
}

pub struct StateChange {
    old_pc: WordAddress,
    new_pc: WordAddress,
    change_type: StateChangeType,
}

impl StateChange {
    fn new(state: &ProgramState, new_pc: WordAddress, tgt: StateChangeType) -> StateChange {
        StateChange {
            old_pc: state.pc,
            new_pc,
            change_type: tgt,
        }
    }

    fn new_pc_p4(state: &ProgramState, tgt: StateChangeType) -> StateChange {
        StateChange::new(state, state.pc + 4, tgt)
    }

    pub fn noop(state: &ProgramState) -> StateChange {
        StateChange::new_pc_p4(state, StateChangeType::NoRegOrMem)
    }

    pub fn pc_update_op(state: &ProgramState, new_pc: WordAddress) -> StateChange {
        StateChange::new(state, new_pc, StateChangeType::NoRegOrMem)
    }

    pub fn reg_write_op(
        state: &ProgramState,
        new_pc: WordAddress,
        reg: IRegister,
        val: DataWord,
    ) -> StateChange {
        StateChange::new(
            state,
            new_pc,
            StateChangeType::Reg(
                reg,
                WordChange {
                    old_value: state.regfile.read(reg),
                    new_value: val,
                },
            ),
        )
    }

    pub fn reg_write_pc_p4(state: &ProgramState, reg: IRegister, val: DataWord) -> StateChange {
        StateChange::new_pc_p4(
            state,
            StateChangeType::Reg(
                reg,
                WordChange {
                    old_value: state.regfile.read(reg),
                    new_value: val,
                },
            ),
        )
    }

    pub fn mem_write_op(state: &ProgramState, addr: WordAddress, val: DataWord) -> StateChange {
        StateChange::new_pc_p4(
            state,
            StateChangeType::Mem(
                addr,
                WordChange {
                    old_value: state.memory.get_word(addr),
                    new_value: val,
                },
            ),
        )
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
    fn test_full_bv() {
        let bv = BitStr32::new(0xFFFF_FFFF, 32);
        assert_eq!(bv.as_u32(), 0xFFFF_FFFF);
    }

    #[test]
    fn test_bv_truncate() {
        let all_ones = 0xFFFF_FFFF;
        let bv = BitStr32::new(all_ones, 12);
        assert_eq!(bv.as_u32(), 0xFFF);
        assert_eq!(bv.as_i32(), all_ones as i32);
    }

    #[test]
    fn test_bv_zero_pad() {
        let bv = BitStr32::new(0x7FF, 12);
        assert_eq!(bv.zero_pad_lsb().as_u32(), 0x7FF0_0000);
    }
}
