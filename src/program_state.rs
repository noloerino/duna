use std::cmp::{max, min};
use std::ops::Add;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone)]
/// A bit vector that can fit inside 32 bits. Used to represent instruction fields.
pub struct BitStr32 {
    pub value: u32,
    pub len: u8,
}

impl BitStr32 {
    pub fn new(value: u32, len: u8) -> BitStr32 {
        BitStr32 {
            // Mask off upper bits
            value: value & ((1 << len as u32) - 1),
            len,
        }
    }

    pub fn concat(self, o: BitStr32) -> BitStr32 {
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
    pub fn index(self, i: u8) -> BitStr32 {
        BitStr32::new(self.value >> i as u32, 1)
    }
}

impl Add for BitStr32 {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        self.concat(other)
    }
}

#[derive(Copy, Clone)]
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
    pub fn to_bit_str(self) -> BitStr32 {
        BitStr32::new(self as u32, 5)
    }
    pub fn to_u8(self) -> u8 { self as u8 }
}

#[derive(Debug, Copy, Clone)]
struct ByteAddress {
    addr: u32,
}

impl ByteAddress {
    fn to_word_address(self) -> WordAddress {
        self.addr >> 2
    }

    fn get_word_offset(self) -> u8 {
        (self.addr & 0b11) as u8
    }
}

type WordAddress = u32;
type Word = u32;

pub struct Memory {
    store: HashMap<WordAddress, Word>
}

impl Memory {
    fn set_word(&mut self, addr: ByteAddress, value: Word) {
        self.store.insert(addr.to_word_address(), value);
    }

    fn get_word(&self, addr: ByteAddress) -> Word {
        if let Some(&v) = self.store.get(&addr.to_word_address()) {
            v
        } else {
            0
        }
    }
}

pub struct ProgramState {
    pub regfile: [u8; 32],
    pub memory: Memory
}

