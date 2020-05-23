use crate::program_state::datatypes::*;
use std::fmt;

#[derive(Copy, Clone, PartialEq, Debug)]
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

impl fmt::Display for IRegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("{:?}", self);
        s.make_ascii_lowercase();
        write!(f, "{}", s)
    }
}

impl From<u8> for IRegister {
    fn from(value: u8) -> IRegister {
        IRegister::REG_ARRAY[value as usize]
    }
}

impl IRegister {
    pub const S0: IRegister = IRegister::FP;
    pub const REG_ARRAY: [IRegister; 32] = [
        IRegister::ZERO,
        IRegister::RA,
        IRegister::SP,
        IRegister::GP,
        IRegister::TP,
        IRegister::T0,
        IRegister::T1,
        IRegister::T2,
        IRegister::FP,
        IRegister::S1,
        IRegister::A0,
        IRegister::A1,
        IRegister::A2,
        IRegister::A3,
        IRegister::A4,
        IRegister::A5,
        IRegister::A6,
        IRegister::A7,
        IRegister::S2,
        IRegister::S3,
        IRegister::S4,
        IRegister::S5,
        IRegister::S6,
        IRegister::S7,
        IRegister::S8,
        IRegister::S9,
        IRegister::S10,
        IRegister::S11,
        IRegister::T3,
        IRegister::T4,
        IRegister::T5,
        IRegister::T6,
    ];
    pub const fn to_bit_str(self) -> BitStr32 {
        BitStr32::new(self as u32, 5)
    }
    pub const fn to_usize(self) -> usize {
        self as usize
    }
}

const REGFILE_SIZE: usize = 32;

pub struct RegFile {
    store: [DataWord; REGFILE_SIZE],
}

impl RegFile {
    pub(in crate::program_state) fn new() -> RegFile {
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
