use crate::data_structures::*;
use crate::program_state::IRegister;
use std::fmt;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum MipsRegister {
    ZERO = 0,
    AT,
    V0,
    V1,
    A0,
    A1,
    A2,
    A3,
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    S0,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    T8,
    S9,
    K0,
    K1,
    GP,
    SP,
    S8,
    RA,
}

impl fmt::Display for MipsRegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("{:?}", self);
        s.make_ascii_lowercase();
        write!(f, "{}", s)
    }
}

impl From<u8> for MipsRegister {
    fn from(value: u8) -> MipsRegister {
        MipsRegister::REG_ARRAY[value as usize]
    }
}

use MipsRegister::*;
impl MipsRegister {
    pub const FP: MipsRegister = S8;
    pub const REG_ARRAY: [MipsRegister; 32] = [
        ZERO, AT, V0, V1, A0, A1, A2, A3, T0, T1, T2, T3, T4, T5, T6, T7, S0, S1, S2, S3, S4, S5,
        S6, S7, T8, S9, K0, K1, GP, SP, S8, RA,
    ];
    pub const fn to_bit_str(self) -> BitStr32 {
        BitStr32::new(self as u32, 5)
    }
}

impl IRegister for MipsRegister {
    fn to_usize(self) -> usize {
        self as usize
    }
}
