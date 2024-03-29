use crate::{data_structures::*, program_state::IRegister};
use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum RiscVRegister {
    Zero = 0,
    Ra,
    Sp,
    Gp,
    Tp,
    T0,
    T1,
    T2,
    S0,
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

impl fmt::Display for RiscVRegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("{:?}", self);
        s.make_ascii_lowercase();
        write!(f, "{}", s)
    }
}

impl From<u8> for RiscVRegister {
    fn from(value: u8) -> RiscVRegister {
        RiscVRegister::REG_ARRAY[value as usize]
    }
}

impl RiscVRegister {
    #[allow(non_upper_case_globals)]
    pub const Fp: RiscVRegister = RiscVRegister::S0;
    pub const REG_ARRAY: [RiscVRegister; 32] = [
        RiscVRegister::Zero,
        RiscVRegister::Ra,
        RiscVRegister::Sp,
        RiscVRegister::Gp,
        RiscVRegister::Tp,
        RiscVRegister::T0,
        RiscVRegister::T1,
        RiscVRegister::T2,
        RiscVRegister::Fp,
        RiscVRegister::S1,
        RiscVRegister::A0,
        RiscVRegister::A1,
        RiscVRegister::A2,
        RiscVRegister::A3,
        RiscVRegister::A4,
        RiscVRegister::A5,
        RiscVRegister::A6,
        RiscVRegister::A7,
        RiscVRegister::S2,
        RiscVRegister::S3,
        RiscVRegister::S4,
        RiscVRegister::S5,
        RiscVRegister::S6,
        RiscVRegister::S7,
        RiscVRegister::S8,
        RiscVRegister::S9,
        RiscVRegister::S10,
        RiscVRegister::S11,
        RiscVRegister::T3,
        RiscVRegister::T4,
        RiscVRegister::T5,
        RiscVRegister::T6,
    ];
    pub const fn to_bit_str(self) -> BitStr32 {
        BitStr32::new(self as u32, 5)
    }
}

impl IRegister for RiscVRegister {
    fn to_usize(self) -> usize {
        self as usize
    }
}
