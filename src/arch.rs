//! Defines a few traits needed to add support for a new architecture to duna.
use crate::assembler::parser::Parser;
use crate::instruction::ConcreteInst;
use crate::program_state::*;
use num_traits::int;
use num_traits::ops::wrapping;
use num_traits::sign;
use std::fmt;

/// Represents an architecture including word size, e.g. "x86-64" or "riscv-32".
pub trait Architecture: Sized {
    type DataWidth: MachineDataWidth;
    type Family: ArchFamily<Self::DataWidth>;
    type Program: Program<Self::Family, Self::DataWidth>;
    type Parser: Parser<Self::Family, Self::DataWidth>;
}

/// Represents an architecture family parametrized over a word size, e.g. "x86" or "riscv".
pub trait ArchFamily<T: MachineDataWidth>: Sized {
    type Register: IRegister;
    type Instruction: ConcreteInst<Self, T>;
    type Syscalls: SyscallConvention<Self, T>;
}

/// Represents a data type that can be used to hold data in a register.
/// Any size must implement conversion from i64 and BitStr32 in order to accomodate immediates
/// produced from parsing.
pub trait RegSize: Copy + Clone + PartialEq + fmt::Display + From<BitStr32> + From<i64> {
    fn zero() -> Self;

    /// Returns a copy of the value with the ith byte set to val.
    fn set_byte(self, i: u8, val: DataByte) -> Self;

    /// Selects the ith byte in the word, where 0 is the LSB.
    fn get_byte(self, i: u8) -> DataByte;

    /// Converts this into a bit string, truncating if necessary.
    fn to_bit_str(self, len: u8) -> BitStr32;

    fn zero_pad_from_byte(b: DataByte) -> Self;
    fn sign_ext_from_byte(b: DataByte) -> Self;
    fn zero_pad_from_half(h: DataHalf) -> Self;
    fn sign_ext_from_half(h: DataHalf) -> Self;
    /// Sign extends a DataWord into this type if necessary.
    fn sign_ext_from_word(value: DataWord) -> Self;

    /// Gets the lower 32 bits of this object.
    fn get_lower_word(self) -> DataWord;
}

/// Encodes the difference between a 32-bit and 64-bit system.
pub trait MachineDataWidth: Clone + Copy {
    type Signed: From<BitStr32>
        + From<Self::RegData>
        + From<Self::ByteAddr>
        + Eq
        + Ord
        + int::PrimInt
        + sign::Signed
        + wrapping::WrappingAdd
        + Copy
        + Clone;
    type Unsigned: From<Self::RegData>
        + From<Self::ByteAddr>
        + Eq
        + Ord
        + int::PrimInt
        + sign::Unsigned
        + wrapping::WrappingAdd
        + wrapping::WrappingSub
        + Copy
        + Clone;
    type RegData: RegSize + From<Self::Signed> + From<Self::Unsigned> + From<Self::ByteAddr>;
    type ByteAddr: ByteAddress + From<Self::Signed> + From<Self::Unsigned> + From<Self::RegData>;

    fn sgn_zero() -> Self::Signed;
    fn sgn_one() -> Self::Signed;
    fn sgn_to_isize(n: Self::Signed) -> isize;
    fn isize_to_sgn(n: isize) -> Self::Signed;
    fn usgn_to_usize(n: Self::Unsigned) -> usize;
    fn usize_to_usgn(n: usize) -> Self::Unsigned;
}

#[derive(Clone, Copy)]
pub struct Width32b;

impl MachineDataWidth for Width32b {
    type Signed = i32;
    type Unsigned = u32;
    type RegData = DataWord;
    type ByteAddr = ByteAddr32;

    fn sgn_zero() -> Self::Signed {
        0i32
    }

    fn sgn_one() -> Self::Signed {
        1i32
    }

    fn sgn_to_isize(n: i32) -> isize {
        n as isize
    }

    fn isize_to_sgn(n: isize) -> i32 {
        n as i32
    }

    fn usgn_to_usize(n: u32) -> usize {
        n as usize
    }

    fn usize_to_usgn(n: usize) -> u32 {
        n as u32
    }
}

#[derive(Clone, Copy)]
pub struct Width64b;

impl MachineDataWidth for Width64b {
    type Signed = i64;
    type Unsigned = u64;
    type RegData = DataDword;
    type ByteAddr = ByteAddr64;

    fn sgn_zero() -> Self::Signed {
        0i64
    }

    fn sgn_one() -> Self::Signed {
        1i64
    }

    fn sgn_to_isize(n: i64) -> isize {
        n as isize
    }

    fn isize_to_sgn(n: isize) -> i64 {
        n as i64
    }

    fn usgn_to_usize(n: u64) -> usize {
        n as usize
    }

    fn usize_to_usgn(n: usize) -> u64 {
        n as u64
    }
}
