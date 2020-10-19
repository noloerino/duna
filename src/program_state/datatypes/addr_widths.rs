use super::*;
use crate::arch::RegSize;
use duna_macro::{ConvertInt32, ConvertInt64};
use num_traits::cast;
use num_traits::int;
use num_traits::sign;
use std::fmt;
use std::hash::Hash;

pub trait ByteAddress: Clone + Copy + Sized + fmt::Debug + fmt::Display + 'static {
    type Signed: int::PrimInt + sign::Signed;
    type Unsigned: int::PrimInt + sign::Unsigned;
    type RegData: RegSize;

    fn as_unsigned(self) -> Self::Unsigned;

    fn as_signed(self) -> Self::Signed;

    fn as_reg_data(self) -> Self::RegData;

    type WordAddress: Hash + Eq + Copy + Clone + cast::AsPrimitive<usize>;

    /// Gets the number of bits in this address type.
    fn bitlen() -> usize;

    /// Gets the raw bits in this address.
    fn bits(self) -> u64;

    fn to_word_address(self) -> Self::WordAddress;

    fn get_word_offset(self) -> u8;

    fn plus_4(self) -> Self;

    fn plus_1(self) -> Self;

    /// Checks the alignment of the address.
    fn is_aligned_to(self, width: DataWidth) -> bool {
        let bits = self.bits();
        match width {
            DataWidth::Byte => true,
            DataWidth::Half => bits % 2 == 0,
            DataWidth::Word => bits % 4 == 0,
            DataWidth::DoubleWord => bits % 8 == 0,
        }
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone, ConvertInt64)]
pub struct ByteAddr64 {
    value: u64,
}

impl ByteAddr64 {
    fn new(value: u64) -> ByteAddr64 {
        ByteAddr64 { value }
    }
}

impl ByteAddress for ByteAddr64 {
    type Signed = i64;
    type Unsigned = u64;
    type RegData = DataDword;

    fn as_unsigned(self) -> Self::Unsigned {
        u64::from(self)
    }

    fn as_signed(self) -> Self::Signed {
        i64::from(self)
    }

    fn as_reg_data(self) -> Self::RegData {
        DataDword::from(self)
    }

    type WordAddress = u64;

    fn bitlen() -> usize {
        64
    }

    fn bits(self) -> u64 {
        self.value
    }

    fn to_word_address(self) -> Self::WordAddress {
        self.value >> 2
    }

    fn get_word_offset(self) -> u8 {
        (self.value & 0b11) as u8
    }

    fn plus_4(self) -> ByteAddr64 {
        ByteAddr64::new(self.value.wrapping_add(4))
    }

    fn plus_1(self) -> ByteAddr64 {
        ByteAddr64::new(self.value.wrapping_add(1))
    }
}

impl From<DataDword> for ByteAddr64 {
    fn from(value: DataDword) -> ByteAddr64 {
        ByteAddr64::new(value.value)
    }
}

impl fmt::Display for ByteAddr64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#X}", self.value)
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone, ConvertInt32)]
pub struct ByteAddr32 {
    value: u32,
}

impl ByteAddr32 {
    fn new(value: u32) -> ByteAddr32 {
        ByteAddr32 { value }
    }
}

impl ByteAddress for ByteAddr32 {
    type Signed = i32;
    type Unsigned = u32;
    type RegData = DataWord;

    fn as_unsigned(self) -> Self::Unsigned {
        u32::from(self)
    }

    fn as_signed(self) -> Self::Signed {
        i32::from(self)
    }

    fn as_reg_data(self) -> Self::RegData {
        DataWord::from(self)
    }

    type WordAddress = u32;

    fn bitlen() -> usize {
        32
    }

    fn bits(self) -> u64 {
        self.value as u64
    }

    fn to_word_address(self) -> Self::WordAddress {
        self.value >> 2
    }

    fn get_word_offset(self) -> u8 {
        (self.value & 0b11) as u8
    }

    fn plus_4(self) -> ByteAddr32 {
        ByteAddr32::new(self.value.wrapping_add(4))
    }

    fn plus_1(self) -> ByteAddr32 {
        ByteAddr32::new(self.value.wrapping_add(1))
    }
}

impl From<DataWord> for ByteAddr32 {
    fn from(value: DataWord) -> ByteAddr32 {
        ByteAddr32::new(value.value)
    }
}

impl From<u64> for ByteAddr32 {
    fn from(value: u64) -> ByteAddr32 {
        ByteAddr32::new(value as u32)
    }
}

impl fmt::Display for ByteAddr32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#X}", self.value)
    }
}
