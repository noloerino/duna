use super::*;
use crate::arch::RegSize;
use duna_macro::{ConvertInt32, ConvertInt64};
use std::fmt;

/// Represents a 64-bit double-word of data.
#[derive(Debug, Copy, Clone, PartialEq, ConvertInt64)]
pub struct DataDword {
    pub(crate) value: u64,
}

impl Data for DataDword {
    fn kind(self) -> DataEnum {
        DataEnum::DoubleWord(self)
    }

    fn width(self) -> DataWidth {
        DataWidth::DoubleWord
    }
}

impl DataDword {
    fn new(value: u64) -> DataDword {
        DataDword { value }
    }

    pub fn from_words(lower: DataWord, upper: DataWord) -> DataDword {
        DataDword {
            value: ((upper.value as u64) << 32) | (lower.value as u64),
        }
    }

    pub fn get_upper_word(self) -> DataWord {
        DataWord::from((self.value >> 32) as u32)
    }
}

impl RegSize for DataDword {
    type Signed = i64;
    type Unsigned = u64;
    type ByteAddr = ByteAddr64;

    fn as_byte_addr(self) -> Self::ByteAddr {
        ByteAddr64::from(self)
    }

    fn as_signed(self) -> Self::Signed {
        i64::from(self)
    }

    fn as_unsigned(self) -> Self::Unsigned {
        u64::from(self)
    }

    fn zero() -> DataDword {
        DataDword { value: 0 }
    }

    fn set_byte(self, i: u8, val: DataByte) -> DataDword {
        debug_assert!(i < 8);
        let mask: u64 = !(0xFF << (i * 8));
        DataDword {
            value: (self.value & mask) | ((val.value as u64) << (i * 8)),
        }
    }

    fn get_byte(self, i: u8) -> DataByte {
        debug_assert!(i < 8);
        DataByte {
            value: (self.value >> (i * 8)) as u8,
        }
    }

    fn to_bit_str(self, len: u8) -> BitStr32 {
        BitStr32::new(self.value as u32, len)
    }

    fn zero_pad_from_byte(b: DataByte) -> DataDword {
        DataDword {
            value: b.value as u64,
        }
    }

    fn sign_ext_from_byte(b: DataByte) -> DataDword {
        DataDword {
            // signed upcasts sign extend
            value: ((b.value as i8) as i64) as u64,
        }
    }

    fn zero_pad_from_half(h: DataHalf) -> DataDword {
        DataDword {
            value: h.value as u64,
        }
    }

    fn sign_ext_from_half(h: DataHalf) -> DataDword {
        DataDword {
            value: ((h.value as i16) as i64) as u64,
        }
    }

    fn get_lower_word(self) -> DataWord {
        DataWord::from(self.value as u32)
    }

    fn sign_ext_from_word(value: DataWord) -> DataDword {
        DataDword::from((value.value as i32) as u64)
    }

    fn zero_pad_from_word(value: DataWord) -> DataDword {
        DataDword::from(value.value as u64)
    }
}

impl fmt::Display for DataDword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<ByteAddr64> for DataDword {
    fn from(value: ByteAddr64) -> DataDword {
        DataDword {
            value: u64::from(value),
        }
    }
}

impl From<BitStr32> for DataDword {
    fn from(value: BitStr32) -> DataDword {
        DataDword {
            value: value.value as u64,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, ConvertInt32)]
/// Represents a 32-bit word of data that can be stored in a register.
/// This struct is used in place of a typealias to force call sites to make clear
/// whether desired behavior is that of a signed or unsigned number.
pub struct DataWord {
    pub(crate) value: u32,
}

impl Data for DataWord {
    fn kind(self) -> DataEnum {
        DataEnum::Word(self)
    }

    fn width(self) -> DataWidth {
        DataWidth::Word
    }
}

impl DataWord {
    fn new(value: u32) -> DataWord {
        DataWord { value }
    }
}

impl RegSize for DataWord {
    type Signed = i32;
    type Unsigned = u32;
    type ByteAddr = ByteAddr32;

    fn as_byte_addr(self) -> Self::ByteAddr {
        ByteAddr32::from(self)
    }

    fn as_signed(self) -> Self::Signed {
        i32::from(self)
    }

    fn as_unsigned(self) -> Self::Unsigned {
        u32::from(self)
    }

    /// Returns a DataWord of zero.
    fn zero() -> DataWord {
        DataWord { value: 0 }
    }

    /// Returns a copy of the value with the ith byte set to val.
    fn set_byte(self, i: u8, val: DataByte) -> DataWord {
        debug_assert!(i < 4);
        let mask: u32 = !(0xFF << (i * 8));
        DataWord {
            value: (self.value & mask) | ((val.value as u32) << (i * 8)),
        }
    }

    /// Selects the ith byte in the word, where 0 is the LSB.
    fn get_byte(self, i: u8) -> DataByte {
        debug_assert!(i < 4);
        DataByte {
            value: (self.value >> (i * 8)) as u8,
        }
    }

    fn to_bit_str(self, len: u8) -> BitStr32 {
        BitStr32::new(self.value, len)
    }

    fn zero_pad_from_byte(b: DataByte) -> DataWord {
        DataWord {
            value: b.value as u32,
        }
    }

    fn sign_ext_from_byte(b: DataByte) -> DataWord {
        DataWord {
            value: ((b.value as i8) as i32) as u32,
        }
    }

    fn zero_pad_from_half(h: DataHalf) -> DataWord {
        DataWord {
            value: h.value as u32,
        }
    }

    fn sign_ext_from_half(h: DataHalf) -> DataWord {
        DataWord {
            value: ((h.value as i16) as i32) as u32,
        }
    }

    fn get_lower_word(self) -> DataWord {
        DataWord::from(self.value)
    }

    fn sign_ext_from_word(value: DataWord) -> DataWord {
        value
    }

    fn zero_pad_from_word(value: DataWord) -> DataWord {
        value
    }
}

impl fmt::Display for DataWord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<i64> for DataWord {
    fn from(value: i64) -> DataWord {
        DataWord {
            value: value as u32,
        }
    }
}

impl From<ByteAddr32> for DataWord {
    fn from(value: ByteAddr32) -> DataWord {
        DataWord {
            value: u32::from(value),
        }
    }
}

impl From<BitStr32> for DataWord {
    fn from(value: BitStr32) -> DataWord {
        DataWord {
            value: value.as_u32(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct DataHalf {
    pub(crate) value: u16,
}

impl DataHalf {
    pub const fn zero() -> DataHalf {
        DataHalf { value: 0 }
    }
}

impl Data for DataHalf {
    fn kind(self) -> DataEnum {
        DataEnum::Half(self)
    }

    fn width(self) -> DataWidth {
        DataWidth::Half
    }
}

impl From<u16> for DataHalf {
    fn from(value: u16) -> DataHalf {
        DataHalf { value }
    }
}

impl From<i16> for DataHalf {
    fn from(value: i16) -> DataHalf {
        DataHalf {
            value: value as u16,
        }
    }
}

impl From<DataHalf> for u16 {
    fn from(value: DataHalf) -> u16 {
        value.value
    }
}

impl From<DataHalf> for i16 {
    fn from(value: DataHalf) -> i16 {
        value.value as i16
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct DataByte {
    pub(crate) value: u8,
}

impl Data for DataByte {
    fn kind(self) -> DataEnum {
        DataEnum::Byte(self)
    }

    fn width(self) -> DataWidth {
        DataWidth::Byte
    }
}

impl DataByte {
    pub const fn zero() -> DataByte {
        DataByte { value: 0 }
    }
}

impl From<u8> for DataByte {
    fn from(value: u8) -> DataByte {
        DataByte { value }
    }
}

impl From<i8> for DataByte {
    fn from(value: i8) -> DataByte {
        DataByte { value: value as u8 }
    }
}

impl From<DataByte> for u8 {
    fn from(value: DataByte) -> u8 {
        value.value
    }
}

impl From<DataByte> for i8 {
    fn from(value: DataByte) -> i8 {
        value.value as i8
    }
}
