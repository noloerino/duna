use duna_macro::*;
use std::cmp::{max, min};
use std::fmt;
use std::num::Wrapping;
use std::ops::{Add, BitAnd, BitOr, Shl};

/// Represents a data type that can be used to hold data in a register.
pub trait RegSize: Copy + Clone + PartialEq {
    fn zero() -> Self;

    /// Returns a copy of the value with the ith byte set to val.
    fn set_byte(self, i: u8, val: DataByte) -> Self;

    /// Selects the ith byte in the word, where 0 is the LSB.
    fn get_byte(self, i: u8) -> DataByte;

    /// Converts this into a bit string, truncating if necessary.
    fn to_bit_str(self, len: u8) -> BitStr32;

    fn zero_pad_from_byte(b: DataByte) -> Self;

    fn sign_ext_from_byte(b: DataByte) -> Self;
}

/// Encodes the difference between a 32-bit and 64-bit system.
pub trait MachineDataWidth {
    type Signed: From<BitStr32>
        + From<Self::RegData>
        + From<Self::ByteAddr>
        + Eq
        + Ord
        + Add<Output = Self::Signed>
        + BitAnd<Output = Self::Signed>
        + BitOr<Output = Self::Signed>
        + Shl<usize, Output = Self::Signed>;
    type Unsigned: From<Self::RegData>
        + From<Self::ByteAddr>
        + Eq
        + Ord
        + Add<Output = Self::Unsigned>;
    type RegData: RegSize + From<Self::Signed> + From<Self::Unsigned> + From<Self::ByteAddr>;
    type ByteAddr: ByteAddress + From<Self::Signed> + From<Self::Unsigned> + From<Self::RegData>;

    fn sgn_zero() -> Self::Signed;
    fn sgn_one() -> Self::Signed;
}

pub struct Width32b;

impl MachineDataWidth for Width32b {
    type Signed = Wrapping<i32>;
    type Unsigned = Wrapping<u32>;
    type RegData = DataWord;
    type ByteAddr = ByteAddr32;

    fn sgn_zero() -> Self::Signed {
        Wrapping(0i32)
    }

    fn sgn_one() -> Self::Signed {
        Wrapping(1i32)
    }
}

pub struct Width64b;

impl MachineDataWidth for Width64b {
    type Signed = Wrapping<i64>;
    type Unsigned = Wrapping<u64>;
    type RegData = DataDword;
    type ByteAddr = ByteAddr64;

    fn sgn_zero() -> Self::Signed {
        Wrapping(0i64)
    }

    fn sgn_one() -> Self::Signed {
        Wrapping(1i64)
    }
}

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

    pub const fn is_zero(self) -> bool {
        self.value == 0
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

impl From<BitStr32> for i32 {
    fn from(value: BitStr32) -> i32 {
        value.to_sgn_data_word().into()
    }
}

impl From<BitStr32> for i64 {
    fn from(value: BitStr32) -> i64 {
        // rust sign extends automatically
        i32::from(value) as i64
    }
}

impl From<BitStr32> for Wrapping<i32> {
    fn from(value: BitStr32) -> Wrapping<i32> {
        Wrapping(i32::from(value))
    }
}

impl From<BitStr32> for Wrapping<i64> {
    fn from(value: BitStr32) -> Wrapping<i64> {
        Wrapping(i64::from(value))
    }
}

/// Represents a 64-bit double-word of data.
#[derive(Debug, Copy, Clone, PartialEq, ConvertInt64)]
pub struct DataDword {
    value: u64,
}

impl DataDword {
    fn new(value: u64) -> DataDword {
        DataDword { value }
    }
}

impl RegSize for DataDword {
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
}

impl From<ByteAddr64> for DataDword {
    fn from(value: ByteAddr64) -> DataDword {
        DataDword {
            value: u64::from(value),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, ConvertInt32)]
/// Represents a 32-bit word of data that can be stored in a register.
/// This struct is used in place of a typealias to force call sites to make clear
/// whether desired behavior is that of a signed or unsigned number.
pub struct DataWord {
    value: u32,
}

impl DataWord {
    fn new(value: u32) -> DataWord {
        DataWord { value }
    }
}

impl RegSize for DataWord {
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
}

impl fmt::Display for DataWord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<ByteAddr32> for DataWord {
    fn from(value: ByteAddr32) -> DataWord {
        DataWord {
            value: u32::from(value),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct DataHalf {
    value: u16,
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
    value: u8,
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

pub trait ByteAddress {
    type WordAddress;

    fn to_word_address(self) -> Self::WordAddress;

    fn get_word_offset(self) -> u8;

    fn plus_4(self) -> Self
    where
        Self: Sized;
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
    type WordAddress = u64;

    fn to_word_address(self) -> Self::WordAddress {
        self.value >> 2
    }

    fn get_word_offset(self) -> u8 {
        (self.value & 0b11) as u8
    }

    fn plus_4(self) -> ByteAddr64 {
        ByteAddr64::new(self.value.wrapping_add(4))
    }
}

impl From<DataDword> for ByteAddr64 {
    fn from(value: DataDword) -> ByteAddr64 {
        ByteAddr64::new(value.value)
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
    type WordAddress = u32;

    fn to_word_address(self) -> Self::WordAddress {
        self.value >> 2
    }

    fn get_word_offset(self) -> u8 {
        (self.value & 0b11) as u8
    }

    fn plus_4(self) -> ByteAddr32 {
        ByteAddr32::new(self.value.wrapping_add(4))
    }
}

impl From<DataWord> for ByteAddr32 {
    fn from(value: DataWord) -> ByteAddr32 {
        ByteAddr32::new(value.value)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bv_as_i32() {
        let bv = BitStr32::new(-4i32 as u32, 12);
        assert_eq!(i32::from(bv), -4);
        let wrap = BitStr32::new(-1i32 as u32, 12);
        assert_eq!(wrap.into(), -1);
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
        assert_eq!(i32::from(bv), all_ones as i32);
    }

    #[test]
    fn test_bv_zero_pad() {
        let bv = BitStr32::new(0x7FF, 12);
        assert_eq!(bv.zero_pad_lsb().as_u32(), 0x7FF0_0000);
    }
}
