use std::cmp::{max, min};
use std::fmt;
use std::ops::Add;

/// Represents a data type that can be used to hold data in a register.
pub trait RegSize {
    fn zero() -> Self;

    /// Returns a copy of the value with the ith byte set to val.
    fn set_byte(self, i: u8, val: DataByte) -> Self;

    /// Selects the ith byte in the word, where 0 is the LSB.
    fn get_byte(self, i: u8) -> DataByte;
}

/// Encodes the difference between a 32-bit and 64-bit system.
pub trait MachineDataWidth {
    type Signed;
    type Unsigned;
    type RegData: RegSize;
    type ByteAddr: ByteAddress;
}

pub struct Width32b;

impl MachineDataWidth for Width32b {
    type Signed = i32;
    type Unsigned = u32;
    type RegData = DataWord;
    type ByteAddr = ByteAddr32;
}

pub struct Width64b;

impl MachineDataWidth for Width64b {
    type Signed = u64;
    type Unsigned = u64;
    type RegData = DataDword;
    type ByteAddr = ByteAddr64;
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

/// Represents a 64-bit double-word of data.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct DataDword {
    value: u64,
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
}

impl From<ByteAddr64> for DataWord {
    fn from(value: ByteAddr64) -> DataWord {
        DataDword {
            value: u64::from(value),
        }
    }
}

impl From<u64> for DataDword {
    fn from(value: u64) -> DataDword {
        DataDword { value }
    }
}

impl From<i64> for DataDword {
    fn from(value: i64) -> DataDword {
        DataDword {
            value: value as u64,
        }
    }
}

impl From<DataDword> for u64 {
    fn from(value: DataDword) -> u64 {
        value.value
    }
}

impl From<DataDword> for i64 {
    fn from(value: DataDword) -> i64 {
        value.value as i64
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
/// Represents a 32-bit word of data that can be stored in a register.
/// This struct is used in place of a typealias to force call sites to make clear
/// whether desired behavior is that of a signed or unsigned number.
pub struct DataWord {
    value: u32,
}

impl DataWord {
    pub const fn to_bit_str(self, size: u8) -> BitStr32 {
        BitStr32::new(self.value, size)
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

    pub const fn zero_pad(self) -> DataWord {
        DataWord {
            value: self.value as u32,
        }
    }

    pub fn sign_extend(self) -> DataWord {
        if (self.value >> 7) > 0 {
            DataWord {
                value: (self.value as u32) | 0xFFFF_FF00,
            }
        } else {
            self.zero_pad()
        }
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

    fn plus_4(self) -> Self;
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct ByteAddr64 {
    addr: u64,
}

impl ByteAddr64 {
    pub const fn new(v: u64) -> ByteAddr64 {
        ByteAddr64 { addr: v }
    }
}

impl ByteAddress for ByteAddr64 {
    type WordAddress = u64;

    fn to_word_address(self) -> Self::WordAddress {
        self.addr >> 2
    }

    fn get_word_offset(self) -> u8 {
        (self.addr & 0b11) as u8
    }

    fn plus_4(self) -> ByteAddr64 {
        ByteAddr64::new(self.addr.wrapping_add(4))
    }
}

impl From<DataDword> for ByteAddr64 {
    fn from(value: DataDword) -> ByteAddr64 {
        ByteAddr64::new(value.value)
    }
}

impl From<u64> for ByteAddr64 {
    fn from(value: u64) -> ByteAddr64 {
        ByteAddr64::new(value)
    }
}

impl From<i64> for ByteAddr64 {
    fn from(value: i64) -> ByteAddr64 {
        ByteAddr64::new(value as u64)
    }
}

impl From<ByteAddr64> for u64 {
    fn from(value: ByteAddr64) -> u64 {
        value.addr
    }
}

impl From<ByteAddr64> for i64 {
    fn from(value: ByteAddr64) -> i64 {
        value.addr as i64
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct ByteAddr32 {
    addr: u32,
}

impl ByteAddr32 {
    const fn new(v: u32) -> ByteAddr32 {
        ByteAddr32 { addr: v }
    }
}

impl ByteAddress for ByteAddr32 {
    type WordAddress = u32;

    fn to_word_address(self) -> Self::WordAddress {
        self.addr >> 2
    }

    fn get_word_offset(self) -> u8 {
        (self.addr & 0b11) as u8
    }

    fn plus_4(self) -> ByteAddr32 {
        ByteAddr32::new(self.addr.wrapping_add(4))
    }
}

impl From<DataWord> for ByteAddr32 {
    fn from(value: DataWord) -> ByteAddr32 {
        ByteAddr32::new(value.value)
    }
}

impl From<u32> for ByteAddr32 {
    fn from(value: u32) -> ByteAddr32 {
        ByteAddr32::new(value)
    }
}

impl From<i32> for ByteAddr32 {
    fn from(value: i32) -> ByteAddr32 {
        ByteAddr32::new(value as u32)
    }
}

impl From<ByteAddr32> for u32 {
    fn from(value: ByteAddr32) -> u32 {
        value.addr
    }
}

impl From<ByteAddr32> for i32 {
    fn from(value: ByteAddr32) -> i32 {
        value.addr as i32
    }
}

#[cfg(test)]
mod test {
    use super::*;

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
