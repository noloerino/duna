use super::*;
use std::{
    cmp::{max, min},
    num::Wrapping,
    ops::Add,
};

/// A bit vector that can fit inside 32 bits. Used to represent instruction fields.
#[derive(Debug, Copy, Clone)]
pub struct BitStr32 {
    pub(crate) value: u32,
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

    /// Sign extends the value and stores it in a DataLword.
    pub fn to_sgn_data_word(self) -> DataLword {
        // Prevent overflow
        if self.len == 32 {
            return DataLword::from(self.value);
        }
        let sign_mask = u32::max_value() << self.len as u32;
        DataLword::from(if self.index(self.len - 1).value == 1 {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bv_as_i32() {
        let bv = BitStr32::new(-4i32 as u32, 12);
        assert_eq!(i32::from(bv), -4);
        let wrap = BitStr32::new(-1i32 as u32, 12);
        assert_eq!(i32::from(wrap), -1);
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
