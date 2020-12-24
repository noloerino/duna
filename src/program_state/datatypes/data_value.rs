use super::BitStr32;
use num_traits::{cast::AsPrimitive, int, sign};
use std::fmt;
use std::marker::PhantomData;

#[derive(Debug, Clone, Copy)]
struct RS8b {
    value: u8,
}

#[derive(Debug, Clone, Copy)]
struct RS16b {
    value: u16,
}

#[derive(Debug, Clone, Copy)]
struct RS32b {
    value: u32,
}

#[derive(Debug, Clone, Copy)]
struct RS64b {
    value: u64,
}

pub trait Data: Clone + Copy + Sized {
    type U: int::PrimInt
        + sign::Unsigned
        + fmt::UpperHex
        + fmt::Display
        + AsPrimitive<u32>
        + AsPrimitive<u64>;
    type S: int::PrimInt + sign::Signed + fmt::Display;

    fn from_u(value: Self::U) -> Self;
    fn from_s(value: Self::S) -> Self;

    fn as_u(self) -> Self::U;
    fn as_s(self) -> Self::S;
}

impl Data for RS8b {
    type U = u8;
    type S = i8;

    fn from_u(value: Self::U) -> Self {
        Self { value }
    }

    fn from_s(value: Self::S) -> Self {
        Self {
            value: value as Self::U,
        }
    }

    fn as_u(self) -> Self::U {
        self.value
    }

    fn as_s(self) -> Self::S {
        self.value as Self::S
    }
}

impl Data for RS16b {
    type U = u16;
    type S = i16;

    fn from_u(value: Self::U) -> Self {
        Self { value }
    }

    fn from_s(value: Self::S) -> Self {
        Self {
            value: value as Self::U,
        }
    }

    fn as_u(self) -> Self::U {
        self.value
    }

    fn as_s(self) -> Self::S {
        self.value as Self::S
    }
}

impl Data for RS32b {
    type U = u32;
    type S = i32;

    fn from_u(value: Self::U) -> Self {
        Self { value }
    }

    fn from_s(value: Self::S) -> Self {
        Self {
            value: value as Self::U,
        }
    }

    fn as_u(self) -> Self::U {
        self.value
    }

    fn as_s(self) -> Self::S {
        self.value as Self::S
    }
}

impl Data for RS64b {
    type U = u64;
    type S = i64;

    fn from_u(value: Self::U) -> Self {
        Self { value }
    }

    fn from_s(value: Self::S) -> Self {
        Self {
            value: value as Self::U,
        }
    }

    fn as_u(self) -> Self::U {
        self.value
    }

    fn as_s(self) -> Self::S {
        self.value as Self::S
    }
}

// Marks an interpretation of a data width.
pub trait DataInterp {}

#[derive(Debug, Clone, Copy)]
pub struct Unsigned;
impl DataInterp for Unsigned {}

#[derive(Debug, Clone, Copy)]
pub struct Signed;
impl DataInterp for Signed {}

#[derive(Debug, Clone, Copy)]
pub struct RegValue;
impl DataInterp for RegValue {}

#[derive(Debug, Clone, Copy)]
pub struct ByteAddr;
impl DataInterp for ByteAddr {}

#[derive(Debug, Clone, Copy)]
pub struct DataValue<S: Data, T: DataInterp> {
    value: S,
    _phantom: PhantomData<T>,
}

// ===== Generic typestate interchange functions =====
impl<S: Data, T: DataInterp> DataValue<S, T> {
    pub fn from_unsigned(value: <S as Data>::U) -> Self {
        DataValue::<S, T> {
            value: <S as Data>::from_u(value),
            _phantom: PhantomData,
        }
    }

    pub fn from_signed(value: <S as Data>::S) -> Self {
        DataValue::<S, T> {
            value: <S as Data>::from_s(value),
            _phantom: PhantomData,
        }
    }

    pub fn value(self) -> S {
        self.value
    }

    pub fn as_unsigned(&self) -> DataValue<S, Unsigned> {
        DataValue::<S, Unsigned> {
            value: self.value,
            _phantom: PhantomData,
        }
    }

    pub fn as_signed(&self) -> DataValue<S, Signed> {
        DataValue::<S, Signed> {
            value: self.value,
            _phantom: PhantomData,
        }
    }

    pub fn as_reg_data(&self) -> DataValue<S, RegValue> {
        DataValue::<S, RegValue> {
            value: self.value,
            _phantom: PhantomData,
        }
    }

    pub fn as_byte_addr(&self) -> DataValue<S, ByteAddr> {
        DataValue::<S, ByteAddr> {
            value: self.value,
            _phantom: PhantomData,
        }
    }
}

// ===== Interpretation differences =====
impl<S: Data> DataValue<S, Unsigned> {
    fn raw(self) -> S::U {
        self.value().as_u()
    }
}

impl<S: Data> fmt::Display for DataValue<S, Unsigned> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.as_u())
    }
}

impl<S: Data> DataValue<S, Signed> {
    fn raw(self) -> S::S {
        self.value().as_s()
    }
}

impl<S: Data> fmt::Display for DataValue<S, Signed> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.as_s())
    }
}

impl<S: Data> DataValue<S, RegValue> {}

// === Special memory stuff ===
pub type ByteAddrValue<S> = DataValue<S, ByteAddr>;

pub type ByteAddr32 = DataValue<RS32b, ByteAddr>;

impl<S: Data> ByteAddr32 {}

pub type ByteAddr64 = DataValue<RS64b, ByteAddr>;

impl<S: Data> ByteAddr64 {}

impl<S: Data> fmt::Display for DataValue<S, ByteAddr> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#X}", self.value.as_u())
    }
}

// ===== Width-specific functions (includes sign extension/zero padding) =====

// Traits to provide sign extension methods and such
trait AtLeast8b<T: DataInterp> {
    fn zero_pad_from_byte(b: DataByte<T>) -> Self;
    fn sign_ext_from_byte(b: DataByte<T>) -> Self;
}

trait AtLeast16b<T: DataInterp>: AtLeast8b<T> {
    fn zero_pad_from_half(h: DataHalf<T>) -> Self;
    fn sign_ext_from_half(h: DataHalf<T>) -> Self;
}

trait AtLeast32b<T: DataInterp>: AtLeast16b<T> {
    fn zero_pad_from_lword(l: DataLword<T>) -> Self;
    fn sign_ext_from_lword(l: DataLword<T>) -> Self;
    fn lower_lword(self) -> DataLword<T>;
    fn to_bit_str(self, len: u8) -> BitStr32;
}

pub type DataByte<T> = DataValue<RS8b, T>;

impl<T: DataInterp> DataByte<T> {}

impl<T: DataInterp> AtLeast8b<T> for DataByte<T> {
    fn zero_pad_from_byte(b: DataByte<T>) -> Self {
        Self::from_unsigned(b.as_unsigned().raw() as u8)
    }

    fn sign_ext_from_byte(b: DataByte<T>) -> Self {
        Self::from_signed(b.as_signed().raw() as i8)
    }
}

pub type DataHalf<T> = DataValue<RS16b, T>;

impl<T: DataInterp> DataHalf<T> {}

impl<T: DataInterp> AtLeast8b<T> for DataHalf<T> {
    fn zero_pad_from_byte(b: DataByte<T>) -> Self {
        Self::from_unsigned(b.as_unsigned().raw() as u16)
    }

    fn sign_ext_from_byte(b: DataByte<T>) -> Self {
        Self::from_signed(b.as_signed().raw() as i16)
    }
}

impl<T: DataInterp> AtLeast16b<T> for DataHalf<T> {
    fn zero_pad_from_half(h: DataHalf<T>) -> Self {
        Self::from_unsigned(h.as_unsigned().raw() as u16)
    }

    fn sign_ext_from_half(h: DataHalf<T>) -> Self {
        Self::from_signed(h.as_signed().raw() as i16)
    }
}

pub type DataLword<T> = DataValue<RS32b, T>;

impl<T: DataInterp> DataLword<T> {
    pub fn zero() -> Self {
        Self::from_unsigned(0)
    }

    /// Returns a copy of the value with the ith byte set to val.
    pub fn set_byte(self, i: u8, val: DataByte<T>) -> Self {
        debug_assert!(i < 4);
        let mask: u32 = !(0xFF << (i * 8));
        DataLword::from_unsigned(
            (self.value().as_u() as u32 & mask) | ((val.value().as_u() as u32) << (i * 8)),
        )
    }

    /// Selects the ith byte in the word, where 0 is the LSB.
    pub fn get_byte(self, i: u8) -> DataByte<T> {
        debug_assert!(i < 4);
        DataByte::from_unsigned((self.value().as_u() >> (i * 8)) as u8)
    }
}

impl<T: DataInterp> AtLeast8b<T> for DataLword<T> {
    fn zero_pad_from_byte(b: DataByte<T>) -> Self {
        Self::from_unsigned(b.as_unsigned().raw() as u32)
    }

    fn sign_ext_from_byte(b: DataByte<T>) -> Self {
        Self::from_signed(b.as_signed().raw() as i32)
    }
}

impl<T: DataInterp> AtLeast16b<T> for DataLword<T> {
    fn zero_pad_from_half(h: DataHalf<T>) -> Self {
        Self::from_unsigned(h.as_unsigned().raw() as u32)
    }

    fn sign_ext_from_half(h: DataHalf<T>) -> Self {
        Self::from_signed(h.as_signed().raw() as i32)
    }
}

impl<T: DataInterp> AtLeast32b<T> for DataLword<T> {
    fn zero_pad_from_lword(l: DataLword<T>) -> Self {
        l
    }

    fn sign_ext_from_lword(l: DataLword<T>) -> Self {
        l
    }

    fn lower_lword(self) -> DataLword<T> {
        self
    }

    fn to_bit_str(self, len: u8) -> BitStr32 {
        BitStr32::new(self.as_unsigned().raw(), len)
    }
}

impl<T: DataInterp> From<BitStr32> for DataLword<T> {
    fn from(value: BitStr32) -> DataLword<T> {
        Self::from_unsigned(value.value as u32)
    }
}

impl<T: DataInterp> From<u32> for DataLword<T> {
    fn from(value: u32) -> Self {
        Self::from_unsigned(value)
    }
}

impl<T: DataInterp> From<DataLword<T>> for u32 {
    fn from(value: DataLword<T>) -> u32 {
        value.as_unsigned().raw()
    }
}

impl<T: DataInterp> From<i32> for DataLword<T> {
    fn from(value: i32) -> Self {
        Self::from_signed(value)
    }
}

impl<T: DataInterp> From<DataLword<T>> for i32 {
    fn from(value: DataLword<T>) -> i32 {
        value.as_signed().raw()
    }
}

pub type DataDword<T> = DataValue<RS64b, T>;

impl<T: DataInterp> DataDword<T> {
    pub fn zero() -> Self {
        Self::from_unsigned(0)
    }

    pub fn from_lwords(lower: DataLword<T>, upper: DataLword<T>) -> Self {
        Self::from_unsigned(((upper.value().as_u() as u64) << 32) | (lower.value().as_u() as u64))
    }

    pub fn upper_lword(self) -> DataLword<T> {
        DataLword::from_unsigned((self.value().as_u() as u64 >> 32) as u32)
    }
}

impl<T: DataInterp> AtLeast8b<T> for DataDword<T> {
    fn zero_pad_from_byte(b: DataByte<T>) -> Self {
        Self::from_unsigned(b.as_unsigned().raw() as u64)
    }

    fn sign_ext_from_byte(b: DataByte<T>) -> Self {
        Self::from_signed(b.as_signed().raw() as i64)
    }
}

impl<T: DataInterp> AtLeast16b<T> for DataDword<T> {
    fn zero_pad_from_half(h: DataHalf<T>) -> Self {
        Self::from_unsigned(h.as_unsigned().raw() as u64)
    }

    fn sign_ext_from_half(h: DataHalf<T>) -> Self {
        Self::from_signed(h.as_signed().raw() as i64)
    }
}

impl<T: DataInterp> AtLeast32b<T> for DataDword<T> {
    fn zero_pad_from_lword(l: DataLword<T>) -> Self {
        Self::from_unsigned(l.as_unsigned().raw() as u64)
    }

    fn sign_ext_from_lword(l: DataLword<T>) -> Self {
        Self::from_signed(l.as_signed().raw() as i64)
    }

    fn lower_lword(self) -> DataLword<T> {
        DataLword::from_unsigned((self.value().as_u() as u64) as u32)
    }

    fn to_bit_str(self, len: u8) -> BitStr32 {
        BitStr32::new(self.as_unsigned().raw() as u32, len)
    }
}

impl<T: DataInterp> From<BitStr32> for DataDword<T> {
    fn from(value: BitStr32) -> Self {
        Self::from_unsigned(value.value as u64)
    }
}

impl<T: DataInterp> From<u64> for DataDword<T> {
    fn from(value: u64) -> Self {
        Self::from_unsigned(value)
    }
}

impl<T: DataInterp> From<DataDword<T>> for u64 {
    fn from(value: DataDword<T>) -> u64 {
        value.as_unsigned().raw()
    }
}

impl<T: DataInterp> From<i64> for DataDword<T> {
    fn from(value: i64) -> Self {
        Self::from_signed(value)
    }
}

impl<T: DataInterp> From<DataDword<T>> for i64 {
    fn from(value: DataDword<T>) -> i64 {
        value.as_signed().raw()
    }
}
