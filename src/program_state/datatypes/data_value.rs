use super::{BitStr32, DataEnum};
use num_traits::{
    bounds::Bounded,
    cast::{AsPrimitive, FromPrimitive},
    int::PrimInt,
    ops::wrapping::{WrappingAdd, WrappingSub},
    sign,
};
use std::fmt;
use std::marker::PhantomData;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RS8b {
    value: u8,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RS16b {
    value: u16,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RS32b {
    value: u32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RS64b {
    value: u64,
}

// Marker traits to allow relevant sign extension methods etc.
pub trait AtLeast8b: Data {}
pub trait AtLeast16b: AtLeast8b {}
pub trait AtLeast32b: AtLeast16b {}

impl AtLeast8b for RS8b {}
impl AtLeast8b for RS16b {}
impl AtLeast8b for RS32b {}
impl AtLeast8b for RS64b {}

impl AtLeast16b for RS16b {}
impl AtLeast16b for RS32b {}
impl AtLeast16b for RS64b {}

impl AtLeast32b for RS32b {}
impl AtLeast32b for RS64b {}

pub trait Data: fmt::Debug + Clone + Copy + PartialEq + Sized + 'static {
    type U: PrimInt
        + sign::Unsigned
        + fmt::UpperHex
        + fmt::Display
        + WrappingAdd
        + AsPrimitive<u32>
        + AsPrimitive<u64>
        + AsPrimitive<usize>
        + Bounded
        + FromPrimitive;
    type S: PrimInt
        + sign::Signed
        + fmt::Display
        + WrappingAdd
        + WrappingSub
        + AsPrimitive<isize>
        + Bounded
        + FromPrimitive;

    fn from_u(value: Self::U) -> Self;
    fn from_s(value: Self::S) -> Self;

    // Needed because num_traits casting is checked rather than saturating
    fn from_u64(value: u64) -> Self;

    fn as_u(self) -> Self::U;
    fn as_s(self) -> Self::S;

    fn is_aligned(self) -> bool;

    fn as_enum(self) -> DataEnum;
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

    fn from_u64(value: u64) -> Self {
        Self { value: value as u8 }
    }

    fn as_u(self) -> Self::U {
        self.value
    }

    fn as_s(self) -> Self::S {
        self.value as Self::S
    }

    fn is_aligned(self) -> bool {
        true
    }

    fn as_enum(self) -> DataEnum {
        DataEnum::Byte(DataByte::new(self))
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

    fn from_u64(value: u64) -> Self {
        Self {
            value: value as u16,
        }
    }

    fn as_u(self) -> Self::U {
        self.value
    }

    fn as_s(self) -> Self::S {
        self.value as Self::S
    }

    fn is_aligned(self) -> bool {
        self.value % 2 == 0
    }

    fn as_enum(self) -> DataEnum {
        DataEnum::Half(DataHalf::new(self))
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
            value: value as u32,
        }
    }

    fn from_u64(value: u64) -> Self {
        Self {
            value: value as u32,
        }
    }

    fn as_u(self) -> Self::U {
        self.value
    }

    fn as_s(self) -> Self::S {
        self.value as Self::S
    }

    fn is_aligned(self) -> bool {
        self.value % 4 == 0
    }

    fn as_enum(self) -> DataEnum {
        DataEnum::Lword(DataLword::new(self))
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
            value: value as u64,
        }
    }

    fn from_u64(value: u64) -> Self {
        Self { value }
    }

    fn as_u(self) -> Self::U {
        self.value
    }

    fn as_s(self) -> Self::S {
        self.value as Self::S
    }

    fn is_aligned(self) -> bool {
        self.value % 8 == 0
    }

    fn as_enum(self) -> DataEnum {
        DataEnum::Dword(DataDword::new(self))
    }
}

// Marks an interpretation of a data width.
pub trait DataInterp: fmt::Debug + Clone + Copy + PartialEq {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Unsigned;
impl DataInterp for Unsigned {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Signed;
impl DataInterp for Signed {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RegData;
impl DataInterp for RegData {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ByteAddr;
impl DataInterp for ByteAddr {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DataValue<S: Data, T: DataInterp> {
    value: S,
    _phantom: PhantomData<T>,
}

// ===== Generic typestate interchange functions =====
impl<S: Data, T: DataInterp> DataValue<S, T> {
    /// Gets the raw bits in this value.
    pub fn bits(self) -> u64 {
        self.as_unsigned().raw().as_() as u64
    }

    pub fn zero() -> Self {
        Self::from_unsigned(<S as Data>::U::from_usize(0usize).unwrap())
    }

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

    pub fn new(value: S) -> Self {
        DataValue {
            value,
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

    pub fn as_reg_data(&self) -> DataValue<S, RegData> {
        DataValue::<S, RegData> {
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

// ===== Arithmetic operations =====
impl<S: Data, T: DataInterp> core::ops::Add for DataValue<S, T> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self::from_signed(
            self.as_signed()
                .raw()
                .wrapping_add(&other.as_signed().raw()),
        )
    }
}

impl<S: Data, T: DataInterp> WrappingAdd for DataValue<S, T> {
    fn wrapping_add(&self, v: &Self) -> Self {
        Self::from_signed(self.as_signed().raw().wrapping_add(&v.as_signed().raw()))
    }
}

impl<S: Data, T: DataInterp> core::ops::Sub for DataValue<S, T> {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self::from_signed(
            self.as_signed()
                .raw()
                .wrapping_sub(&other.as_signed().raw()),
        )
    }
}

impl<S: Data, T: DataInterp> WrappingSub for DataValue<S, T> {
    fn wrapping_sub(&self, v: &Self) -> Self {
        Self::from_signed(self.as_signed().raw().wrapping_sub(&v.as_signed().raw()))
    }
}

impl<S: Data, T: DataInterp> core::ops::BitAnd for DataValue<S, T> {
    type Output = Self;

    fn bitand(self, other: Self) -> Self {
        Self::from_signed(self.as_signed().raw() & other.as_signed().raw())
    }
}

impl<S: Data, T: DataInterp> core::ops::BitOr for DataValue<S, T> {
    type Output = Self;

    fn bitor(self, other: Self) -> Self {
        Self::from_signed(self.as_signed().raw() | other.as_signed().raw())
    }
}

impl<S: Data, T: DataInterp> core::ops::BitXor for DataValue<S, T> {
    type Output = Self;

    fn bitxor(self, other: Self) -> Self {
        Self::from_signed(self.as_signed().raw() ^ other.as_signed().raw())
    }
}

// ===== Mutual casts =====
// Manually unrolled to avoid conflicting From<T> for DataValue<S, T> implementations
impl<S: Data> From<DataValue<S, Unsigned>> for DataValue<S, Signed> {
    fn from(value: DataValue<S, Unsigned>) -> Self {
        Self::from_unsigned(value.as_unsigned().raw())
    }
}

impl<S: Data> From<DataValue<S, Unsigned>> for DataValue<S, RegData> {
    fn from(value: DataValue<S, Unsigned>) -> Self {
        Self::from_unsigned(value.as_unsigned().raw())
    }
}

impl<S: Data> From<DataValue<S, Unsigned>> for DataValue<S, ByteAddr> {
    fn from(value: DataValue<S, Unsigned>) -> Self {
        Self::from_unsigned(value.as_unsigned().raw())
    }
}

impl<S: Data> From<DataValue<S, Signed>> for DataValue<S, Unsigned> {
    fn from(value: DataValue<S, Signed>) -> Self {
        Self::from_unsigned(value.as_unsigned().raw())
    }
}

impl<S: Data> From<DataValue<S, Signed>> for DataValue<S, RegData> {
    fn from(value: DataValue<S, Signed>) -> Self {
        Self::from_unsigned(value.as_unsigned().raw())
    }
}

impl<S: Data> From<DataValue<S, Signed>> for DataValue<S, ByteAddr> {
    fn from(value: DataValue<S, Signed>) -> Self {
        Self::from_unsigned(value.as_unsigned().raw())
    }
}

impl<S: Data> From<DataValue<S, RegData>> for DataValue<S, Unsigned> {
    fn from(value: DataValue<S, RegData>) -> Self {
        Self::from_unsigned(value.as_unsigned().raw())
    }
}

impl<S: Data> From<DataValue<S, RegData>> for DataValue<S, Signed> {
    fn from(value: DataValue<S, RegData>) -> Self {
        Self::from_unsigned(value.as_unsigned().raw())
    }
}

impl<S: Data> From<DataValue<S, RegData>> for DataValue<S, ByteAddr> {
    fn from(value: DataValue<S, RegData>) -> Self {
        Self::from_unsigned(value.as_unsigned().raw())
    }
}

impl<S: Data> From<DataValue<S, ByteAddr>> for DataValue<S, Unsigned> {
    fn from(value: DataValue<S, ByteAddr>) -> Self {
        Self::from_unsigned(value.as_unsigned().raw())
    }
}

impl<S: Data> From<DataValue<S, ByteAddr>> for DataValue<S, Signed> {
    fn from(value: DataValue<S, ByteAddr>) -> Self {
        Self::from_unsigned(value.as_unsigned().raw())
    }
}

impl<S: Data> From<DataValue<S, ByteAddr>> for DataValue<S, RegData> {
    fn from(value: DataValue<S, ByteAddr>) -> Self {
        Self::from_unsigned(value.as_unsigned().raw())
    }
}

// ===== Interpretation differences =====
pub type UnsignedValue<S> = DataValue<S, Unsigned>;

impl<S: Data> UnsignedValue<S> {
    pub fn raw(self) -> S::U {
        self.value().as_u()
    }
}

impl<S: Data> From<usize> for UnsignedValue<S> {
    fn from(value: usize) -> Self {
        DataValue::new(S::from_u64(value as u64))
    }
}

impl<S: Data> fmt::Display for DataValue<S, Unsigned> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.as_u())
    }
}

pub type SignedValue<S> = DataValue<S, Signed>;

impl<S: Data> SignedValue<S> {
    pub fn raw(self) -> S::S {
        self.value().as_s()
    }
}

impl<S: Data> From<isize> for SignedValue<S> {
    fn from(value: isize) -> Self {
        DataValue::new(S::from_u64(value as u64))
    }
}

impl<S: Data> fmt::Display for DataValue<S, Signed> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.as_s())
    }
}

impl<S: Data> DataValue<S, RegData> {}

pub type RegValue<S> = DataValue<S, RegData>;

impl<S: Data> fmt::Display for RegValue<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.as_u())
    }
}

// === Special memory stuff ===
pub type ByteAddrValue<S> = DataValue<S, ByteAddr>;
pub type ByteAddr32 = DataValue<RS32b, ByteAddr>;
pub type ByteAddr64 = DataValue<RS64b, ByteAddr>;

impl<S: Data> ByteAddrValue<S> {
    pub fn plus_4(self) -> Self {
        Self::from_unsigned(
            self.as_unsigned()
                .raw()
                .wrapping_add(&S::U::from_usize(4).unwrap()),
        )
    }

    pub fn is_aligned_to<W: Data>(self) -> bool {
        W::from_u64(self.as_unsigned().raw().as_() as u64).is_aligned()
    }

    pub fn to_word_address(self) -> S::U {
        S::from_u64(self.as_unsigned().raw().as_() as u64 >> 2).as_u()
    }
}

impl<S: Data> fmt::Display for DataValue<S, ByteAddr> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#X}", self.value.as_u())
    }
}

// ===== Width-specific functions (includes sign extension/zero padding) =====
pub type DataByte = DataValue<RS8b, RegData>;

impl<S: AtLeast8b, T: DataInterp> DataValue<S, T> {
    pub fn zero_pad_from_byte(b: DataByte) -> Self {
        Self::from_unsigned(<S as Data>::U::from_u8(b.as_unsigned().raw()).unwrap())
    }

    pub fn sign_ext_from_byte(b: DataByte) -> Self {
        Self::from_signed(<S as Data>::S::from_i8(b.as_signed().raw()).unwrap())
    }
}

impl<T: DataInterp> From<u8> for DataValue<RS8b, T> {
    fn from(value: u8) -> Self {
        Self::from_unsigned(value)
    }
}

impl<T: DataInterp> From<DataValue<RS8b, T>> for u8 {
    fn from(value: DataValue<RS8b, T>) -> u8 {
        value.as_unsigned().raw()
    }
}

impl<T: DataInterp> From<i8> for DataValue<RS8b, T> {
    fn from(value: i8) -> Self {
        Self::from_signed(value)
    }
}

impl<T: DataInterp> From<DataValue<RS8b, T>> for i8 {
    fn from(value: DataValue<RS8b, T>) -> i8 {
        value.as_signed().raw()
    }
}

pub type DataHalf = DataValue<RS16b, RegData>;

impl<S: AtLeast16b, T: DataInterp> DataValue<S, T> {
    pub fn zero_pad_from_half(h: DataHalf) -> Self {
        Self::from_unsigned(<S as Data>::U::from_u16(h.as_unsigned().raw()).unwrap())
    }

    pub fn sign_ext_from_half(h: DataHalf) -> Self {
        Self::from_signed(<S as Data>::S::from_i16(h.as_signed().raw()).unwrap())
    }
}

impl<T: DataInterp> From<u16> for DataValue<RS16b, T> {
    fn from(value: u16) -> Self {
        Self::from_unsigned(value)
    }
}

impl<T: DataInterp> From<DataValue<RS16b, T>> for u16 {
    fn from(value: DataValue<RS16b, T>) -> u16 {
        value.as_unsigned().raw()
    }
}

impl<T: DataInterp> From<i16> for DataValue<RS16b, T> {
    fn from(value: i16) -> Self {
        Self::from_signed(value)
    }
}

impl<T: DataInterp> From<DataValue<RS16b, T>> for i16 {
    fn from(value: DataValue<RS16b, T>) -> i16 {
        value.as_signed().raw()
    }
}

impl<S: AtLeast32b> From<BitStr32> for DataValue<S, Unsigned> {
    fn from(value: BitStr32) -> Self {
        Self::zero_pad_from_lword(DataLword::from_unsigned(value.as_u32()))
    }
}

impl<S: AtLeast32b> From<BitStr32> for DataValue<S, Signed> {
    fn from(value: BitStr32) -> Self {
        Self::sign_ext_from_lword(value.to_sgn_data_word())
    }
}

pub type DataLword = DataValue<RS32b, RegData>;

impl<S: AtLeast32b, T: DataInterp> DataValue<S, T> {
    pub fn zero_pad_from_lword(l: DataLword) -> Self {
        Self::from_unsigned(<S as Data>::U::from_u32(l.as_unsigned().raw()).unwrap())
    }

    pub fn sign_ext_from_lword(l: DataLword) -> Self {
        Self::from_signed(<S as Data>::S::from_i32(l.as_signed().raw()).unwrap())
    }

    pub fn lower_lword(self) -> DataLword {
        DataLword::from_unsigned(self.as_unsigned().raw().as_() as u32)
    }

    pub fn to_bit_str(self, len: u8) -> BitStr32 {
        BitStr32::new(self.as_unsigned().raw().as_() as u32, len)
    }

    /// Returns a copy of the value with the ith byte set to val.
    pub fn set_byte(self, i: u8, val: DataByte) -> Self {
        let mask: u64 = !(0xFF << (i * 8));
        let other_raw_val: u64 = val.as_unsigned().raw().as_();
        Self::new(S::from_u64(
            (self.as_unsigned().raw().as_() as u64 & mask) | ((other_raw_val as u64) << (i * 8)),
        ))
    }

    /// Selects the ith byte in the word, where 0 is the LSB.
    pub fn get_byte(self, i: u8) -> DataByte {
        DataByte::from_unsigned((self.as_unsigned().raw() >> (i * 8).into()).as_() as u8)
    }
}

impl<T: DataInterp> From<u32> for DataValue<RS32b, T> {
    fn from(value: u32) -> Self {
        Self::from_unsigned(value)
    }
}

impl<T: DataInterp> From<DataValue<RS32b, T>> for u32 {
    fn from(value: DataValue<RS32b, T>) -> u32 {
        value.as_unsigned().raw()
    }
}

impl<T: DataInterp> From<i32> for DataValue<RS32b, T> {
    fn from(value: i32) -> Self {
        Self::from_signed(value)
    }
}

impl<T: DataInterp> From<DataValue<RS32b, T>> for i32 {
    fn from(value: DataValue<RS32b, T>) -> i32 {
        value.as_signed().raw()
    }
}

pub type DataDword = DataValue<RS64b, RegData>;

impl DataDword {
    pub fn from_lwords(lower: DataLword, upper: DataLword) -> Self {
        Self::from_unsigned(((upper.value().as_u() as u64) << 32) | (lower.value().as_u() as u64))
    }

    pub fn upper_lword(self) -> DataLword {
        DataLword::from_unsigned((self.value().as_u() as u64 >> 32) as u32)
    }
}

impl<T: DataInterp> From<u64> for DataValue<RS64b, T> {
    fn from(value: u64) -> Self {
        Self::from_unsigned(value)
    }
}

impl<T: DataInterp> From<DataValue<RS64b, T>> for u64 {
    fn from(value: DataValue<RS64b, T>) -> u64 {
        value.as_unsigned().raw()
    }
}

impl<T: DataInterp> From<DataValue<RS64b, T>> for i64 {
    fn from(value: DataValue<RS64b, T>) -> i64 {
        value.as_signed().raw()
    }
}

// All widths must implement From<i64> for immediate parsing
impl<S: Data, T: DataInterp> From<i64> for DataValue<S, T> {
    fn from(value: i64) -> Self {
        // Hack to coerce values with high MSB that num_traits won't cast
        if value as u64 > (u32::MAX as u64) {
            Self::from_signed(S::S::from_i64(value).unwrap())
        } else {
            Self::from_signed(S::S::from_i32(value as i32).unwrap())
        }
    }
}
