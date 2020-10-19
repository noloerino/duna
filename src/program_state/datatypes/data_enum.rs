use super::*;

#[derive(Copy, Clone, PartialEq)]
pub enum DataWidth {
    Byte,
    Half,
    Word,
    DoubleWord,
}

impl DataWidth {
    pub fn zero(self) -> DataEnum {
        match self {
            DataWidth::Byte => DataEnum::Byte(0u8.into()),
            DataWidth::Half => DataEnum::Half(0u16.into()),
            DataWidth::Word => DataEnum::Word(0u32.into()),
            DataWidth::DoubleWord => DataEnum::DoubleWord(0u64.into()),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum DataEnum {
    Byte(DataByte),
    Half(DataHalf),
    Word(DataWord),
    DoubleWord(DataDword),
}

impl DataEnum {
    pub fn width(self) -> DataWidth {
        match self {
            DataEnum::Byte(_) => DataWidth::Byte,
            DataEnum::Half(_) => DataWidth::Half,
            DataEnum::Word(_) => DataWidth::Word,
            DataEnum::DoubleWord(_) => DataWidth::DoubleWord,
        }
    }
}

impl From<DataEnum> for DataByte {
    fn from(value: DataEnum) -> DataByte {
        match value {
            DataEnum::Byte(b) => b,
            _ => panic!("DataByte was coerced from DataEnum of wrong width"),
        }
    }
}

impl From<DataEnum> for DataHalf {
    fn from(value: DataEnum) -> DataHalf {
        match value {
            DataEnum::Half(h) => h,
            _ => panic!("DataHalf was coerced from DataEnum of wrong width"),
        }
    }
}

impl From<DataEnum> for DataWord {
    fn from(value: DataEnum) -> DataWord {
        match value {
            DataEnum::Word(w) => w,
            _ => panic!("DataWord was coerced from DataEnum of wrong width"),
        }
    }
}

impl From<DataEnum> for DataDword {
    fn from(value: DataEnum) -> DataDword {
        match value {
            DataEnum::DoubleWord(d) => d,
            _ => panic!("DataDword was coerced from DataEnum of wrong width"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum DataEnumDiff {
    Byte { old: DataByte, new: DataByte },
    Half { old: DataHalf, new: DataHalf },
    Word { old: DataWord, new: DataWord },
    DoubleWord { old: DataDword, new: DataDword },
}

impl DataEnumDiff {
    pub fn old_val(self) -> DataEnum {
        use DataEnumDiff::*;
        match self {
            Byte { old, .. } => DataEnum::Byte(old),
            Half { old, .. } => DataEnum::Half(old),
            Word { old, .. } => DataEnum::Word(old),
            DoubleWord { old, .. } => DataEnum::DoubleWord(old),
        }
    }

    pub fn new_val(self) -> DataEnum {
        use DataEnumDiff::*;
        match self {
            Byte { new, .. } => DataEnum::Byte(new),
            Half { new, .. } => DataEnum::Half(new),
            Word { new, .. } => DataEnum::Word(new),
            DoubleWord { new, .. } => DataEnum::DoubleWord(new),
        }
    }
}

/// Marker trait for different datatypes.
pub trait Data: Copy + Clone + PartialEq {
    fn kind(self) -> DataEnum;
    fn width(self) -> DataWidth;
}
