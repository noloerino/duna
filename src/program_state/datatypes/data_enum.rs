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
pub enum DataEnum<T> {
    Byte(DataByte<T>),
    Half(DataHalf<T>),
    Word(DataLword<T>),
    DoubleWord(DataDword<T>),
}

impl<T> DataEnum<T> {
    pub fn width(self) -> DataWidth<T> {
        match self {
            DataEnum::Byte(_) => DataWidth::Byte,
            DataEnum::Half(_) => DataWidth::Half,
            DataEnum::Word(_) => DataWidth::Word,
            DataEnum::DoubleWord(_) => DataWidth::DoubleWord,
        }
    }
}

impl<T> From<DataEnum<T>> for DataByte<T> {
    fn from(value: DataEnum) -> Self {
        match value {
            DataEnum::Byte(b) => b,
            _ => panic!("DataByte was coerced from DataEnum of wrong width"),
        }
    }
}

impl<T> From<DataEnum<T>> for DataHalf<T> {
    fn from(value: DataEnum) -> Self {
        match value {
            DataEnum::Half(h) => h,
            _ => panic!("DataHalf was coerced from DataEnum of wrong width"),
        }
    }
}

impl<T> From<DataEnum<T>> for DataLword<T> {
    fn from(value: DataEnum) -> Self {
        match value {
            DataEnum::Word(w) => w,
            _ => panic!("DataWord was coerced from DataEnum of wrong width"),
        }
    }
}

impl<T> From<DataEnum<T>> for DataDword<T> {
    fn from(value: DataEnum) -> Self {
        match value {
            DataEnum::DoubleWord(d) => d,
            _ => panic!("DataDword was coerced from DataEnum of wrong width"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum DataEnumDiff<T> {
    Byte {
        old: DataByte<T>,
        new: DataByte<T>,
    },
    Half {
        old: DataHalf<T>,
        new: DataHalf<T>,
    },
    Word {
        old: DataLword<T>,
        new: DataLword<T>,
    },
    DoubleWord {
        old: DataDword<T>,
        new: DataDword<T>,
    },
}

impl<T> DataEnumDiff<T> {
    pub fn old_val(self) -> DataEnum<T> {
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
