use super::*;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum DataWidthEnum {
    Byte,
    Half,
    Lword,
    Dword,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DataEnum {
    Byte(DataByte),
    Half(DataHalf),
    Lword(DataLword),
    Dword(DataDword),
}

impl DataEnum {
    pub fn width(self) -> DataWidthEnum {
        match self {
            DataEnum::Byte(_) => DataWidthEnum::Byte,
            DataEnum::Half(_) => DataWidthEnum::Half,
            DataEnum::Lword(_) => DataWidthEnum::Lword,
            DataEnum::Dword(_) => DataWidthEnum::Dword,
        }
    }
}

impl From<DataEnum> for DataByte {
    fn from(value: DataEnum) -> Self {
        match value {
            DataEnum::Byte(b) => b,
            _ => panic!("DataByte was coerced from DataEnum of wrong width"),
        }
    }
}

impl From<DataEnum> for DataHalf {
    fn from(value: DataEnum) -> Self {
        match value {
            DataEnum::Half(h) => h,
            _ => panic!("DataHalf was coerced from DataEnum of wrong width"),
        }
    }
}

impl From<DataEnum> for DataLword {
    fn from(value: DataEnum) -> Self {
        match value {
            DataEnum::Lword(w) => w,
            _ => panic!("DataWord was coerced from DataEnum of wrong width"),
        }
    }
}

impl From<DataEnum> for DataDword {
    fn from(value: DataEnum) -> Self {
        match value {
            DataEnum::Dword(d) => d,
            _ => panic!("DataDword was coerced from DataEnum of wrong width"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct DataDiff<S: DataWidth> {
    pub old: RegValue<S>,
    pub new: RegValue<S>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DataEnumDiff {
    Byte(DataDiff<W8b>),
    Half(DataDiff<W16b>),
    Lword(DataDiff<W32b>),
    Dword(DataDiff<W64b>),
}

impl DataEnumDiff {
    pub fn old_val(self) -> DataEnum {
        use DataEnumDiff::*;
        match self {
            Byte(DataDiff { old, .. }) => DataEnum::Byte(old),
            Half(DataDiff { old, .. }) => DataEnum::Half(old),
            Lword(DataDiff { old, .. }) => DataEnum::Lword(old),
            Dword(DataDiff { old, .. }) => DataEnum::Dword(old),
        }
    }

    pub fn new_val(self) -> DataEnum {
        use DataEnumDiff::*;
        match self {
            Byte(DataDiff { new, .. }) => DataEnum::Byte(new),
            Half(DataDiff { new, .. }) => DataEnum::Half(new),
            Lword(DataDiff { new, .. }) => DataEnum::Lword(new),
            Dword(DataDiff { new, .. }) => DataEnum::Dword(new),
        }
    }
}
