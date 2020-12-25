use super::*;

#[derive(Copy, Clone, PartialEq)]
pub enum DataWidth {
    Byte,
    Half,
    Lword,
    Dword,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum DataEnum {
    Byte(DataByte),
    Half(DataHalf),
    Lword(DataLword),
    Dword(DataDword),
}

// impl DataEnum {
//     pub fn width(self) -> DataWidth {
//         match self {
//             DataEnum::Byte(_) => DataWidth::Byte,
//             DataEnum::Half(_) => DataWidth::Half,
//             DataEnum::Word(_) => DataWidth::Word,
//             DataEnum::DoubleWord(_) => DataWidth::DoubleWord,
//         }
//     }
// }

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

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct DataDiff<S: Data> {
    pub old: RegValue<S>,
    pub new: RegValue<S>,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum DataEnumDiff {
    Byte(DataDiff<RS8b>),
    Half(DataDiff<RS16b>),
    Lword(DataDiff<RS32b>),
    Dword(DataDiff<RS64b>),
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
