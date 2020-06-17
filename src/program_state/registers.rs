use crate::arch::*;
use std::fmt;
use std::fmt::Display;
use std::marker::PhantomData;

/// Marker trait to denote an integer register.
pub trait IRegister: Copy + Clone + PartialEq + From<u8> + fmt::Debug + fmt::Display {
    /// Indexes the register file.
    /// If the result is 0, then the corresponding register value is pinned to 0.
    fn to_usize(self) -> usize;
}

const REGFILE_SIZE: usize = 32;
pub struct RegFile<R: IRegister, T: MachineDataWidth> {
    store: [T::RegData; REGFILE_SIZE],
    _phantom: PhantomData<R>,
}

impl<R: IRegister, T: MachineDataWidth> RegFile<R, T> {
    pub(in crate::program_state) fn new() -> RegFile<R, T> {
        RegFile {
            store: [T::RegData::zero(); REGFILE_SIZE],
            _phantom: PhantomData,
        }
    }

    pub fn set(&mut self, rd: R, val: T::RegData) {
        let idx = rd.to_usize();
        if idx != 0 {
            self.store[rd.to_usize()] = val;
        }
    }

    pub fn read(&self, rs: R) -> T::RegData {
        self.store[rs.to_usize()]
    }
}

/// Dumps the contents of the register file.
impl<R: IRegister, T: MachineDataWidth> Display for RegFile<R, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in 0..REGFILE_SIZE {
            let reg: R = (i as u8).into();
            writeln!(f, "{}: {}", reg, self.read(reg))?;
        }
        Ok(())
    }
}
