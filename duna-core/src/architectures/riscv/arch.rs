use super::{instruction::*, parser::RiscVInstParser, program::*, registers::RiscVRegister};
use crate::{arch::*, data_structures::*};
use std::marker::PhantomData;

pub struct RiscV<S: DataWidth> {
    _phantom: PhantomData<S>,
}

impl<S: AtLeast32b> ArchFamily<S> for RiscV<S> {
    type Register = RiscVRegister;
    type Instruction = RiscVInst<S>;
    type Syscalls = RiscVSyscallConvention<S>;
}

pub struct RV32;

impl Architecture for RV32 {
    type DataWidth = W32b;
    type Family = RiscV<W32b>;
    type ProgramBehavior = RiscVProgramBehavior<W32b>;
    type InstParser = RiscVInstParser<W32b>;
}

pub struct RV64;

impl Architecture for RV64 {
    type DataWidth = W64b;
    type Family = RiscV<W64b>;
    type ProgramBehavior = RiscVProgramBehavior<W64b>;
    type InstParser = RiscVInstParser<W64b>;
}
