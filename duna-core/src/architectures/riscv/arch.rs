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

pub struct Rv32;

impl Architecture for Rv32 {
    type DataWidth = W32b;
    type Family = RiscV<W32b>;
    type ProgramBehavior = RiscVProgramBehavior<W32b>;
    type InstParser = RiscVInstParser<W32b>;
}

pub struct Rv64;

impl Architecture for Rv64 {
    type DataWidth = W64b;
    type Family = RiscV<W64b>;
    type ProgramBehavior = RiscVProgramBehavior<W64b>;
    type InstParser = RiscVInstParser<W64b>;
}
