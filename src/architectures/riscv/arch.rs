use super::instruction::*;
use super::parser::RiscVParser;
use super::program::*;
use super::registers::RiscVRegister;
use crate::arch::*;
use std::marker::PhantomData;

pub struct RiscV<T: MachineDataWidth> {
    _phantom: PhantomData<T>,
}

impl<T: MachineDataWidth> ArchFamily<T> for RiscV<T> {
    type Register = RiscVRegister;
    type Instruction = RiscVInst<T>;
    type Syscalls = RiscVSyscallConvention<T>;
}

pub struct RV32;

impl Architecture for RV32 {
    type DataWidth = Width32b;
    type Family = RiscV<Width32b>;
    type ProgramBehavior = RiscVProgramBehavior<Width32b>;
    type Parser = RiscVParser<Width32b>;
}

pub struct RV64;

impl Architecture for RV64 {
    type DataWidth = Width64b;
    type Family = RiscV<Width64b>;
    type ProgramBehavior = RiscVProgramBehavior<Width64b>;
    type Parser = RiscVParser<Width64b>;
}
