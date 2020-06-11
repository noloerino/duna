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
    type Program = RiscVProgram<Width32b>;
    type Parser = RiscVParser<Width32b>;
}
