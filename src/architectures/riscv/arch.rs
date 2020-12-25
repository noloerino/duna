use super::instruction::*;
use super::parser::RiscVParser;
use super::program::*;
use super::registers::RiscVRegister;
use crate::arch::*;
use crate::program_state::*;
use std::marker::PhantomData;

pub struct RiscV<S: Data> {
    _phantom: PhantomData<S>,
}

impl<S: AtLeast32b> ArchFamily<S> for RiscV<S> {
    type Register = RiscVRegister;
    type Instruction = RiscVInst<S>;
    type Syscalls = RiscVSyscallConvention<S>;
}

pub struct RV32;

impl Architecture for RV32 {
    type DataWidth = RS32b;
    type Family = RiscV<RS32b>;
    type ProgramBehavior = RiscVProgramBehavior<RS32b>;
    type Parser = RiscVParser<RS32b>;
}

pub struct RV64;

impl Architecture for RV64 {
    type DataWidth = RS64b;
    type Family = RiscV<RS64b>;
    type ProgramBehavior = RiscVProgramBehavior<RS64b>;
    type Parser = RiscVParser<RS64b>;
}
