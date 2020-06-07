use super::instruction::*;
use super::parser::RiscVParser;
use super::program::*;
use super::registers::RiscVRegister;
use crate::arch::*;
use std::marker::PhantomData;

pub struct RiscV<T: MachineDataWidth> {
    _phantom: PhantomData<T>,
}

impl<T: MachineDataWidth> Architecture<T> for RiscV<T> {
    type Register = RiscVRegister;
    type Instruction = RiscVInst<T>;
    type Program = RiscVProgram<T>;
    type Parser = RiscVParser<T>;
}
