use super::instruction::*;
use super::parser::RiscVParser;
use super::program::*;
use super::registers::RiscVRegister;
use crate::arch::*;

pub struct RiscV;

impl Architecture for RiscV {
    type Register = RiscVRegister;
    type Instruction = RiscVInst<Width32b>;
    type Program = RiscVProgram<Width32b>;
    type Parser = RiscVParser;
}
