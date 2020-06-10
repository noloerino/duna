use super::instruction::*;
use super::parser::RiscVParser;
use super::program::*;
use super::registers::RiscVRegister;
use crate::arch::*;

/// Marker trait that allows us to make data structures applicable to both 32 and 64-bit RISCV.
pub trait RiscV: Architecture + Clone {}

#[derive(Clone)]
pub struct RV32;

impl RiscV for RV32 {}
impl Architecture for RV32 {
    type DataWidth = Width32b;
    type Register = RiscVRegister;
    type Instruction = RiscVInst<Width32b>;
    type Program = RiscVProgram<Width32b>;
    type Parser = RiscVParser<Width32b>;
}
