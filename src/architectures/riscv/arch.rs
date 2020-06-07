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

// struct DummyInst;
// impl ConcreteInst<RiscV<Width64b>, Width64b> for DummyInst {}
// struct DummyProgram;
// impl Program<RiscV<Width64b>, Width64b> for DummyProgram {}
// struct DummyParser;
// impl Parser<RiscV<Width64b>, Width64b> for DummyParser {}

// impl Architecture<Width64b> for RiscV<Width64b> {
//     type Register = RiscVRegister;
//     type Instruction = DummyInst;
//     type Program = DummyProgram;
//     type Parser = DummyParser;
// }
