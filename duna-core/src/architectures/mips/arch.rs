use super::{instruction::*, parser::MipsParser, program::*, registers::MipsRegister};
use crate::{arch::*, data_structures::*};
use std::marker::PhantomData;

pub struct Mips<S: DataWidth> {
    _phantom: PhantomData<S>,
}

impl<S: AtLeast32b> ArchFamily<S> for Mips<S> {
    type Register = MipsRegister;
    type Instruction = MipsInst<S>;
    type Syscalls = MipsSyscallConvention<S>;
}

pub struct Mips32;

impl Architecture for Mips32 {
    type DataWidth = W32b;
    type Family = Mips<W32b>;
    type ProgramBehavior = MipsProgramBehavior<W32b>;
    type Parser = MipsParser<W32b>;
}
