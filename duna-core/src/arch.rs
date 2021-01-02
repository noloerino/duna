//! Defines a few traits needed to add support for a new architecture to duna.
use crate::{
    assembler::parser::Parser, data_structures::DataWidth, instruction::ConcreteInst,
    program_state::*,
};

/// Represents an architecture including word size, e.g. "x86-64" or "riscv-32".
pub trait Architecture: Sized {
    type DataWidth: DataWidth;
    type Family: ArchFamily<Self::DataWidth>;
    type ProgramBehavior: ProgramBehavior<Self::Family, Self::DataWidth>;
    type Parser: Parser<Self::Family, Self::DataWidth>;
}

/// Represents an architecture family parametrized over a word size, e.g. "x86" or "riscv".
pub trait ArchFamily<S: DataWidth>: Sized {
    type Register: IRegister;
    type Instruction: ConcreteInst<Self, S>;
    // TODO make SyscallConvention dynamically modifiable
    type Syscalls: SyscallConvention<Self, S>;
}
