use crate::instruction::*;
use crate::isa::*;
use crate::program_state::IRegister::*;
use crate::program_state::{DataWord, ProgramState};

mod instruction;
mod isa;
mod lexer;
mod parser;
mod program_state;

fn main() {
    let instructions = [Addi::new(T0, T1, DataWord::zero())];
    let mut state = ProgramState::new();
    for inst in &instructions {
        state.apply_inst(inst);
    }
}
