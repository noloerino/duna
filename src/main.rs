use crate::instruction::*;
use crate::isa::*;
use crate::program_state::IRegister::*;
use crate::program_state::{ProgramState, StateChange};
use std::vec::Vec;

mod instruction;
mod isa;
mod program_state;

fn main() {
    let instructions = [Addi::new(T0, T1, 0)];
    let state = ProgramState::new();
    let mut diffs = Vec::<StateChange>::new();
    for inst in &instructions {
        diffs.push((*inst.eval)(&state))
    }
}
