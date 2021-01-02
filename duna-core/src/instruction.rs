use crate::{
    arch::*,
    data_structures::DataWidth,
    program_state::{InstResult, ProgramState},
};
use std::fmt;

pub trait ConcreteInst<F, S>: fmt::Debug + fmt::Display
where
    F: ArchFamily<S>,
    S: DataWidth,
{
    fn to_machine_code(&self) -> u32;
    fn apply(&self, state: &ProgramState<F, S>) -> InstResult<F, S>;
}
