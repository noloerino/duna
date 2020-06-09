use crate::arch::*;
use crate::program_state::{InstResult, ProgramState};

pub trait ConcreteInst<S>
where
    S: Architecture,
{
    fn to_machine_code(&self) -> u32;
    fn apply(&self, state: &ProgramState<S>) -> InstResult<S::Register, S::DataWidth>;
}
