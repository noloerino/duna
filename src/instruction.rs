use crate::arch::*;
use crate::program_state::{InstResult, ProgramState};

pub trait ConcreteInst<S, T>
where
    S: Architecture<T>,
    T: MachineDataWidth,
{
    fn to_machine_code(&self) -> u32;
    fn apply(&self, state: &ProgramState<S, T>) -> InstResult<S::Register, T>;
}
