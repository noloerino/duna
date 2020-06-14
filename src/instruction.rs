use crate::arch::*;
use crate::program_state::{InstResult, ProgramState};
use std::fmt::Debug;

pub trait ConcreteInst<F, T>: Debug
where
    F: ArchFamily<T>,
    T: MachineDataWidth,
{
    fn to_machine_code(&self) -> u32;
    fn apply(&self, state: &ProgramState<F, T>) -> InstResult<F, T>;
}
