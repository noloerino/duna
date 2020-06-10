use crate::arch::*;
use crate::program_state::IRegister;
use crate::program_state::{InstResult, ProgramState};

pub trait ConcreteInst<R, T>
where
    R: IRegister,
    T: MachineDataWidth,
{
    fn to_machine_code(&self) -> u32;
    fn apply(&self, state: &ProgramState<R, T>) -> InstResult<R, T>;
}
