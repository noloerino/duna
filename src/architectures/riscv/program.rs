use super::arch::*;
use super::registers::RiscVRegister;
use crate::program_state::*;
use num_traits::cast::AsPrimitive;
use std::collections::HashMap;
use std::marker::PhantomData;

pub struct RiscVProgramBehavior<S: DataWidth> {
    _phantom: PhantomData<S>,
}

impl ProgramBehavior<RiscV<RS32b>, RS32b> for RiscVProgramBehavior<RS32b> {
    fn sp_register() -> RiscVRegister {
        RiscVRegister::SP
    }

    fn return_register() -> RiscVRegister {
        RiscVRegister::A0
    }
}

impl ProgramBehavior<RiscV<RS64b>, RS64b> for RiscVProgramBehavior<RS64b> {
    fn sp_register() -> RiscVRegister {
        RiscVRegister::SP
    }

    fn return_register() -> RiscVRegister {
        RiscVRegister::A0
    }
}

lazy_static! {
    /// Syscall numbers for RV32.
    /// See https://github.com/hrw/syscalls-table/blob/master/tables/syscalls-riscv32.
    /// See https://fedora.juszkiewicz.com.pl/syscalls.html for other ISAs
    static ref RISCV_SYSCALL_TABLE: HashMap<isize, Syscall> = {
        use Syscall::*;
        [
            (63, Read),
            (64, Write),
            (53, Open),
            (57, Close),
            (93, Exit),
            (214, Brk),
            (222, Mmap),
        ]
        .iter()
        .cloned()
        .collect()
    };
    static ref RISCV_SYSCALL_NUMBERS: HashMap<Syscall, isize> =
        RISCV_SYSCALL_TABLE
        .iter()
        .map(|(n, syscall)| {(*syscall, *n)})
        .collect();
}

pub struct RiscVSyscallConvention<S: DataWidth> {
    _phantom: PhantomData<S>,
}

/// Per the RISCV calling convention (see http://man7.org/linux/man-pages/man2/syscall.2.html),
/// the a7 register determines which syscall is being performed, and the arguments are stored
/// in the argument registers of user space.
impl<S: AtLeast32b> SyscallConvention<RiscV<S>, S> for RiscVSyscallConvention<S> {
    fn number_to_syscall(n: SignedValue<S>) -> Option<Syscall> {
        RISCV_SYSCALL_TABLE.get(&(n.raw().as_() as isize)).cloned()
    }

    fn syscall_to_number(syscall: Syscall) -> RegValue<S> {
        SignedValue::<S>::from(RISCV_SYSCALL_NUMBERS.get(&syscall).copied().unwrap_or(-1)).into()
    }

    fn syscall_number_reg() -> RiscVRegister {
        RiscVRegister::A7
    }

    fn syscall_arg_regs() -> Vec<RiscVRegister> {
        use RiscVRegister::*;
        vec![A0, A1, A2, A3, A4, A5, A6]
    }

    fn syscall_return_regs() -> Vec<RiscVRegister> {
        use RiscVRegister::*;
        vec![A0, A1]
    }
}
