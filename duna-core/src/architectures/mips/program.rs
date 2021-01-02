use super::{arch::*, registers::MipsRegister};
use crate::{data_structures::*, program_state::*};
use num_traits::cast::AsPrimitive;
use std::{collections::HashMap, marker::PhantomData};

pub struct MipsProgramBehavior<T: DataWidth> {
    _phantom: PhantomData<T>,
}

impl ProgramBehavior<Mips<W32b>, W32b> for MipsProgramBehavior<W32b> {
    fn sp_register() -> MipsRegister {
        unimplemented!()
    }

    fn return_register() -> MipsRegister {
        unimplemented!()
    }
}

lazy_static! {
    /// Syscall numbers for MIPS.
    /// See https://github.com/hrw/syscalls-table/blob/master/tables/syscalls-Mips32.
    /// See https://fedora.juszkiewicz.com.pl/syscalls.html for other ISAs
    static ref MIPS_SYSCALL_TABLE: HashMap<isize, Syscall> = {
        use Syscall::*;
        [
            (4003, Read),
            (4004, Write),
            (4005, Open),
            (4006, Close),
            (4001, Exit),
            (4045, Brk),
            (4090, Mmap),
        ]
        .iter()
        .cloned()
        .collect()
    };
    static ref MIPS_SYSCALL_NUMBERS: HashMap<Syscall, isize> =
        MIPS_SYSCALL_TABLE
        .iter()
        .map(|(n, syscall)| {(*syscall, *n)})
        .collect();
}

pub struct MipsSyscallConvention<S: DataWidth> {
    _phantom: PhantomData<S>,
}

/// Per the MIPS calling convention (see http://man7.org/linux/man-pages/man2/syscall.2.html),
/// the v0 register determines which syscall is being performed, and the arguments are stored
/// in the argument registers of user space.
impl<S: AtLeast32b> SyscallConvention<Mips<S>, S> for MipsSyscallConvention<S> {
    fn number_to_syscall(n: SignedValue<S>) -> Option<Syscall> {
        MIPS_SYSCALL_TABLE.get(&(n.raw().as_() as isize)).cloned()
    }

    fn syscall_to_number(syscall: Syscall) -> RegValue<S> {
        SignedValue::<S>::from(MIPS_SYSCALL_NUMBERS.get(&syscall).copied().unwrap_or(-1)).into()
    }

    fn syscall_number_reg() -> MipsRegister {
        unimplemented!()
    }

    fn syscall_arg_regs() -> Vec<MipsRegister> {
        unimplemented!()
    }

    fn syscall_return_regs() -> Vec<MipsRegister> {
        unimplemented!()
    }
}
