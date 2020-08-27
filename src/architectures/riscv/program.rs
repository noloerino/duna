use super::arch::*;
use super::registers::RiscVRegister;
use crate::arch::*;
use crate::program_state::*;
use std::collections::HashMap;
use std::marker::PhantomData;

pub struct RiscVProgramBehavior<T: MachineDataWidth> {
    _phantom: PhantomData<T>,
}

impl ProgramBehavior<RiscV<Width32b>, Width32b> for RiscVProgramBehavior<Width32b> {
    fn sp_register() -> RiscVRegister {
        RiscVRegister::SP
    }

    fn return_register() -> RiscVRegister {
        RiscVRegister::A0
    }

    fn text_start() -> ByteAddr32 {
        0x1000_0000.into()
    }

    fn stack_start() -> ByteAddr32 {
        0x7FFF_FFF0.into()
    }

    fn data_start() -> ByteAddr32 {
        0x2000_0000.into()
    }
}

impl ProgramBehavior<RiscV<Width64b>, Width64b> for RiscVProgramBehavior<Width64b> {
    fn sp_register() -> RiscVRegister {
        RiscVRegister::SP
    }

    fn return_register() -> RiscVRegister {
        RiscVRegister::A0
    }

    fn text_start() -> ByteAddr64 {
        0x1000_0000_0000_0000u64.into()
    }

    fn stack_start() -> ByteAddr64 {
        0x7FFF_FFF0_0000_0000u64.into()
    }

    fn data_start() -> ByteAddr64 {
        0x2000_0000_0000_0000u64.into()
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

pub struct RiscVSyscallConvention<T: MachineDataWidth> {
    _phantom: PhantomData<T>,
}

/// Per the RISCV calling convention (see http://man7.org/linux/man-pages/man2/syscall.2.html),
/// the a7 register determines which syscall is being performed, and the arguments are stored
/// in the argument registers of user space.
impl<T: MachineDataWidth> SyscallConvention<RiscV<T>, T> for RiscVSyscallConvention<T> {
    fn number_to_syscall(n: T::Signed) -> Option<Syscall> {
        RISCV_SYSCALL_TABLE.get(&T::sgn_to_isize(n)).cloned()
    }

    fn syscall_to_number(syscall: Syscall) -> T::RegData {
        T::isize_to_sgn(RISCV_SYSCALL_NUMBERS.get(&syscall).copied().unwrap_or(-1)).into()
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
