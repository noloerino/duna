use super::arch::*;
use super::instruction::RiscVInst;
use super::registers::RiscVRegister;
use crate::arch::*;
use crate::assembler::{Linker, ParseErrorReport, SectionStore};
use crate::instruction::*;
use crate::program_state::*;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::str;

pub struct RiscVProgram<T: MachineDataWidth> {
    pub insts: Vec<RiscVInst<T>>,
    pub state: ProgramState<RiscV<T>, T>,
}

impl RiscVProgram<Width32b> {
    pub const TEXT_START_32: u32 = 0x1000_0000;
    pub const STACK_START_32: u32 = 0x7FFF_FFF0;
    pub const DATA_START_32: u32 = 0x2000_0000;
}

impl Program<RiscV<Width32b>, Width32b> for RiscVProgram<Width32b> {
    /// Initializes a new program instance from the provided instructions.
    ///
    /// The instructions are loaded into memory at the start of the instruction section,
    /// which defaults to TEXT_START to avoid any accidental null pointer derefs.
    ///
    /// The stack pointer is initialized to STACK_START.
    ///
    /// The data given in SectionStore is used to initialize the data and rodata sections.
    ///
    /// Until paged memory is implemented, rodata is placed sequentially with data, and
    /// no guarantees on read-onliness are enforced.
    fn new(
        insts: Vec<RiscVInst<Width32b>>,
        sections: SectionStore,
        mut memory: Box<dyn Memory<ByteAddr32>>,
    ) -> RiscVProgram<Width32b> {
        let text_start: ByteAddr32 = RiscVProgram::TEXT_START_32.into();
        let stack_start: ByteAddr32 = RiscVProgram::STACK_START_32.into();
        let data_start: ByteAddr32 = RiscVProgram::DATA_START_32.into();
        // Page in text, stack, and data
        memory.map_page(text_start).unwrap();
        memory.map_page(stack_start).unwrap();
        memory.map_page(data_start).unwrap();
        let mut state = ProgramState::new(memory);
        let mut user_state = &mut state.user_state;
        // Initialize SP and PC
        user_state
            .regfile
            .set(RiscVRegister::SP, stack_start.into());
        user_state.pc = text_start;
        // store instructions
        let mut next_addr: ByteAddr32 = user_state.pc;
        for inst in &insts {
            user_state
                .memory
                .set_word(next_addr, DataWord::from(inst.to_machine_code()))
                .unwrap();
            next_addr = next_addr.plus_4()
        }
        // store data
        let all_data = sections.data.into_iter().chain(sections.rodata.into_iter());
        for (_offs, byte) in all_data.enumerate() {
            user_state.memory.set_byte(data_start, byte.into()).unwrap()
        }
        RiscVProgram { insts, state }
    }

    /// Prints out all the instructions that this program contains.
    fn dump_insts(&self) {
        for inst in &self.insts {
            println!("{:?}", inst);
        }
    }

    /// Runs the program to completion, returning the value in register a0.
    fn run(&mut self) -> i32 {
        // for now, just use the instruction vec to determine the next instruction
        let pc_start: u32 = RiscVProgram::TEXT_START_32;
        // for now, if we're out of instructions just call it a day
        // if pc dipped below pc_start, panic for now is also fine
        while let Some(inst) = self.insts.get(
            ByteAddr32::from(u32::from(self.state.user_state.pc).wrapping_sub(pc_start))
                .to_word_address() as usize,
        ) {
            if let Err(cause) = self.state.apply_inst(inst) {
                // TODO find more elegant way to set exit code
                self.state
                    .user_state
                    .regfile
                    .set(RiscVRegister::A0, cause.to_exit_code::<Width32b>().into());
                break;
            }
        }
        self.state.user_state.regfile.read(RiscVRegister::A0).into()
    }

    fn get_inst_vec(&self) -> &[RiscVInst<Width32b>] {
        self.insts.as_slice()
    }

    fn get_state(self) -> ProgramState<RiscV<Width32b>, Width32b> {
        self.state
    }
}

impl str::FromStr for RiscVProgram<Width32b> {
    type Err = ParseErrorReport;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Linker::with_main_str(s).link::<RV32>(Default::default())
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
            (57, Close)
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
