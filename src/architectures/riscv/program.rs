use super::arch::*;
use super::instruction::RiscVInst;
use super::registers::RiscVRegister;
use crate::arch::*;
use crate::assembler::{Linker, ParseErrorReport, SectionStore};
use crate::instruction::*;
use crate::program_state::*;
use num_traits::cast::{FromPrimitive, ToPrimitive};
use num_traits::ops::wrapping::WrappingSub;
use std::str;

pub struct RiscVProgram<T: MachineDataWidth> {
    pub insts: Vec<RiscVInst<T>>,
    pub state: ProgramState<RiscV<T>, T>,
}

impl<T: MachineDataWidth> RiscVProgram<T> {
    pub const TEXT_START_32: u32 = 0x1000_0000;
    pub const STACK_START_32: u32 = 0x7FFF_FFF0;
    pub const DATA_START_32: u32 = 0x2000_0000;
    pub const TEXT_START_64: u64 = 0x1000_0000_0000_0000;
    pub const STACK_START_64: u64 = 0x7FFF_FFFF_FFFF_FFF0;
    pub const DATA_START_64: u64 = 0x2000_0000_0000_0000;
}

impl<T: MachineDataWidth> Program<RiscV<T>, T> for RiscVProgram<T> {
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
    fn new(insts: Vec<RiscVInst<T>>, sections: SectionStore) -> RiscVProgram<T> {
        let mut state = ProgramState::new();
        let mut user_state = &mut state.user_state;
        let text_start: T::ByteAddr = match T::get_enum() {
            BitWidthEnum::BitWidth32 => {
                <T::Unsigned as FromPrimitive>::from_u32(RiscVProgram::<T>::TEXT_START_32)
                    .unwrap()
                    .into()
            }
            BitWidthEnum::BitWidth64 => {
                <T::Unsigned as FromPrimitive>::from_u64(RiscVProgram::<T>::TEXT_START_64)
                    .unwrap()
                    .into()
            }
        };
        let stack_start: T::ByteAddr = match T::get_enum() {
            BitWidthEnum::BitWidth32 => {
                <T::Unsigned as FromPrimitive>::from_u32(RiscVProgram::<T>::STACK_START_32)
                    .unwrap()
                    .into()
            }
            BitWidthEnum::BitWidth64 => {
                <T::Unsigned as FromPrimitive>::from_u64(RiscVProgram::<T>::STACK_START_64)
                    .unwrap()
                    .into()
            }
        };
        user_state
            .regfile
            .set(RiscVRegister::SP, stack_start.into());
        user_state.pc = text_start;
        // store instructions
        let mut next_addr: T::ByteAddr = user_state.pc;
        for inst in &insts {
            user_state.memory.set_word(
                next_addr.to_word_address(),
                DataWord::from(inst.to_machine_code()),
            );
            next_addr = next_addr.plus_4()
        }
        // store data
        let all_data = sections.data.into_iter().chain(sections.rodata.into_iter());
        for (offs, byte) in all_data.enumerate() {
            user_state.memory.set_byte(
                match T::get_enum() {
                    BitWidthEnum::BitWidth32 => {
                        <T::Unsigned as FromPrimitive>::from_u32(RiscVProgram::<T>::DATA_START_32)
                            .unwrap()
                            .into()
                    }
                    BitWidthEnum::BitWidth64 => {
                        <T::Unsigned as FromPrimitive>::from_u64(RiscVProgram::<T>::DATA_START_64)
                            .unwrap()
                            .into()
                    }
                },
                byte.into(),
            )
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
        let pc_start: T::Unsigned = match T::get_enum() {
            BitWidthEnum::BitWidth32 => {
                <T::Unsigned as FromPrimitive>::from_u32(RiscVProgram::<T>::TEXT_START_32)
                    .unwrap()
                    .into()
            }
            BitWidthEnum::BitWidth64 => {
                <T::Unsigned as FromPrimitive>::from_u64(RiscVProgram::<T>::TEXT_START_64)
                    .unwrap()
                    .into()
            }
        };
        // for now, if we're out of instructions just call it a day
        // if pc dipped below pc_start, panic for now is also fine
        while let Some(inst) = self.insts.get({
            let addr_value: T::Unsigned =
                <T::Unsigned>::from(self.state.user_state.pc).wrapping_sub(&pc_start);
            let addr = <T::ByteAddr as From<T::Unsigned>>::from(addr_value).to_word_address();
            ToPrimitive::to_usize(&addr).unwrap()
        }) {
            self.state.apply_inst(inst);
        }
        ToPrimitive::to_i32(&<T::Signed as From<T::RegData>>::from(
            self.state.user_state.regfile.read(RiscVRegister::A0),
        ))
        .unwrap()
    }
    fn get_inst_vec(&self) -> &[RiscVInst<T>] {
        self.insts.as_slice()
    }
    fn get_state(self) -> ProgramState<RiscV<T>, T> {
        self.state
    }
}

impl<T: MachineDataWidth> str::FromStr for RiscVProgram<T> {
    type Err = ParseErrorReport;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Linker::with_main_str(s).link::<RiscV<T>, T>()
    }
}
