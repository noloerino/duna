use super::datatypes::*;
use super::memory::*;
pub use super::phys::*;
pub use super::priv_s::*;
use super::registers::RegFile;
pub use super::user::*;
use crate::arch::*;
use crate::assembler::{Linker, ParseErrorReport, SectionStore};
use crate::instruction::ConcreteInst;
use num_traits::cast::AsPrimitive;
use num_traits::ops::wrapping::WrappingSub;
use std::str::FromStr;

/// Defines architecture-specific behavior that defines the execution of a program.
pub trait ProgramBehavior<F, T>
where
    F: ArchFamily<T>,
    T: MachineDataWidth,
{
    /// Returns the register that holds the stack pointer.
    fn sp_register() -> F::Register;
    /// Returns the register that holds function return values.
    fn return_register() -> F::Register;
    /// Start addresses for text, stack, and data sections. TODO make these configurable
    fn text_start() -> T::ByteAddr;
    fn stack_start() -> T::ByteAddr;
    fn data_start() -> T::ByteAddr;
}

pub struct Program<A: Architecture> {
    pub insts: Vec<<A::Family as ArchFamily<A::DataWidth>>::Instruction>,
    pub state: ProgramState<A::Family, A::DataWidth>,
}

impl<A: Architecture> Program<A> {
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
    pub fn new(
        insts: Vec<<A::Family as ArchFamily<A::DataWidth>>::Instruction>,
        sections: SectionStore,
        pt: Box<dyn PageTable<<A::DataWidth as MachineDataWidth>::ByteAddr>>,
    ) -> Self {
        let text_start =
            <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::text_start();
        let stack_start =
            <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::stack_start();
        let data_start =
            <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::data_start();
        // Page in text, stack, and data
        pt.map_page(text_start).unwrap();
        pt.map_page(stack_start).unwrap();
        pt.map_page(data_start).unwrap();
        let mut state = ProgramState::new(pt);
        let mut user_state = &mut state.user_state;
        let sp = <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::sp_register();
        // Initialize SP and PC
        user_state.regfile.set(sp, stack_start.into());
        user_state.pc = text_start;
        // store instructions
        let mut next_addr: <A::DataWidth as MachineDataWidth>::ByteAddr = user_state.pc;
        for inst in &insts {
            state
                .memory_set(
                    next_addr,
                    DataEnum::Word(DataWord::from(inst.to_machine_code())),
                )
                .unwrap();
            next_addr = next_addr.plus_4()
        }
        // store data
        let all_data = sections.data.into_iter().chain(sections.rodata.into_iter());
        let data_start_usize = <A::DataWidth as MachineDataWidth>::usgn_to_usize(data_start.into());
        for (offs, byte) in all_data.enumerate() {
            let addr: <A::DataWidth as MachineDataWidth>::ByteAddr =
                <A::DataWidth as MachineDataWidth>::usize_to_usgn(data_start_usize + offs).into();
            state.memory_set(addr, DataEnum::Byte(byte.into())).unwrap();
        }
        Program { insts, state }
    }

    /// Prints out all the instructions that this program contains.
    pub fn dump_insts(&self) {
        for inst in &self.insts {
            println!("{:?}", inst);
        }
    }

    /// Runs the program to completion, returning an exit code.
    /// If the program was terminated abnormally, the upper bit of the u8 will be set.
    /// The lower 7 bits are the value passed to the exit handler (or the default register for the
    /// first argument of a syscall if exit is not explicitly invoked), and will be truncated.
    pub fn run(&mut self) -> u8 {
        ProgramExecutor::run(self)
    }

    /// Returns the instructions provided to this program.
    pub fn get_inst_vec(&self) -> &[<A::Family as ArchFamily<A::DataWidth>>::Instruction] {
        self.insts.as_slice()
    }

    /// Returns the current state of this program.
    pub fn get_state(self) -> ProgramState<A::Family, A::DataWidth> {
        self.state
    }

    fn get_pc_word_index(&self) -> usize {
        let pc_start: <A::DataWidth as MachineDataWidth>::ByteAddr =
            <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::text_start();
        // all this logic calculates the next address (very verbose due to generic types)
        let curr_pc: <A::DataWidth as MachineDataWidth>::Unsigned = self.state.user_state.pc.into();
        let orig_pc: <A::DataWidth as MachineDataWidth>::Unsigned = pc_start.into();
        let offs: <A::DataWidth as MachineDataWidth>::ByteAddr =
            curr_pc.wrapping_sub(&orig_pc).into();
        let idx: usize = offs.to_word_address().as_();
        idx
    }
}

impl<A: Architecture> FromStr for Program<A> {
    type Err = ParseErrorReport;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Linker::with_main_str(s).link::<A>(Default::default())
    }
}

pub struct ProgramExecutor<A: Architecture> {
    pub program: Program<A>,
    /// Represents the history of instructions executed by the program.
    /// The diffs in each instruction should be executed in sequence.
    pub inst_stack: Vec<InstResult<A::Family, A::DataWidth>>,
}

impl<A: Architecture> ProgramExecutor<A> {
    pub fn new(program: Program<A>) -> Self {
        ProgramExecutor {
            program,
            inst_stack: Vec::new(),
        }
    }

    /// Runs the next instruction of the program.
    /// Returns the exit code if the program terminates, and None otherwise.
    pub fn step(&mut self) -> Option<u8> {
        let program = &mut self.program;
        // TODO gracefully handle out of bounds instructions
        // for now, if we reach an oob instruction just report the return value
        if let Some(inst) = program.insts.get(program.get_pc_word_index()) {
            let exec_result = program.state.apply_inst(&inst);
            match exec_result {
                Ok(inst_result) => {
                    self.inst_stack.push(inst_result);
                    None
                }
                Err(cause) => Some(cause.handle_exit(&mut program.state)),
            }
        } else {
            let a0 =
                <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::return_register();
            let a0_val: <A::DataWidth as MachineDataWidth>::Unsigned =
                program.state.regfile_read(a0).into();
            // For a non-abnormal exit, downcast to u8 and set upper bit to 0
            let val_u8 = a0_val.as_();
            Some(val_u8 & 0b0111_1111)
        }
    }

    /// Reverts one step of exeuction. Returns None if there are no steps to revert.
    pub fn revert(&mut self) -> Option<()> {
        let inst_result = self.inst_stack.pop()?;
        for diff in inst_result.diffs.iter().rev() {
            self.program.state.revert_diff(&diff);
        }
        Some(())
    }

    /// Attempts to run the program to completion. If a more than timeout cycles were run after the
    /// invocation of this method, then the function will return None.
    pub fn step_to_completion(&mut self, timeout: usize) -> Option<u8> {
        for _ in 0..timeout {
            if let Some(code) = self.step() {
                return Some(code);
            }
        }
        None
    }

    /// Runs the program to completion, returning an exit code.
    /// Hangs on an infinite loop.
    pub fn run(program: &mut Program<A>) -> u8 {
        // for now, just use the instruction vec to determine the next instruction
        // for now, if we're out of instructions just call it a day
        // if pc dipped below pc_start, panic for now is also fine
        while let Some(inst) = program.insts.get(program.get_pc_word_index()) {
            if let Err(cause) = program.state.apply_inst(inst) {
                return cause.handle_exit(&mut program.state);
            }
        }
        let a0 =
            <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::return_register();
        let a0_val: <A::DataWidth as MachineDataWidth>::Unsigned =
            program.state.regfile_read(a0).into();
        // For a non-abnormal exit, downcast to u8 and set upper bit to 0
        let val_u8 = a0_val.as_();
        val_u8 & 0b0111_1111
    }
}

pub struct ProgramState<F: ArchFamily<T>, T: MachineDataWidth> {
    pub(crate) user_state: UserState<F, T>,
    pub(crate) priv_state: PrivState<T>,
    pub(crate) phys_state: PhysState,
}

impl<F: ArchFamily<T>, T: MachineDataWidth> Default for ProgramState<F, T> {
    fn default() -> Self {
        ProgramState::new(Box::new(AllMappedPt::new()))
    }
}

/// TODO put custom types for syscall args
/// TODO put errno on user state at a thread-local statically known location
impl<F: ArchFamily<T>, T: MachineDataWidth> ProgramState<F, T> {
    pub fn get_stdout(&self) -> &[u8] {
        self.priv_state.stdout.as_slice()
    }

    pub fn get_stderr(&self) -> &[u8] {
        self.priv_state.stderr.as_slice()
    }

    pub fn write_stderr(&mut self, string: &str) {
        self.priv_state.stderr.extend(string.as_bytes());
        eprint!("{}", string);
    }

    pub fn get_user_pc(&self) -> T::ByteAddr {
        self.user_state.pc
    }

    pub fn set_user_pc(&mut self, addr: T::ByteAddr) {
        self.user_state.pc = addr
    }

    pub fn regfile_read(&self, reg: F::Register) -> T::RegData {
        self.user_state.regfile.read(reg)
    }

    pub fn regfile_set(&mut self, reg: F::Register, val: T::RegData) {
        self.user_state.regfile.set(reg, val);
    }

    pub fn regfile(&mut self) -> &RegFile<F::Register, T> {
        &self.user_state.regfile
    }

    pub fn get_pc(&mut self) -> T::ByteAddr {
        self.user_state.pc
    }

    /// Performs a read from memory with the specified data width.
    /// Returns the sequence of state updates on success, or a page fault on failure.
    pub fn memory_get(
        &self,
        vaddr: T::ByteAddr,
        width: DataWidth,
    ) -> Result<(DataEnum, InstResult<F, T>), MemFault<T::ByteAddr>> {
        // TODO how do we handle lookups spanning multiple pages?
        let PteLookupData {
            diffs: pt_diffs,
            ppn,
            offs,
        } = self.priv_state.page_table.lookup_page(vaddr)?;
        let diffs: Vec<StateDiff<F, T>> = pt_diffs
            .into_iter()
            .map(PteUpdate::into_state_diff)
            .collect();
        Ok((
            self.phys_state.memory_get(ppn, offs, width),
            InstResult::new(diffs),
        ))
    }

    /// Performs a read to memory with the specified data.
    /// Returns the sequence of state updates on success, or a page fault on failure.
    pub fn memory_set(
        &self,
        vaddr: T::ByteAddr,
        data: DataEnum,
    ) -> Result<InstResult<F, T>, MemFault<T::ByteAddr>> {
        // TODO how do we handle lookups spanning multiple pages?
        let PteLookupData {
            diffs: pt_diffs,
            ppn,
            offs,
        } = self.priv_state.page_table.lookup_page(vaddr)?;
        let mut diffs: Vec<StateDiff<F, T>> = pt_diffs
            .into_iter()
            .map(PteUpdate::into_state_diff)
            .collect();
        diffs.push(
            self.phys_state
                .memory_set(ppn, offs, data)
                .into_state_diff(),
        );
        Ok(InstResult::new(diffs))
    }

    /// Used to inspect memory. Any page table updates will not be performed.
    pub fn memory_inspect_word(&self, addr: T::ByteAddr) -> DataWord {
        let (v, _diffs) = self.memory_get(addr, DataWidth::Word).unwrap();
        v.into()
    }

    /// Used for testing only. Any operations applied in this fashion are noninvertible, as the
    /// history stack only keeps track of full instructions.
    #[cfg(test)]
    pub fn memory_get_word(&mut self, addr: T::ByteAddr) -> DataWord {
        let (v, diffs) = self.memory_get(addr, DataWidth::Word).unwrap();
        self.apply_inst_result(diffs).unwrap();
        v.into()
    }

    #[cfg(test)]
    pub fn memory_set_word(&mut self, addr: T::ByteAddr, val: DataWord) {
        self.apply_inst_result(self.memory_set(addr, val.kind()).unwrap())
            .unwrap();
    }

    #[cfg(test)]
    pub fn memory_get_doubleword(&mut self, addr: T::ByteAddr) -> DataDword {
        let (v, diffs) = self.memory_get(addr, DataWidth::DoubleWord).unwrap();
        self.apply_inst_result(diffs).unwrap();
        v.into()
    }

    #[cfg(test)]
    pub fn memory_set_doubleword(&mut self, addr: T::ByteAddr, val: DataDword) {
        self.apply_inst_result(self.memory_set(addr, val.kind()).unwrap())
            .unwrap();
    }

    pub fn handle_trap(&self, trap_kind: &TrapKind<T::ByteAddr>) -> InstResult<F, T> {
        match trap_kind {
            TrapKind::Ecall => self.dispatch_syscall(),
            TrapKind::MemFault(MemFault {
                user_vaddr: _,
                cause,
            }) => match cause {
                // even though the OS could attempt to map the page,
                // we requite the user to manually call brk/sbrk/mmap etc.
                MemFaultCause::PageFault => PrivDiff::Terminate(TermCause::SegFault),
                MemFaultCause::SegFault => PrivDiff::Terminate(TermCause::SegFault),
                MemFaultCause::BusError => PrivDiff::Terminate(TermCause::BusError),
            }
            .into_inst_result(),
        }
    }

    pub fn dispatch_syscall(&self) -> InstResult<F, T> {
        let rf = &self.user_state.regfile;
        let syscall_number_reg = <F::Syscalls as SyscallConvention<F, T>>::syscall_number_reg();
        let arg_regs = <F::Syscalls as SyscallConvention<F, T>>::syscall_arg_regs();
        let a0 = rf.read(arg_regs[0]);
        let a1 = rf.read(arg_regs[1]);
        let a2 = rf.read(arg_regs[2]);
        if let Some(nr) = <F::Syscalls as SyscallConvention<F, T>>::number_to_syscall(
            self.user_state.regfile.read(syscall_number_reg).into(),
        ) {
            match nr {
                Syscall::Write => self.syscall_write(a0, a1.into(), a2),
                Syscall::Exit => self.syscall_exit(a0),
                _ => unimplemented!(),
            }
        } else {
            self.syscall_unknown()
        }
    }

    /// Writes contents to a specified file descriptor.
    /// TODO for now, this is hardcoded to print to stdout regardless of the provided FD.
    /// * fd - file descriptor
    /// * buf - pointer to the buffer to be written
    /// * len - the number of bytes to write
    fn syscall_write(&self, fd: T::RegData, buf: T::ByteAddr, len: T::RegData) -> InstResult<F, T> {
        let len_val: T::Unsigned = len.into();
        let count: usize = T::usgn_to_usize(len_val);
        let base_addr: T::Unsigned = buf.into();
        let ret_reg = <F::Syscalls as SyscallConvention<F, T>>::syscall_return_regs()[0];
        let mut v = Vec::new();
        let bytes: Vec<u8> = (0..count)
            .map(|i| {
                u8::from({
                    let (val, inst_result) = self
                        .memory_get((base_addr + T::usize_to_usgn(i)).into(), DataWidth::Byte)
                        .unwrap();
                    let byte: DataByte = val.into();
                    v.extend(inst_result.diffs);
                    byte
                })
            })
            .collect();
        // Awkward here, but changing type sig makes common case less ergonomic
        v.push(PrivDiff::FileWrite { fd, data: bytes }.into_state_diff());
        v.extend(UserDiff::reg_write_pc_p4(&self.user_state, ret_reg, len).diffs);
        InstResult::new(v)
    }

    /// Exits the program with the provided 32-bit code.
    /// Note that the shell will only see the lower 7-bits.
    fn syscall_exit(&self, code: T::RegData) -> InstResult<F, T> {
        // downcast to u32 no matter what
        let val: T::Unsigned = code.into();
        PrivDiff::Terminate(TermCause::Exit(T::usgn_to_usize(val) as u32)).into_inst_result()
    }

    /// Handles an unknown syscall.
    fn syscall_unknown(&self) -> InstResult<F, T> {
        panic!("Unknown syscall")
    }

    pub fn new(pt: Box<dyn PageTable<T::ByteAddr>>) -> ProgramState<F, T> {
        ProgramState {
            user_state: UserState::new(),
            priv_state: PrivState::new(pt),
            // TODO make configurable
            phys_state: Default::default(),
        }
    }

    pub fn apply_inst(&mut self, inst: &F::Instruction) -> Result<InstResult<F, T>, TermCause> {
        self.apply_inst_result(inst.apply(self))
    }

    /// Asserts that applying the instruction does not fail.
    #[cfg(test)]
    pub fn apply_inst_test(&mut self, inst: &F::Instruction) {
        self.apply_inst_result(inst.apply(self)).unwrap();
    }

    /// Performs the provided instruction. Returns the applied instruction for ownership reasons.
    /// TODO maybe there's a bug here because if the inst terminates, what happens to the
    /// popped instresult?
    pub fn apply_inst_result(
        &mut self,
        inst_result: InstResult<F, T>,
    ) -> Result<InstResult<F, T>, TermCause> {
        for diff in &inst_result.diffs {
            self.apply_diff(&diff)?;
        }
        Ok(inst_result)
    }

    pub fn apply_diff(&mut self, diff: &StateDiff<F, T>) -> Result<(), TermCause> {
        match diff {
            StateDiff::User(u) => {
                self.user_state.apply_diff(u);
                Ok(())
            }
            StateDiff::Priv(p) => self.priv_state.apply_diff::<F>(p),
            StateDiff::Phys(p) => {
                self.phys_state.apply_diff(p);
                Ok(())
            }
        }
    }

    /// Reverts the described operation.
    pub fn revert_diff(&mut self, diff: &StateDiff<F, T>) {
        match diff {
            StateDiff::User(u) => self.user_state.revert_diff(u),
            StateDiff::Priv(p) => self.priv_state.revert_diff::<F>(p),
            StateDiff::Phys(p) => self.phys_state.revert_diff(p),
        }
    }
}

pub trait SyscallConvention<F: ArchFamily<T>, T: MachineDataWidth> {
    /// Returns the syscall identified by number N, or none if no such syscall exists.
    fn number_to_syscall(n: T::Signed) -> Option<Syscall>;
    /// Returns the number corresponding to the syscall, or -1 if it is unimplemented.
    fn syscall_to_number(syscall: Syscall) -> T::RegData;
    /// Returns which register is used to pass the syscall number.
    fn syscall_number_reg() -> F::Register;
    /// Returns which registers are used to pass arguments to syscalls.
    fn syscall_arg_regs() -> Vec<F::Register>;
    /// Returns which registers are used to return arguments from syscalls.
    fn syscall_return_regs() -> Vec<F::Register>;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Syscall {
    Read,
    Write,
    Open,
    Close,
    Exit,
    Brk,
    Mmap,
}

#[derive(Copy, Clone)]
pub struct RegDataChange<T: MachineDataWidth> {
    pub old_value: T::RegData,
    pub new_value: T::RegData,
}

/// Represents the result of an instruction in terms of its actions on the machine state.
pub struct InstResult<F: ArchFamily<T>, T: MachineDataWidth> {
    pub diffs: Vec<StateDiff<F, T>>,
}

impl<F: ArchFamily<T>, T: MachineDataWidth> InstResult<F, T> {
    pub fn new(diffs: Vec<StateDiff<F, T>>) -> Self {
        InstResult { diffs }
    }

    pub fn add(mut self, diff: StateDiff<F, T>) {
        self.diffs.push(diff);
    }
}

/// Represents an individual atomic change in the state of the machine.
///
/// These diffs occur at one of three levels: user, OS/kernel, and hardware.
pub enum StateDiff<F: ArchFamily<T>, T: MachineDataWidth> {
    User(UserDiff<F, T>),
    Priv(PrivDiff<T>),
    Phys(PhysDiff),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::architectures::riscv::RiscVRegister::*;
    use crate::architectures::riscv::RV32;

    /// Makes sure the executor can step and revert instructions.
    #[test]
    fn test_executor() {
        let code = "
            addi a0, zero, 4
            addi a1, zero, 2
            addi a0, zero, 16
            ";
        let mut executor = ProgramExecutor::<RV32>::new(code.parse::<Program<RV32>>().unwrap());
        // after one operation
        assert_eq!(executor.step(), None);
        assert_eq!(executor.program.state.regfile_read(A0), 4u32.into());
        assert_eq!(executor.program.state.regfile_read(A1), 0u32.into());
        // after two operations
        assert_eq!(executor.step(), None);
        assert_eq!(executor.program.state.regfile_read(A0), 4u32.into());
        assert_eq!(executor.program.state.regfile_read(A1), 2u32.into());
        // rewind once
        assert_eq!(executor.revert(), Some(()));
        assert_eq!(executor.program.state.regfile_read(A0), 4u32.into());
        assert_eq!(executor.program.state.regfile_read(A1), 0u32.into());
        // rewind again
        assert_eq!(executor.revert(), Some(()));
        assert_eq!(executor.program.state.regfile_read(A0), 0u32.into());
        assert_eq!(executor.program.state.regfile_read(A1), 0u32.into());
        // next rewind should fail
        assert_eq!(executor.revert(), None);
        // step 2
        assert_eq!(executor.step_to_completion(2), None);
        assert_eq!(executor.program.state.regfile_read(A0), 4u32.into());
        assert_eq!(executor.program.state.regfile_read(A1), 2u32.into());
        // step once more
        assert_eq!(executor.step(), None);
        assert_eq!(executor.program.state.regfile_read(A0), 16u32.into());
        assert_eq!(executor.program.state.regfile_read(A1), 2u32.into());
        // final step should cause a termination
        assert_eq!(executor.step(), Some(16));
        // reverting after termination should revert the last instruction, not the termination
        assert_eq!(executor.revert(), Some(()));
        assert_eq!(executor.program.state.regfile_read(A0), 4u32.into());
        assert_eq!(executor.program.state.regfile_read(A1), 2u32.into());
    }
}
