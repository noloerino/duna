use super::datatypes::*;
use super::memory::*;
use super::registers::{IRegister, RegFile};
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
        mut memory: Box<dyn Memory<<A::DataWidth as MachineDataWidth>::ByteAddr>>,
    ) -> Self {
        let text_start =
            <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::text_start();
        let stack_start =
            <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::stack_start();
        let data_start =
            <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::data_start();
        // Page in text, stack, and data
        memory.map_page(text_start).unwrap();
        memory.map_page(stack_start).unwrap();
        memory.map_page(data_start).unwrap();
        let mut state = ProgramState::new(memory);
        let mut user_state = &mut state.user_state;
        let sp = <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::sp_register();
        // Initialize SP and PC
        user_state.regfile.set(sp, stack_start.into());
        user_state.pc = text_start;
        // store instructions
        let mut next_addr: <A::DataWidth as MachineDataWidth>::ByteAddr = user_state.pc;
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
    pub diff_stack: Vec<ProgramDiff<A::Family, A::DataWidth>>,
}

impl<A: Architecture> ProgramExecutor<A> {
    pub fn new(program: Program<A>) -> Self {
        ProgramExecutor {
            program,
            diff_stack: Vec::new(),
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
                Ok(diffs) => {
                    self.diff_stack.extend(diffs);
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
        let diff = self.diff_stack.pop()?;
        self.program.state.revert_diff(&diff);
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
    pub(crate) priv_state: PrivProgState,
    pub(crate) user_state: UserProgState<F, T>,
}

impl<F: ArchFamily<T>, T: MachineDataWidth> Default for ProgramState<F, T> {
    fn default() -> Self {
        ProgramState::new(Box::new(SimpleMemory::new()))
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

    pub fn set_user_pc(&mut self, addr: <T>::ByteAddr) {
        self.user_state.pc = addr
    }

    pub fn regfile_read(&self, reg: F::Register) -> <T>::RegData {
        self.user_state.regfile.read(reg)
    }

    pub fn regfile_set(&mut self, reg: F::Register, val: <T>::RegData) {
        self.user_state.regfile.set(reg, val);
    }

    #[cfg(test)]
    pub fn memory_get_word(&self, addr: T::ByteAddr) -> DataWord {
        self.user_state.memory.get_word(addr).unwrap()
    }

    #[cfg(test)]
    pub fn memory_set_word(&mut self, addr: T::ByteAddr, val: DataWord) {
        self.user_state.memory.set_word(addr, val).unwrap()
    }

    pub fn handle_trap(&self, trap_kind: &TrapKind<T::ByteAddr>) -> PrivStateChange<T> {
        match trap_kind {
            TrapKind::Ecall => self.dispatch_syscall(),
            TrapKind::MemFault(MemFault {
                user_vaddr: _,
                cause,
            }) => match cause {
                // even though the OS could attempt to map the page,
                // we requite the user to manually call brk/sbrk/mmap etc.
                MemFaultCause::PageFault => PrivStateChange::Terminate(TermCause::SegFault),
                MemFaultCause::SegFault => PrivStateChange::Terminate(TermCause::SegFault),
                MemFaultCause::BusError => PrivStateChange::Terminate(TermCause::BusError),
            },
        }
    }

    pub fn dispatch_syscall(&self) -> PrivStateChange<T> {
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
                Syscall::Brk => self.syscall_brk(a0.into()),
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
    fn syscall_write(
        &self,
        fd: T::RegData,
        buf: T::ByteAddr,
        len: T::RegData,
    ) -> PrivStateChange<T> {
        PrivStateChange::FileWrite { fd, buf, len }
    }

    /// Attempts to move the "program break" up to the indicated location.
    /// For us, this means the OS will attempt to page in memory up to the designated address.
    /// TODO unmap pages if brk goes down
    /// TODO initialize the brk pointer on program state
    /// * addr - the
    fn syscall_brk(&self, addr: T::ByteAddr) -> PrivStateChange<T> {
        PrivStateChange::TryPageIn { addr }
    }

    /// Exits the program with the provided 32-bit code.
    /// Note that the shell will only see the lower 7-bits.
    fn syscall_exit(&self, code: T::RegData) -> PrivStateChange<T> {
        // downcast to u32 no matter what
        let val: T::Unsigned = code.into();
        PrivStateChange::Terminate(TermCause::Exit(T::usgn_to_usize(val) as u32))
    }

    /// Handles an unknown syscall.
    fn syscall_unknown(&self) -> PrivStateChange<T> {
        panic!("Unknown syscall")
    }

    pub fn new(memory: Box<dyn Memory<T::ByteAddr>>) -> ProgramState<F, T> {
        ProgramState {
            priv_state: PrivProgState::new(),
            user_state: UserProgState::new(memory),
        }
    }

    pub fn apply_inst(
        &mut self,
        inst: &F::Instruction,
    ) -> Result<Vec<ProgramDiff<F, T>>, TermCause> {
        self.apply_inst_result(inst.apply(self))
    }

    /// Asserts that applying the instruction does not fail.
    #[cfg(test)]
    pub fn apply_inst_test(&mut self, inst: &F::Instruction) {
        self.apply_inst_result(inst.apply(self)).unwrap();
    }

    /// Performs the described operation.
    /// The privileged operation is applied first, followed by the user operation.
    /// If the diff terminates the program, the TermCause is returned. Otherwise, it just returns
    /// the program diff(s) produced in the order that they were applied.
    pub fn apply_inst_result(
        &mut self,
        diff: InstResult<F, T>,
    ) -> Result<Vec<ProgramDiff<F, T>>, TermCause> {
        match diff {
            InstResult::Trap(trap_kind) => {
                let priv_diff = self.handle_trap(&trap_kind);
                // In the event of termination, nothing happens in userland.
                let user_diff = self
                    .priv_state
                    .apply_diff(&mut self.user_state, &priv_diff)?;
                self.user_state.apply_diff(&user_diff);
                Ok(vec![
                    ProgramDiff::PrivOnly(priv_diff),
                    ProgramDiff::UserOnly(user_diff),
                ])
            }
            InstResult::UserStateChange(user_diff) => {
                self.user_state.apply_diff(&user_diff);
                Ok(vec![ProgramDiff::UserOnly(user_diff)])
            }
        }
    }

    /// Reverts the described operation.
    /// Since the privileged diff is applied first during execution, the user diff should
    /// be applied first during a revert.
    /// TODO figure out how to implement that...
    pub fn revert_diff(&mut self, diff: &ProgramDiff<F, T>) {
        match diff {
            ProgramDiff::UserOnly(user_only) => self.user_state.revert_diff(user_only),
            ProgramDiff::PrivOnly(priv_only) => {
                self.priv_state.revert_diff(&self.user_state, priv_only)
            }
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

/// Contains program state that is visited only to privileged entities, i.e. a kernel thread.
/// TODO add kernel thread information (tid, file descriptors, etc.)
pub struct PrivProgState {
    /// Holds the contents of all bytes that have been printed to stdout (used mostly for testing)
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    // file_descriptors: Vec<Vec<u8>>
}

impl Default for PrivProgState {
    fn default() -> Self {
        PrivProgState::new()
    }
}

impl PrivProgState {
    pub fn new() -> PrivProgState {
        PrivProgState {
            stdout: Vec::new(),
            stderr: Vec::new(),
        }
    }

    pub fn apply_diff<F: ArchFamily<T>, T: MachineDataWidth>(
        &mut self,
        user_state: &mut UserProgState<F, T>,
        diff: &PrivStateChange<T>,
    ) -> Result<UserDiff<F, T>, TermCause> {
        use PrivStateChange::*;
        let ret_reg = <F::Syscalls as SyscallConvention<F, T>>::syscall_return_regs()[0];
        match diff {
            NoChange => Ok(UserDiff::noop(user_state)),
            FileWrite { fd, buf, len } => {
                let memory = &user_state.memory;
                let len_val: T::Unsigned = (*len).into();
                let count: usize = T::usgn_to_usize(len_val);
                let base_addr: T::Unsigned = (*buf).into();
                let bytes: Vec<u8> = (0..count)
                    .map(|i| {
                        u8::from(
                            memory
                                .get_byte((base_addr + T::usize_to_usgn(i)).into())
                                .unwrap(),
                        )
                    })
                    .collect();
                // TODO impl for other files
                let fd_idx: usize = {
                    let num: T::Unsigned = (*fd).into();
                    T::usgn_to_usize(num)
                };
                match fd_idx {
                    1 => {
                        print!("{}", String::from_utf8_lossy(&bytes));
                        self.stdout.extend(bytes);
                    }
                    2 => {
                        eprint!("{}", String::from_utf8_lossy(&bytes));
                        self.stderr.extend(bytes);
                    }
                    _ => unimplemented!(),
                }
                Ok(UserDiff::reg_write_pc_p4(user_state, ret_reg, *len))
            }
            TryPageIn { addr } => {
                user_state
                    .memory
                    .map_page(*addr)
                    .map_err(|fault| fault.into()) // converts into termcause
                    .map(|_| UserDiff::reg_write_pc_p4(user_state, ret_reg, 0.into()))
            }
            Terminate(cause) => Err(*cause),
        }
    }

    /// Reverts a privileged state change.
    /// The originally produced UserOnly diff MUST have already been applied.
    pub fn revert_diff<F: ArchFamily<T>, T: MachineDataWidth>(
        &mut self,
        _user_state: &UserProgState<F, T>,
        diff: &PrivStateChange<T>,
    ) {
        use PrivStateChange::*;
        match diff {
            NoChange => {}
            // TODO delete last len bytes from fd
            FileWrite {
                fd: _,
                buf: _,
                len: _,
            } => {}
            _ => unimplemented!(),
        }
    }
}

/// Contains program state that is visible to the user.
pub struct UserProgState<F: ArchFamily<T>, T: MachineDataWidth> {
    pub pc: T::ByteAddr,
    pub regfile: RegFile<F::Register, T>,
    pub memory: Box<dyn Memory<T::ByteAddr>>,
}

#[cfg(test)]
impl<F: ArchFamily<T>, T: MachineDataWidth> Default for UserProgState<F, T> {
    fn default() -> Self {
        UserProgState::new(Box::new(SimpleMemory::new()))
    }
}

impl<F: ArchFamily<T>, T: MachineDataWidth> UserProgState<F, T> {
    pub fn new(memory: Box<dyn Memory<T::ByteAddr>>) -> UserProgState<F, T> {
        UserProgState {
            pc: T::sgn_zero().into(),
            regfile: RegFile::new(),
            memory,
        }
    }

    pub fn apply_diff(&mut self, diff: &UserDiff<F, T>) {
        self.pc = diff.pc.new_pc;
        if let Some(RegDiff {
            reg,
            val: RegDataChange { new_value, .. },
        }) = diff.reg
        {
            self.regfile.set(reg, new_value);
        }
        if let Some(MemDiff { addr, new_val, .. }) = diff.mem {
            self.memory.set(addr, new_val).unwrap();
        }
    }

    pub fn revert_diff(&mut self, diff: &UserDiff<F, T>) {
        self.pc = diff.pc.old_pc;
        if let Some(RegDiff {
            reg,
            val: RegDataChange { old_value, .. },
        }) = diff.reg
        {
            self.regfile.set(reg, old_value);
        }
        if let Some(MemDiff { addr, old_val, .. }) = diff.mem {
            self.memory.set(addr, old_val).unwrap();
        }
    }
}

#[derive(Copy, Clone)]
struct RegDataChange<T: MachineDataWidth> {
    old_value: T::RegData,
    new_value: T::RegData,
}

/// A change to the program counter.
struct PcDiff<T: MachineDataWidth> {
    old_pc: T::ByteAddr,
    new_pc: T::ByteAddr,
}

/// A change to a register.
struct RegDiff<R: IRegister, T: MachineDataWidth> {
    reg: R,
    val: RegDataChange<T>,
}

/// A change to memory. old_val and new_val must have the same width.
struct MemDiff<T: MachineDataWidth> {
    addr: T::ByteAddr,
    old_val: DataEnum,
    new_val: DataEnum,
}

/// Represents a possible cause for the termination of a program.
#[derive(Copy, Clone, Debug)]
pub enum TermCause {
    /// An invocation of the exit syscall.
    Exit(u32),
    /// The program was terminated by a segmentation fault, i.e. the program attempted to
    /// access invalid memory.
    SegFault,
    /// The program was terminated by a bus error, i.e. the program attempted to access a physically
    /// invalid address
    BusError,
}

impl<T: ByteAddress> From<MemFault<T>> for TermCause {
    fn from(fault: MemFault<T>) -> TermCause {
        match fault.cause {
            MemFaultCause::PageFault => TermCause::SegFault,
            MemFaultCause::SegFault => TermCause::SegFault,
            MemFaultCause::BusError => TermCause::BusError,
        }
    }
}

impl TermCause {
    /// Prints any messages related to the exit cause, and returns the exit code.
    pub fn handle_exit<F: ArchFamily<T>, T: MachineDataWidth>(
        self,
        program_state: &mut ProgramState<F, T>,
    ) -> u8 {
        use TermCause::*;
        const ABNORMAL_MASK: u8 = 0b1000_0000;
        const NORMAL_MASK: u8 = 0b0111_1111;
        match self {
            Exit(n) => (n as u8) & NORMAL_MASK,
            SegFault => {
                program_state.write_stderr("Segmentation fault: 11\n");
                11u8 | ABNORMAL_MASK
            }
            BusError => {
                program_state.write_stderr("bus error\n");
                10u8 | ABNORMAL_MASK
            }
        }
    }
}

/// Represents the type of trap being raised from user mode.
/// See "Machine Cause Register" in the RISCV privileged spec for details.
pub enum TrapKind<T: ByteAddress> {
    /// Corresponds to an ecall instruction issued from user mode.
    Ecall,
    MemFault(MemFault<T>),
}

/// Encodes a change that occurs within the user space of a program, which entails a write to the
/// PC and possibly a register or memory operation.
pub enum InstResult<F: ArchFamily<T>, T: MachineDataWidth> {
    Trap(TrapKind<T::ByteAddr>),
    UserStateChange(UserDiff<F, T>),
}

/// Converts a memory fault into a trap.
impl<T: ByteAddress> From<MemFault<T>> for TrapKind<T> {
    fn from(fault: MemFault<T>) -> TrapKind<T> {
        TrapKind::MemFault(fault)
    }
}

/// Represents a diff as it is applied to a program.
pub enum ProgramDiff<F: ArchFamily<T>, T: MachineDataWidth> {
    PrivOnly(PrivStateChange<T>),
    UserOnly(UserDiff<F, T>),
}

#[derive(Copy, Clone)]
/// Encodes a change that occurred to the state of the privileged aspects of a program,
/// such as a write to a file.
pub enum PrivStateChange<T: MachineDataWidth> {
    /// Indicates that the program should terminate.
    Terminate(TermCause),
    NoChange,
    /// Represents a file write.
    /// * fd: the file descriptor
    /// * buf: user pointer to the data written
    /// * len: the number of bytes written
    FileWrite {
        fd: T::RegData,
        buf: T::ByteAddr,
        len: T::RegData,
    },
    /// Represents an attempt to map memory containing the provided address.
    TryPageIn {
        addr: T::ByteAddr,
    },
}

/// Represents a diff that is applied only to the user state of a program.
pub struct UserDiff<F: ArchFamily<T>, T: MachineDataWidth> {
    pc: PcDiff<T>,
    reg: Option<RegDiff<F::Register, T>>,
    mem: Option<MemDiff<T>>,
}

impl<F: ArchFamily<T>, T: MachineDataWidth> UserDiff<F, T> {
    pub fn into_inst_result(self) -> InstResult<F, T> {
        InstResult::UserStateChange(self)
    }

    fn new(
        state: &UserProgState<F, T>,
        new_pc: T::ByteAddr,
        reg_change: Option<RegDiff<F::Register, T>>,
        mem_change: Option<MemDiff<T>>,
    ) -> Self {
        UserDiff {
            pc: PcDiff {
                old_pc: state.pc,
                new_pc,
            },
            reg: reg_change,
            mem: mem_change,
        }
    }

    // A temporary haack to allow for empty user diffs.
    pub fn empty(state: &UserProgState<F, T>) -> Self {
        UserDiff::new(state, state.pc, None, None)
    }

    fn new_pc_p4(
        state: &UserProgState<F, T>,
        reg_change: Option<RegDiff<F::Register, T>>,
        mem_change: Option<MemDiff<T>>,
    ) -> Self {
        UserDiff::new(state, state.pc.plus_4(), reg_change, mem_change)
    }

    pub fn noop(state: &UserProgState<F, T>) -> Self {
        UserDiff::new_pc_p4(state, None, None)
    }

    pub fn pc_update_op(state: &UserProgState<F, T>, new_pc: T::ByteAddr) -> Self {
        UserDiff::new(state, new_pc, None, None)
    }

    pub fn reg_write_op(
        state: &UserProgState<F, T>,
        new_pc: T::ByteAddr,
        reg: F::Register,
        val: T::RegData,
    ) -> Self {
        UserDiff::new(
            state,
            new_pc,
            Some(RegDiff {
                reg,
                val: RegDataChange {
                    old_value: state.regfile.read(reg),
                    new_value: val,
                },
            }),
            None,
        )
    }

    pub fn reg_write_pc_p4(state: &UserProgState<F, T>, reg: F::Register, val: T::RegData) -> Self {
        UserDiff::reg_write_op(state, state.pc.plus_4(), reg, val)
    }

    /// Performs a memory write operation.
    /// This may trap to the OS in the event of exceptional events like a page fault.
    pub fn mem_write_op(
        state: &UserProgState<F, T>,
        addr: T::ByteAddr,
        val: DataEnum,
    ) -> Result<Self, MemFault<T::ByteAddr>> {
        Ok(UserDiff::new_pc_p4(
            state,
            None,
            Some(MemDiff {
                addr,
                old_val: state.memory.get(addr, val.width())?,
                new_val: val,
            }),
        ))
    }
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
