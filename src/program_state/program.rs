use super::datatypes::*;
use super::memory::Memory;
use super::registers::{IRegister, RegFile};
use crate::assembler::{Assembler, ParseErrorReport};
use crate::instruction::ConcreteInst;
use std::collections::HashMap;
use std::str;

pub struct RiscVProgram<T: MachineDataWidth> {
    pub insts: Vec<ConcreteInst<T>>,
    pub state: ProgramState<T>,
    // TODO add symbol table, relocation data, etc.?
}

impl<T: MachineDataWidth> RiscVProgram<T> {
    const TEXT_START: u64 = 0x0000_0008;
    const STACK_START: u64 = 0x7FFF_FFF0;

    /// Initializes a new program instance from the provided instructions.
    /// The instructions are loaded into memory at the start of the instruction section,
    /// which defaults to TEXT_START to avoid any accidental null pointer derefs.
    /// The stack pointer is initialized to STACK_START.
    pub fn new(insts: Vec<ConcreteInst<T>>) -> RiscVProgram<T> {
        let mut state = ProgramState::new();
        let mut user_state = &mut state.user_state;
        let text_start = T::ByteAddr::new(RiscVProgram::TEXT_START);
        let stack_start = T::ByteAddr::new(RiscVProgram::STACK_START);
        user_state
            .regfile
            .set(IRegister::SP, T::addr_to_data(stack_start));
        user_state.pc = text_start;
        let mut next_addr = (user_state.pc as T::ByteAddr).to_word_address();
        for inst in &insts {
            user_state.memory.set_word(
                next_addr,
                <T::RegData as From<u64>>::from(inst.to_machine_code() as u64),
            );
            next_addr += 1
        }
        RiscVProgram { insts, state }
    }

    /// Prints out all the instructions that this program contains.
    pub fn dump_insts(&self) {
        for inst in &self.insts {
            println!("{:?}", inst);
        }
    }

    pub fn from_file(path: &str) -> Result<RiscVProgram<T>, ParseErrorReport> {
        Ok(Assembler::assemble_file(path)?.try_into_program())
    }

    /// Runs the program to completion, returning the value in register a0.
    pub fn run(&mut self) -> T::Signed {
        // for now, just use the instruction vec to determine the next instruction
        let pc_start = RiscVProgram::TEXT_START;
        // for now, if we're out of instructions just call it a day
        // if pc dipped below pc_start, panic for now is also fine
        while let Some(inst) = self.insts.get(
            T::unsigned_to_addr(
                T::addr_to_unsigned(self.state.user_state.pc) - T::addr_to_unsigned(pc_start),
            )
            .to_word_address() as usize,
        ) {
            self.state.apply_inst(inst);
        }
        T::data_to_signed(self.state.user_state.regfile.read(IRegister::A0))
    }
}

impl<T: MachineDataWidth> str::FromStr for RiscVProgram<T> {
    type Err = ParseErrorReport;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Assembler::assemble_str(s)?.try_into_program())
    }
}

pub struct ProgramState<T: MachineDataWidth> {
    pub(crate) priv_state: PrivProgState,
    pub(crate) user_state: UserProgState<T>,
}

impl<T: MachineDataWidth> Default for ProgramState<T> {
    fn default() -> Self {
        ProgramState::new()
    }
}

/// Implements functions that require OS privileges to perform, such as reading/writing files.
/// Per the RISCV calling convention (see http://man7.org/linux/man-pages/man2/syscall.2.html),
/// the a7 register determines which syscall is being performed, and the arguments are stored
/// in the argument registers of user space.
/// See [Syscall] for syscall codes.
/// TODO put custom types for syscall args
/// TODO put errno on user state at a thread-local statically known location
impl<T: MachineDataWidth> ProgramState<T> {
    pub fn get_stdout(&self) -> &[u8] {
        self.priv_state.stdout.as_slice()
    }

    pub fn get_user_pc(&self) -> T::ByteAddr {
        self.user_state.pc
    }

    pub fn set_user_pc(&mut self, addr: T::ByteAddr) {
        self.user_state.pc = addr
    }

    pub fn regfile_read(&self, reg: IRegister) -> T::RegData {
        self.user_state.regfile.read(reg)
    }

    pub fn regfile_set(&mut self, reg: IRegister, val: T::RegData) {
        self.user_state.regfile.set(reg, val);
    }

    pub fn memory_get_word(&self, addr: <T::ByteAddr as ByteAddress>::WordAddress) -> T::RegData {
        self.user_state.memory.get_word(addr)
    }

    pub fn memory_set_word(
        &mut self,
        addr: <T::ByteAddr as ByteAddress>::WordAddress,
        val: T::RegData,
    ) {
        self.user_state.memory.set_word(addr, val);
    }

    pub fn handle_trap(&self, trap_kind: &TrapKind) -> PrivStateChange<T> {
        use TrapKind::*;
        match trap_kind {
            Ecall => self.dispatch_syscall(),
            _ => unimplemented!(),
        }
    }

    pub fn dispatch_syscall(&self) -> PrivStateChange<T> {
        use IRegister::*;
        let rf = &self.user_state.regfile;
        let a0 = rf.read(A0);
        let a1 = rf.read(A1);
        let a2 = rf.read(A2);
        if let Some(nr) =
            Syscall::from_number::<T>(T::data_to_signed(self.user_state.regfile.read(A7)))
        {
            match nr {
                Syscall::Write => self.syscall_write(a0, T::data_to_addr(a1), a2),
                _ => self.syscall_unknown(),
            }
        } else {
            self.syscall_unknown()
        }
    }

    /// Writes contents to a specified file descriptor.
    /// TODO for now, this is hardcoded to print to stdout regardless of the provided FD.
    /// * a0 - file descriptor
    /// * a1 - pointer to the buffer to be written
    /// * a2 - the number of bytes to write
    fn syscall_write(
        &self,
        fd: T::RegData,
        buf: T::ByteAddr,
        len: T::RegData,
    ) -> PrivStateChange<T> {
        PrivStateChange::FileWrite { fd, buf, len }
    }

    /// Handles an unknown syscall.
    fn syscall_unknown(&self) -> PrivStateChange<T> {
        panic!("Unknown syscall")
    }

    pub fn new() -> ProgramState<T> {
        ProgramState {
            priv_state: PrivProgState::new(),
            user_state: UserProgState::new(),
        }
    }

    pub fn apply_inst(&mut self, inst: &ConcreteInst<T>) {
        self.apply_diff(&(*inst.eval)(self));
    }

    /// Performs the described operation.
    /// The privileged operation is applied first, followed by the user operation.
    pub fn apply_diff(&mut self, diff: &InstResult<T>) {
        match diff {
            InstResult::Trap(trap_kind) => {
                let priv_diff = &self.handle_trap(trap_kind);
                let user_diff = self.priv_state.apply_diff(&self.user_state, priv_diff);
                self.user_state.apply_diff(&user_diff);
            }
            InstResult::UserStateChange(user_diff) => self.user_state.apply_diff(&user_diff),
        };
    }

    /// Reverts the described operation.
    /// Since the privileged diff is applied first during execution, the user diff should
    /// be applied first during a revert.
    /// TODO figure out how to implement that...
    pub fn revert_diff(&mut self, diff: &ProgramDiff<T>) {
        match diff {
            ProgramDiff::UserOnly(user_only) => self.user_state.revert_diff(user_only),
            ProgramDiff::PrivOnly(priv_only) => {
                self.priv_state.revert_diff(&self.user_state, priv_only)
            }
        }
    }
}
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Syscall {
    Read,
    Write,
    Open,
    Close,
}

lazy_static! {
    /// Syscall numbers for RV32.
    /// See https://github.com/hrw/syscalls-table/blob/master/tables/syscalls-riscv32.
    /// See https://fedora.juszkiewicz.com.pl/syscalls.html for other ISAs
    static ref RISCV_SYSCALL_TABLE: HashMap<i32, Syscall> = {
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
    static ref RISCV_SYSCALL_NUMBERS: HashMap<Syscall, i32> =
        RISCV_SYSCALL_TABLE
        .iter()
        .map(|(n, syscall)| {(*syscall, *n)})
        .collect();
}

impl Syscall {
    /// Returns the syscall identified by number N, or none if no such syscall exists.
    pub fn from_number<T: MachineDataWidth>(n: T::Signed) -> Option<Syscall> {
        RISCV_SYSCALL_TABLE.get(&n).cloned()
    }

    /// Returns the number corresponding to the syscall, or -1 if it is unimplemented.
    pub fn to_number(self) -> DataWord {
        DataWord::from(RISCV_SYSCALL_NUMBERS.get(&self).copied().unwrap_or(-1))
    }
}

/// Contains program state that is visited only to privileged entities, i.e. a kernel thread.
pub struct PrivProgState {
    /// Holds the contents of all bytes that have been printed to stdout (used mostly for testing)
    /// TODO add kernel thread information (tid, file descriptors, etc.)
    pub stdout: Vec<u8>,
}

impl Default for PrivProgState {
    fn default() -> Self {
        PrivProgState::new()
    }
}

impl PrivProgState {
    pub fn new() -> PrivProgState {
        PrivProgState { stdout: Vec::new() }
    }

    pub fn apply_diff<T: MachineDataWidth>(
        &mut self,
        user_state: &UserProgState<T>,
        diff: &PrivStateChange<T>,
    ) -> UserDiff<T> {
        use PrivStateChange::*;
        match diff {
            NoChange => UserDiff::noop(user_state),
            FileWrite { fd: _, buf, len } => {
                let memory = &user_state.memory;
                let count = T::unsigned_to_usize(T::data_to_unsigned(*len));
                let bytes: Vec<u8> =
                    (0..count)
                        .map(|i| {
                            u8::from(memory.get_byte(T::data_to_addr(
                                T::data_to_unsigned(*buf).wrapping_add(i),
                            )))
                        })
                        .collect();
                // TODO impl for other files
                print!("{}", String::from_utf8_lossy(&bytes));
                self.stdout.extend(bytes);
                UserDiff::reg_write_pc_p4(user_state, IRegister::A0, *len)
            }
            Exit => unimplemented!(),
        }
    }

    /// Reverts a privileged state change.
    /// The originally produced UserOnly diff MUST have already been applied.
    pub fn revert_diff<T: MachineDataWidth>(
        &mut self,
        _user_state: &UserProgState<T>,
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
pub struct UserProgState<T: MachineDataWidth> {
    pub pc: T::ByteAddr,
    pub regfile: RegFile<T>,
    pub memory: Memory<T>,
}

impl<T: MachineDataWidth> Default for UserProgState<T> {
    fn default() -> Self {
        UserProgState::new()
    }
}

impl<T: MachineDataWidth> UserProgState<T> {
    pub fn new() -> UserProgState<T> {
        UserProgState {
            pc: T::unsigned_to_addr(0),
            regfile: RegFile::new(),
            memory: Memory::new(),
        }
    }

    pub fn apply_diff(&mut self, diff: &UserDiff<T>) {
        self.pc = diff.pc.new_pc;
        if let Some(RegDiff {
            reg,
            val: WordChange { new_value, .. },
        }) = diff.reg
        {
            self.regfile.set(reg, new_value);
        }
        if let Some(MemDiff {
            addr,
            val: WordChange { new_value, .. },
        }) = diff.mem
        {
            self.memory.set_word(addr, new_value);
        }
    }

    pub fn revert_diff(&mut self, diff: &UserDiff<T>) {
        self.pc = diff.pc.old_pc;
        if let Some(RegDiff {
            reg,
            val: WordChange { old_value, .. },
        }) = diff.reg
        {
            self.regfile.set(reg, old_value);
        }
        if let Some(MemDiff {
            addr,
            val: WordChange { old_value, .. },
        }) = diff.mem
        {
            self.memory.set_word(addr, old_value);
        }
    }
}

#[derive(Copy, Clone)]
struct WordChange<T: MachineDataWidth> {
    old_value: T::RegData,
    new_value: T::RegData,
}

/// A change to the program counter.
struct PcDiff<T: MachineDataWidth> {
    old_pc: T::ByteAddr,
    new_pc: T::ByteAddr,
}

/// A change to a register.
struct RegDiff<T: MachineDataWidth> {
    reg: IRegister,
    val: WordChange<T>,
}

/// A change to memory.
struct MemDiff<T: MachineDataWidth> {
    addr: <T::ByteAddr as ByteAddress>::WordAddress,
    val: WordChange<T>,
}

/// Represents the type of trap being raised from user mode.
/// See "Machine Cause Register" in the RISCV privileged spec for details.
pub enum TrapKind {
    /// Corresponds to an ecall instruction issued from user mode.
    Ecall,
    /// TODO
    PageFault,
}

/// Encodes a change that occurs within the user space of a program, which entails a write to the
/// PC and possibly a register or memory operation.
pub enum InstResult<T: MachineDataWidth> {
    Trap(TrapKind),
    UserStateChange(UserDiff<T>),
}

/// Represents a diff as it is applied to a program.
pub enum ProgramDiff<T: MachineDataWidth> {
    PrivOnly(PrivStateChange<T>),
    UserOnly(UserDiff<T>),
}

#[derive(Copy, Clone)]
/// Encodes a change that occurred to the state of the privileged aspects of a program,
/// such as a write to a file.
pub enum PrivStateChange<T: MachineDataWidth> {
    /// Indicates that the program should terminate.
    /// TODO implement exit codes
    Exit,
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
}

/// Represents a diff that is applied only to the user state of a program.
pub struct UserDiff<T: MachineDataWidth> {
    pc: PcDiff<T>,
    reg: Option<RegDiff<T>>,
    mem: Option<MemDiff<T>>,
}

impl<T: MachineDataWidth> UserDiff<T> {
    pub fn into_inst_result(self) -> InstResult<T> {
        InstResult::UserStateChange(self)
    }

    fn new(
        state: &UserProgState<T>,
        new_pc: T::ByteAddr,
        reg_change: Option<RegDiff<T>>,
        mem_change: Option<MemDiff<T>>,
    ) -> UserDiff<T> {
        UserDiff {
            pc: PcDiff {
                old_pc: state.pc,
                new_pc,
            },
            reg: reg_change,
            mem: mem_change,
        }
    }

    fn new_pc_p4(
        state: &UserProgState<T>,
        reg_change: Option<RegDiff<T>>,
        mem_change: Option<MemDiff<T>>,
    ) -> UserDiff<T> {
        UserDiff::new(state, state.pc.plus_4(), reg_change, mem_change)
    }

    pub fn noop(state: &UserProgState<T>) -> UserDiff<T> {
        UserDiff::new_pc_p4(state, None, None)
    }

    pub fn pc_update_op(state: &UserProgState<T>, new_pc: T::ByteAddr) -> UserDiff<T> {
        UserDiff::new(state, new_pc, None, None)
    }

    pub fn reg_write_op(
        state: &UserProgState<T>,
        new_pc: T::ByteAddr,
        reg: IRegister,
        val: T::RegData,
    ) -> UserDiff<T> {
        UserDiff::new(
            state,
            new_pc,
            Some(RegDiff {
                reg,
                val: WordChange {
                    old_value: state.regfile.read(reg),
                    new_value: val,
                },
            }),
            None,
        )
    }

    pub fn reg_write_pc_p4(
        state: &UserProgState<T>,
        reg: IRegister,
        val: T::RegData,
    ) -> UserDiff<T> {
        UserDiff::reg_write_op(state, state.pc.plus_4(), reg, val)
    }

    /// Performs a memory write operation.
    /// This may trap to the OS in the event of exceptional events like a page fault.
    pub fn mem_write_op(
        state: &UserProgState<T>,
        addr: <T::ByteAddr as ByteAddress>::WordAddress,
        val: T::RegData,
    ) -> UserDiff<T> {
        UserDiff::new_pc_p4(
            state,
            None,
            Some(MemDiff {
                addr,
                val: WordChange {
                    old_value: state.memory.get_word(addr),
                    new_value: val,
                },
            }),
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::program_state::Width32b;

    #[test]
    fn test_e2e_program() {
        let mut program = "addi s1, zero, 4\nadd a0, s1, zero"
            .parse::<RiscVProgram<Width32b>>()
            .unwrap();
        let result = program.run();
        assert_eq!(result, 4);
    }
}
