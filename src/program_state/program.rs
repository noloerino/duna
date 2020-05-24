use crate::instruction::ConcreteInst;
use crate::lexer::Lexer;
use crate::parser::{ParseError, RiscVParser};
use crate::program_state::memory::Memory;
use crate::program_state::registers::IRegister;
use crate::program_state::registers::RegFile;

use crate::program_state::datatypes::*;

pub struct RiscVProgram {
    pub insts: Vec<ConcreteInst>,
    pub state: ProgramState,
    // TODO add symbol table, relocation data, etc.?
}

impl RiscVProgram {
    pub const TEXT_START: ByteAddress = ByteAddress::new(0x0000_0008);
    pub const STACK_START: ByteAddress = ByteAddress::new(0x7FFF_FFF0);

    /// Initializes a new program instance from the provided instructions.
    /// The instructions are loaded into memory at the start of the instruction section,
    /// which defaults to TEXT_START to avoid any accidental null pointer derefs.
    /// The stack pointer is initialized to STACK_START.

    pub fn new(insts: Vec<ConcreteInst>) -> RiscVProgram {
        let mut state = ProgramState::new();
        let mut user_state = &mut state.user_state;
        user_state
            .regfile
            .set(IRegister::SP, DataWord::from(RiscVProgram::STACK_START));
        user_state.pc = RiscVProgram::TEXT_START;
        let mut next_addr = user_state.pc.to_word_address();
        for inst in &insts {
            user_state
                .memory
                .set_word(next_addr, DataWord::from(inst.to_machine_code()));
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

    pub fn from_file(path: &str) -> Result<RiscVProgram, Vec<ParseError>> {
        RiscVProgram::call_parser(Lexer::from_file(path))
    }

    pub fn from_string(contents: String) -> Result<RiscVProgram, Vec<ParseError>> {
        RiscVProgram::call_parser(Lexer::from_string(contents))
    }

    fn call_parser(lexer: Lexer) -> Result<RiscVProgram, Vec<ParseError>> {
        let (toks, lex_errs) = lexer.lex();
        let (insts, parse_errs) = RiscVParser::from_tokens(toks).parse();
        let mut all_errs = lex_errs;
        all_errs.extend(parse_errs);
        if all_errs.is_empty() {
            Ok(RiscVProgram::new(insts))
        } else {
            Err(all_errs)
        }
    }

    /// Runs the program to completion, returning the value in register a0.
    pub fn run(&mut self) -> i32 {
        // for now, just use the instruction vec to determine the next instruction
        let pc_start = RiscVProgram::TEXT_START;
        // for now, if we're out of instructions just call it a day
        // if pc dipped below pc_start, panic for now is also fine
        while let Some(inst) = self.insts.get(
            ByteAddress::from(u32::from(self.state.user_state.pc) - u32::from(pc_start))
                .to_word_address() as usize,
        ) {
            self.state.apply_inst(inst);
        }
        i32::from(self.state.user_state.regfile.read(IRegister::A0))
    }
}

pub struct ProgramState {
    pub(crate) priv_state: PrivProgState,
    pub(crate) user_state: UserProgState,
}

impl Default for ProgramState {
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
impl ProgramState {
    pub fn get_stdout(&self) -> &[u8] {
        self.priv_state.stdout.as_slice()
    }

    pub fn get_user_pc(&self) -> ByteAddress {
        self.user_state.pc
    }

    pub fn set_user_pc(&mut self, addr: ByteAddress) {
        self.user_state.pc = addr
    }

    pub fn regfile_read(&self, reg: IRegister) -> DataWord {
        self.user_state.regfile.read(reg)
    }

    pub fn regfile_set(&mut self, reg: IRegister, val: DataWord) {
        self.user_state.regfile.set(reg, val);
    }

    pub fn memory_get_word(&self, addr: WordAddress) -> DataWord {
        self.user_state.memory.get_word(addr)
    }

    pub fn memory_set_word(&mut self, addr: WordAddress, val: DataWord) {
        self.user_state.memory.set_word(addr, val);
    }

    pub fn handle_trap(&self, trap_kind: &TrapKind) -> PrivStateChange {
        use TrapKind::*;
        match trap_kind {
            Ecall => self.dispatch_syscall(),
            _ => unimplemented!(),
        }
    }

    pub fn dispatch_syscall(&self) -> PrivStateChange {
        use IRegister::*;
        let rf = &self.user_state.regfile;
        let a0 = rf.read(A0);
        let a1 = rf.read(A1);
        let a2 = rf.read(A2);
        if let Some(nr) = Syscall::from_number(u32::from(self.user_state.regfile.read(A7))) {
            match nr {
                Syscall::Write => self.syscall_write(a0, ByteAddress::from(a1), a2),
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
    fn syscall_write(&self, fd: DataWord, buf: ByteAddress, len: DataWord) -> PrivStateChange {
        PrivStateChange::FileWrite { fd, buf, len }
    }

    /// Handles an unknown syscall.
    fn syscall_unknown(&self) -> PrivStateChange {
        panic!("Unknown syscall")
    }

    pub fn new() -> ProgramState {
        ProgramState {
            priv_state: PrivProgState::new(),
            user_state: UserProgState::new(),
        }
    }

    pub fn apply_inst(&mut self, inst: &ConcreteInst) {
        self.apply_diff(&(*inst.eval)(self));
    }

    /// Performs the described operation.
    /// The privileged operation is applied first, followed by the user operation.
    pub fn apply_diff(&mut self, diff: &InstResult) {
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
    pub fn revert_diff(&mut self, diff: &ProgramDiff) {
        match diff {
            ProgramDiff::UserOnly(user_only) => self.user_state.revert_diff(user_only),
            ProgramDiff::PrivOnly(priv_only) => {
                self.priv_state.revert_diff(&self.user_state, priv_only)
            }
        }
    }
}
/// Syscall numbers for x86_64.
/// See https://fedora.juszkiewicz.com.pl/syscalls.html
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Syscall {
    Read = 0,
    Write,
    Open,
    Close,
}

impl Syscall {
    const SYSCALL_LIST: [Syscall; 4] =
        [Syscall::Read, Syscall::Write, Syscall::Open, Syscall::Close];
    /// Returns the syscall identified by number N, or none if no such syscall exists.
    pub fn from_number(n: u32) -> Option<Syscall> {
        if (n as usize) < Syscall::SYSCALL_LIST.len() {
            Some(Syscall::SYSCALL_LIST[n as usize])
        } else {
            None
        }
    }

    pub fn to_number(self) -> DataWord {
        DataWord::from(self as u32)
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

    pub fn apply_diff(&mut self, user_state: &UserProgState, diff: &PrivStateChange) -> UserDiff {
        use PrivStateChange::*;
        match diff {
            NoChange => UserDiff::noop(user_state),
            FileWrite { fd: _, buf, len } => {
                let memory = &user_state.memory;
                let count = u32::from(*len);
                let bytes: Vec<u8> = (0..count)
                    .map(|i| {
                        u8::from(
                            memory.get_byte(ByteAddress::from(u32::from(*buf).wrapping_add(i))),
                        )
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
    pub fn revert_diff(&mut self, _user_state: &UserProgState, diff: &PrivStateChange) {
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
pub struct UserProgState {
    pub pc: ByteAddress,
    pub regfile: RegFile,
    pub memory: Memory,
}

impl Default for UserProgState {
    fn default() -> Self {
        UserProgState::new()
    }
}

impl UserProgState {
    pub fn new() -> UserProgState {
        UserProgState {
            pc: ByteAddress::from(0),
            regfile: RegFile::new(),
            memory: Memory::new(),
        }
    }

    pub fn apply_diff(&mut self, diff: &UserDiff) {
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

    pub fn revert_diff(&mut self, diff: &UserDiff) {
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
struct WordChange {
    old_value: DataWord,
    new_value: DataWord,
}

/// A change to the program counter.
struct PcDiff {
    old_pc: ByteAddress,
    new_pc: ByteAddress,
}

/// A change to a register.
struct RegDiff {
    reg: IRegister,
    val: WordChange,
}

/// A change to memory.
struct MemDiff {
    addr: WordAddress,
    val: WordChange,
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
pub enum InstResult {
    Trap(TrapKind),
    UserStateChange(UserDiff),
}

/// Represents a diff as it is applied to a program.
pub enum ProgramDiff {
    PrivOnly(PrivStateChange),
    UserOnly(UserDiff),
}

#[derive(Copy, Clone)]
/// Encodes a change that occurred to the state of the privileged aspects of a program,
/// such as a write to a file.
pub enum PrivStateChange {
    /// Indicates that the program should terminate.
    /// TODO implement exit codes
    Exit,
    NoChange,
    /// Represents a file write.
    /// * fd: the file descriptor
    /// * buf: user pointer to the data written
    /// * len: the number of bytes written
    FileWrite {
        fd: DataWord,
        buf: ByteAddress,
        len: DataWord,
    },
}

/// Represents a diff that is applied only to the user state of a program.
pub struct UserDiff {
    pc: PcDiff,
    reg: Option<RegDiff>,
    mem: Option<MemDiff>,
}

impl UserDiff {
    pub fn into_inst_result(self) -> InstResult {
        InstResult::UserStateChange(self)
    }

    fn new(
        state: &UserProgState,
        new_pc: ByteAddress,
        reg_change: Option<RegDiff>,
        mem_change: Option<MemDiff>,
    ) -> UserDiff {
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
        state: &UserProgState,
        reg_change: Option<RegDiff>,
        mem_change: Option<MemDiff>,
    ) -> UserDiff {
        UserDiff::new(state, state.pc.plus_4(), reg_change, mem_change)
    }

    pub fn noop(state: &UserProgState) -> UserDiff {
        UserDiff::new_pc_p4(state, None, None)
    }

    pub fn pc_update_op(state: &UserProgState, new_pc: ByteAddress) -> UserDiff {
        UserDiff::new(state, new_pc, None, None)
    }

    pub fn reg_write_op(
        state: &UserProgState,
        new_pc: ByteAddress,
        reg: IRegister,
        val: DataWord,
    ) -> UserDiff {
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

    pub fn reg_write_pc_p4(state: &UserProgState, reg: IRegister, val: DataWord) -> UserDiff {
        UserDiff::reg_write_op(state, state.pc.plus_4(), reg, val)
    }

    /// Performs a memory write operation.
    /// This may trap to the OS in the event of exceptional events like a page fault.
    pub fn mem_write_op(state: &UserProgState, addr: WordAddress, val: DataWord) -> UserDiff {
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

    #[test]
    fn test_e2e_program() {
        let mut program =
            RiscVProgram::from_string("addi s1, zero, 4\nadd a0, s1, zero".to_string()).unwrap();
        let result = program.run();
        assert_eq!(result, 4);
    }
}
