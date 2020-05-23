use crate::instruction::*;
use crate::lexer::Lexer;
use crate::parser::{ParseError, RiscVParser};
use std::cmp::{max, min};
use std::collections::HashMap;
use std::fmt;
use std::ops::Add;

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

#[derive(Debug, Copy, Clone)]
/// A bit vector that can fit inside 32 bits. Used to represent instruction fields.
pub struct BitStr32 {
    value: u32,
    pub len: u8,
}

impl BitStr32 {
    pub const fn new(value: u32, len: u8) -> BitStr32 {
        // Mask off upper bits
        // Shift by 32 is undef behavior, so need to use checked version; sadly it's not const fn
        // let truncated = value & !(u32::max_value().checked_shl(len as u32).unwrap_or(0));
        let shamt = (32 - len) as u32;
        let truncated = (value << shamt) >> shamt;
        BitStr32 {
            value: truncated,
            len,
        }
    }

    pub const fn is_zero(self) -> bool {
        self.value == 0
    }

    pub const fn concat(self, o: BitStr32) -> BitStr32 {
        BitStr32::new((self.value << o.len as u32) | o.value, self.len + o.len)
    }

    /// Extracts a the bits between start and end, inclusive.
    pub fn slice(self, start: u8, end: u8) -> BitStr32 {
        let high = max(start, end);
        let low = min(start, end);
        let len = high - low + 1;
        BitStr32::new(self.value >> low as u32, len)
    }

    /// Extracts the bit at index i.
    pub const fn index(self, i: u8) -> BitStr32 {
        BitStr32::new(self.value >> i as u32, 1)
    }

    /// Zero pads the LSB of this BitStr32.
    pub const fn zero_pad_lsb(self) -> BitStr32 {
        let shamt = (32 - self.len) as u32;
        BitStr32::new(self.value << shamt, 32)
    }

    /// Sign extends the value and stores it in a DataWord.
    pub fn to_sgn_data_word(self) -> DataWord {
        let sign_mask = u32::max_value() << self.len as u32;
        DataWord::from(if self.index(self.len - 1).value == 1 {
            self.value | sign_mask
        } else {
            self.value
        })
    }

    pub fn as_i32(self) -> i32 {
        i32::from(self.to_sgn_data_word())
    }

    pub const fn as_u32(self) -> u32 {
        self.value
    }
}

impl Add for BitStr32 {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        self.concat(other)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
/// Represents a 32-bit word of data that can be stored in a register.
/// This struct is used in place of a typealias to force call sites to make clear
/// whether desired behavior is that of a signed or unsigned number.
/// Note that Rust casts between int types of the same size is specified to be a no-op.
/// https://doc.rust-lang.org/nomicon/casts.html
pub struct DataWord {
    value: u32,
}

impl DataWord {
    /// Returns a DataWord of zero.
    pub const fn zero() -> DataWord {
        DataWord { value: 0 }
    }

    pub const fn to_bit_str(self, size: u8) -> BitStr32 {
        BitStr32::new(self.value, size)
    }

    /// Returns a copy of the value with the ith byte set to val.
    pub fn set_byte(self, i: u8, val: DataByte) -> DataWord {
        debug_assert!(i < 4);
        let mask: u32 = !(0xFF << (i * 8));
        DataWord {
            value: (self.value & mask) | ((val.value as u32) << (i * 8)),
        }
    }

    /// Selects the ith byte in the word, where 0 is the LSB.
    pub fn get_byte(self, i: u8) -> DataByte {
        debug_assert!(i < 4);
        DataByte {
            value: (self.value >> (i * 8)) as u8,
        }
    }
}

impl fmt::Display for DataWord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<ByteAddress> for DataWord {
    fn from(value: ByteAddress) -> DataWord {
        DataWord {
            value: u32::from(value),
        }
    }
}

impl From<u32> for DataWord {
    fn from(value: u32) -> DataWord {
        DataWord { value }
    }
}

impl From<i32> for DataWord {
    fn from(value: i32) -> DataWord {
        DataWord {
            value: value as u32,
        }
    }
}

impl From<DataWord> for u32 {
    fn from(value: DataWord) -> u32 {
        value.value
    }
}

impl From<DataWord> for i32 {
    fn from(value: DataWord) -> i32 {
        value.value as i32
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct DataByte {
    value: u8,
}

impl DataByte {
    pub const fn zero() -> DataByte {
        DataByte { value: 0 }
    }

    pub const fn zero_pad(self) -> DataWord {
        DataWord {
            value: self.value as u32,
        }
    }

    pub fn sign_extend(self) -> DataWord {
        if (self.value >> 7) > 0 {
            DataWord {
                value: (self.value as u32) | 0xFFFF_FF00,
            }
        } else {
            self.zero_pad()
        }
    }
}

impl From<u8> for DataByte {
    fn from(value: u8) -> DataByte {
        DataByte { value }
    }
}

impl From<i8> for DataByte {
    fn from(value: i8) -> DataByte {
        DataByte { value: value as u8 }
    }
}

impl From<DataByte> for u8 {
    fn from(value: DataByte) -> u8 {
        value.value
    }
}

impl From<DataByte> for i8 {
    fn from(value: DataByte) -> i8 {
        value.value as i8
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum IRegister {
    ZERO = 0,
    RA,
    SP,
    GP,
    TP,
    T0,
    T1,
    T2,
    FP,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    T3,
    T4,
    T5,
    T6,
}

impl fmt::Display for IRegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("{:?}", self);
        s.make_ascii_lowercase();
        write!(f, "{}", s)
    }
}

impl From<u8> for IRegister {
    fn from(value: u8) -> IRegister {
        IRegister::REG_ARRAY[value as usize]
    }
}

impl IRegister {
    pub const S0: IRegister = IRegister::FP;
    pub const REG_ARRAY: [IRegister; 32] = [
        IRegister::ZERO,
        IRegister::RA,
        IRegister::SP,
        IRegister::GP,
        IRegister::TP,
        IRegister::T0,
        IRegister::T1,
        IRegister::T2,
        IRegister::FP,
        IRegister::S1,
        IRegister::A0,
        IRegister::A1,
        IRegister::A2,
        IRegister::A3,
        IRegister::A4,
        IRegister::A5,
        IRegister::A6,
        IRegister::A7,
        IRegister::S2,
        IRegister::S3,
        IRegister::S4,
        IRegister::S5,
        IRegister::S6,
        IRegister::S7,
        IRegister::S8,
        IRegister::S9,
        IRegister::S10,
        IRegister::S11,
        IRegister::T3,
        IRegister::T4,
        IRegister::T5,
        IRegister::T6,
    ];
    pub const fn to_bit_str(self) -> BitStr32 {
        BitStr32::new(self as u32, 5)
    }
    pub const fn to_usize(self) -> usize {
        self as usize
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct ByteAddress {
    addr: u32,
}

impl ByteAddress {
    pub const fn new(v: u32) -> ByteAddress {
        ByteAddress { addr: v }
    }

    pub const fn to_word_address(self) -> WordAddress {
        self.addr >> 2
    }

    pub const fn get_word_offset(self) -> u8 {
        (self.addr & 0b11) as u8
    }

    pub const fn plus_4(self) -> ByteAddress {
        ByteAddress::new(self.addr.wrapping_add(4))
    }
}

impl From<DataWord> for ByteAddress {
    fn from(value: DataWord) -> ByteAddress {
        ByteAddress::new(value.value)
    }
}

impl From<u32> for ByteAddress {
    fn from(value: u32) -> ByteAddress {
        ByteAddress::new(value)
    }
}

impl From<i32> for ByteAddress {
    fn from(value: i32) -> ByteAddress {
        ByteAddress::new(value as u32)
    }
}

impl From<ByteAddress> for u32 {
    fn from(value: ByteAddress) -> u32 {
        value.addr
    }
}

impl From<ByteAddress> for i32 {
    fn from(value: ByteAddress) -> i32 {
        value.addr as i32
    }
}

pub type WordAddress = u32;

const REGFILE_SIZE: usize = 32;

pub struct RegFile {
    store: [DataWord; REGFILE_SIZE],
}

impl RegFile {
    fn new() -> RegFile {
        RegFile {
            store: [DataWord::zero(); REGFILE_SIZE],
        }
    }

    pub fn set(&mut self, rd: IRegister, val: DataWord) {
        if rd != IRegister::ZERO {
            self.store[rd.to_usize()] = val;
        }
    }

    pub fn read(&self, rs: IRegister) -> DataWord {
        self.store[rs.to_usize()]
    }
}

pub struct Memory {
    store: HashMap<WordAddress, DataWord>,
}

impl Memory {
    fn new() -> Memory {
        Memory {
            store: HashMap::new(),
        }
    }

    #[allow(dead_code)]
    pub fn set_byte(&mut self, addr: ByteAddress, value: DataByte) {
        let word_addr = addr.to_word_address();
        let offs = addr.get_word_offset();
        let word_val = if let Some(&old_val) = self.store.get(&word_addr) {
            old_val
        } else {
            DataWord::zero()
        };
        self.set_word(word_addr, word_val.set_byte(offs, value))
    }

    pub fn get_byte(&self, addr: ByteAddress) -> DataByte {
        let word_addr = addr.to_word_address();
        let offs = addr.get_word_offset();
        self.get_word(word_addr).get_byte(offs)
    }

    pub fn set_word(&mut self, addr: WordAddress, value: DataWord) {
        self.store.insert(addr, value);
    }

    pub fn get_word(&self, addr: WordAddress) -> DataWord {
        if let Some(&v) = self.store.get(&addr) {
            v
        } else {
            DataWord::zero()
        }
    }
}

pub struct ProgramState {
    pub priv_state: PrivProgState,
    pub user_state: UserProgState,
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
/// TODO create diffs on privileged state so they're reversible.
/// TODO put errno on user state (although it's at a thread-local statically known location)
impl ProgramState {
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
    pub fn apply_diff(&mut self, diff: &UserStateChange) {
        match diff {
            UserStateChange::Trap(trap_kind) => {
                let priv_diff = &self.handle_trap(trap_kind);
                let user_diff = self.priv_state.apply_diff(&self.user_state, priv_diff);
                self.user_state.apply_diff(&user_diff);
            }
            UserStateChange::UserOnly(user_diff) => self.user_state.apply_diff(&user_diff),
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

    pub fn apply_diff(&mut self, user_state: &UserProgState, diff: &PrivStateChange) -> UserOnly {
        use PrivStateChange::*;
        match diff {
            NoChange => UserStateChange::noop(user_state),
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
                UserStateChange::reg_write_pc_p4(user_state, IRegister::A0, *len)
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

    pub fn apply_diff(&mut self, diff: &UserOnly) {
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

    pub fn revert_diff(&mut self, diff: &UserOnly) {
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
/// TODO rename to InstResult
pub enum UserStateChange {
    Trap(TrapKind),
    UserOnly(UserOnly),
}

/// Represents a diff as it is applied to a program.
pub enum ProgramDiff {
    PrivOnly(PrivStateChange),
    UserOnly(UserOnly),
}

/// Represents a diff that is applied only to the user state of a program.
pub struct UserOnly {
    pc: PcDiff,
    reg: Option<RegDiff>,
    mem: Option<MemDiff>,
}

impl UserOnly {
    fn new(pc: PcDiff, reg: Option<RegDiff>, mem: Option<MemDiff>) -> UserOnly {
        UserOnly { pc, reg, mem }
    }
}

impl UserStateChange {
    fn new_user_only(
        state: &UserProgState,
        new_pc: ByteAddress,
        reg_change: Option<RegDiff>,
        mem_change: Option<MemDiff>,
    ) -> UserOnly {
        UserOnly::new(
            PcDiff {
                old_pc: state.pc,
                new_pc,
            },
            reg_change,
            mem_change,
        )
    }

    fn new_pc_p4(
        state: &UserProgState,
        reg_change: Option<RegDiff>,
        mem_change: Option<MemDiff>,
    ) -> UserOnly {
        UserStateChange::new_user_only(state, state.pc.plus_4(), reg_change, mem_change)
    }

    pub fn noop(state: &UserProgState) -> UserOnly {
        UserStateChange::new_pc_p4(state, None, None)
    }

    pub fn pc_update_op(state: &UserProgState, new_pc: ByteAddress) -> UserOnly {
        UserStateChange::new_user_only(state, new_pc, None, None)
    }

    pub fn reg_write_op(
        state: &UserProgState,
        new_pc: ByteAddress,
        reg: IRegister,
        val: DataWord,
    ) -> UserOnly {
        UserStateChange::new_user_only(
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

    pub fn reg_write_pc_p4(state: &UserProgState, reg: IRegister, val: DataWord) -> UserOnly {
        UserStateChange::reg_write_op(state, state.pc.plus_4(), reg, val)
    }

    /// Performs a memory write operation.
    /// This may trap to the OS in the event of exceptional events like a page fault.
    pub fn mem_write_op(state: &UserProgState, addr: WordAddress, val: DataWord) -> UserOnly {
        UserStateChange::new_pc_p4(
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
    fn test_bv_as_i32() {
        let bv = BitStr32::new(-4i32 as u32, 12);
        assert_eq!(bv.as_i32(), -4);
        let wrap = BitStr32::new(-1i32 as u32, 12);
        assert_eq!(wrap.as_i32(), -1);
    }

    #[test]
    fn test_full_bv() {
        let bv = BitStr32::new(0xFFFF_FFFF, 32);
        assert_eq!(bv.as_u32(), 0xFFFF_FFFF);
    }

    #[test]
    fn test_bv_truncate() {
        let all_ones = 0xFFFF_FFFF;
        let bv = BitStr32::new(all_ones, 12);
        assert_eq!(bv.as_u32(), 0xFFF);
        assert_eq!(bv.as_i32(), all_ones as i32);
    }

    #[test]
    fn test_bv_zero_pad() {
        let bv = BitStr32::new(0x7FF, 12);
        assert_eq!(bv.zero_pad_lsb().as_u32(), 0x7FF0_0000);
    }

    #[test]
    fn test_e2e_program() {
        let mut program =
            RiscVProgram::from_string("addi s1, zero, 4\nadd a0, s1, zero".to_string()).unwrap();
        let result = program.run();
        assert_eq!(result, 4);
    }
}
