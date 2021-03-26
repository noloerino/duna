use super::{memory::*, registers::RegFile};
pub use super::{phys::*, priv_s::*, user::*};
use crate::{
    arch::*,
    assembler::{ErrorReport, Linker, SectionStore},
    config::{MemConfig, SegmentStarts},
    data_structures::*,
    instruction::ConcreteInst,
};
use num_traits::{cast::AsPrimitive, ops::wrapping::WrappingSub};
use std::str::FromStr;

/// Defines architecture-specific behavior that defines the execution of a program.
pub trait ProgramBehavior<F, S>
where
    F: ArchFamily<S>,
    S: DataWidth,
{
    /// Returns the register that holds the stack pointer.
    fn sp_register() -> F::Register;
    /// Returns the register that holds function return values.
    fn return_register() -> F::Register;
}

#[derive(Clone)]
pub struct ProgramResetParams {
    start_inst_idx: usize,
    segment_starts: SegmentStarts,
    sections: SectionStore,
    mem_config: MemConfig,
}

pub struct Program<A: Architecture> {
    insts: Vec<<A::Family as ArchFamily<A::DataWidth>>::Instruction>,
    reset_params: ProgramResetParams,
    pub state: ProgramState<A::Family, A::DataWidth>,
    text_start: ByteAddrValue<A::DataWidth>,
}

impl<A: Architecture> Program<A> {
    /// Initializes a new program instance from the provided instructions.
    ///
    /// The instructions are loaded into memory at the start of the instruction section,
    /// which is specified in SEGMENT_STARTS.
    ///
    /// The program counter is initialized to point to the instruction specified by START_INST_IDX.
    ///
    /// The data given in SECTIONS is used to initialize the data and rodata sections.
    ///
    /// Until paged memory is implemented, rodata is placed sequentially with data, and
    /// no guarantees on read-onliness are enforced.
    pub fn new(
        insts: Vec<<A::Family as ArchFamily<A::DataWidth>>::Instruction>,
        start_inst_idx: usize,
        segment_starts: SegmentStarts,
        sections: SectionStore,
        mem_config: MemConfig,
    ) -> Self {
        let pg_count = mem_config.phys_pn_bits;
        let pg_ofs_len = mem_config.pg_ofs_bits;
        let page_table = mem_config.build_mem();
        let text_start: ByteAddrValue<A::DataWidth> = segment_starts.text();
        let state = ProgramState::new(pg_count, pg_ofs_len, page_table);
        let mut p = Program {
            insts,
            reset_params: ProgramResetParams {
                start_inst_idx,
                segment_starts,
                sections,
                mem_config,
            },
            state,
            text_start,
        };
        p.reset();
        p
    }

    /// Resets the state of this program.
    pub fn reset(&mut self) {
        let ProgramResetParams {
            start_inst_idx,
            segment_starts,
            sections,
            mem_config,
        } = &self.reset_params;
        let pg_ofs_len = mem_config.pg_ofs_bits;
        let text_start: ByteAddrValue<A::DataWidth> = segment_starts.text();
        let stack_start: ByteAddrValue<A::DataWidth> = segment_starts.stack();
        let data_start: ByteAddrValue<A::DataWidth> = segment_starts.data();
        self.state.reset();
        let state = &mut self.state;
        let mem = &mut state.phys_state.phys_mem;
        let pt = &mut state.priv_state.page_table;
        // Page in text, stack, and data
        pt.force_map_page(mem, text_start).unwrap();
        pt.force_map_page(mem, stack_start).unwrap();
        pt.force_map_page(mem, data_start).unwrap();
        let mut user_state = &mut state.user_state;
        let sp = <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::sp_register();
        // Initialize SP and PC
        user_state.regfile.set(sp, stack_start.into());
        user_state.pc =
            text_start + (UnsignedValue::<A::DataWidth>::from(4 * start_inst_idx)).into();
        // store instructions
        let mut next_addr: ByteAddrValue<A::DataWidth> = user_state.pc;
        for inst in &self.insts {
            state.memory_force_set(next_addr, DataLword::from(inst.to_machine_code()));
            next_addr = next_addr.plus_4()
        }
        // store data
        let all_data = sections
            .data()
            .iter()
            .chain(sections.rodata().iter())
            .cloned();
        let data_start_usize = AsPrimitive::<usize>::as_(data_start.as_unsigned().raw());
        let mut end_of_data: usize = data_start_usize;
        for (offs, byte) in all_data.enumerate() {
            let addr: ByteAddrValue<A::DataWidth> =
                UnsignedValue::<A::DataWidth>::from(data_start_usize + offs).into();
            state.memory_force_set(addr, byte.into());
            end_of_data = data_start_usize + offs;
        }
        // Round up to next page
        let heap_start = ((end_of_data >> pg_ofs_len) + 1) << 1;
        // Can't reuse variables for lifetime reasons
        state
            .priv_state
            .page_table
            .force_map_page(
                &mut state.phys_state.phys_mem,
                UnsignedValue::<A::DataWidth>::from(heap_start).into(),
            )
            .unwrap();
    }

    pub fn insts(&self) -> &Vec<<A::Family as ArchFamily<A::DataWidth>>::Instruction> {
        &self.insts
    }

    /// Prints out all the instructions that this program contains.
    pub fn dump_insts(&self) {
        for inst in self.insts() {
            println!("{:?}", inst);
        }
    }

    /// Runs the program to completion, returning an exit code.
    /// If the program was terminated abnormally, the upper bit of the u8 will be set.
    /// The lower 7 bits are the value passed to the exit handler (or the default register for the
    /// first argument of a syscall if exit is not explicitly invoked), and will be truncated.
    pub fn run(&mut self) -> u8 {
        // for now, just use the instruction vec to determine the next instruction
        // for now, if we're out of instructions just call it a day
        // if pc dipped below pc_start, panic for now is also fine
        while let Some(inst) = self.insts.get(self.get_pc_word_index()) {
            if let Err(cause) = self.state.apply_inst(inst) {
                return cause.handle_exit(&mut self.state);
            }
        }
        let a0 =
            <A::ProgramBehavior as ProgramBehavior<A::Family, A::DataWidth>>::return_register();
        let a0_val: UnsignedValue<A::DataWidth> = self.state.regfile_read(a0).into();
        // For a non-abnormal exit, downcast to u8 and set upper bit to 0
        let val_u8 = AsPrimitive::<u8>::as_(a0_val.raw());
        val_u8 & 0b0111_1111
    }

    /// Returns the instructions provided to this program.
    pub fn get_inst_vec(&self) -> &[<A::Family as ArchFamily<A::DataWidth>>::Instruction] {
        self.insts().as_slice()
    }

    /// Returns the current state of this program.
    pub fn get_state(self) -> ProgramState<A::Family, A::DataWidth> {
        self.state
    }

    fn get_pc_word_index(&self) -> usize {
        let pc_start: ByteAddrValue<A::DataWidth> = self.text_start;
        // all this logic calculates the next address (very verbose due to generic types)
        let curr_pc: UnsignedValue<A::DataWidth> = self.state.user_state.pc.into();
        let orig_pc: UnsignedValue<A::DataWidth> = pc_start.into();
        let offs: ByteAddrValue<A::DataWidth> = curr_pc.wrapping_sub(&orig_pc).into();
        let idx: usize = offs.to_word_address().as_();
        idx
    }
}

impl<A: Architecture> FromStr for Program<A> {
    type Err = ErrorReport;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Linker::with_main_str(s).link::<A>(Default::default())
    }
}

pub struct ProgramExecutor<A: Architecture> {
    pub program: Program<A>,
    /// Represents the history of instructions executed by the program.
    /// The diffs in each instruction should be executed in sequence.
    pub inst_stack: Vec<DiffStack<A::Family, A::DataWidth>>,
    /// Represents the index of the next InstResult to be applied.
    /// For example, when this value is 1, the 0th InstResult was applied, and advancing
    /// to the next InstResult would either fail or apply the 1th InstResult.
    ///
    /// If some but not all of an InstResult's diffs were applied, then this value is one after the
    /// index of the InstResult currently being applied.
    pub curr_inst_idx: usize,
    /// Represents the index of the next StateDiff to be applied within the current InstResult.
    pub curr_step_idx: usize,
}

impl<A: Architecture> ProgramExecutor<A> {
    pub fn new(program: Program<A>) -> Self {
        ProgramExecutor {
            program,
            inst_stack: Vec::new(),
            curr_inst_idx: 0,
            curr_step_idx: 0,
        }
    }

    /// Returns a reference to the underlying program state.
    pub fn state(&self) -> &ProgramState<A::Family, A::DataWidth> {
        &self.program.state
    }

    /// Resets the program state and starts execution from the begining.
    pub fn reset(&mut self) {
        self.program.reset();
        self.inst_stack.clear();
        self.curr_inst_idx = 0;
        self.curr_step_idx = 0;
    }

    pub fn curr_inst(&self) -> Option<&<A::Family as ArchFamily<A::DataWidth>>::Instruction> {
        assert!(self.curr_inst_idx <= self.inst_stack.len());
        self.program.insts().get(self.program.get_pc_word_index())
    }

    /// Runs the next instruction of the program.
    /// Returns the exit code if the program terminates, and None otherwise.
    pub fn step(&mut self) -> Option<u8> {
        assert!(self.curr_inst_idx <= self.inst_stack.len());
        let program = &mut self.program;
        let rv = if self.curr_inst_idx == self.inst_stack.len() {
            assert!(self.curr_step_idx == 0);
            // If the inst_stack was exhausted, apply a new instruction
            // TODO gracefully handle out of bounds instructions
            // for now, if we reach an oob instruction just report the return value
            if let Some(inst) = program.insts.get(program.get_pc_word_index()) {
                let exec_result = program.state.apply_inst(inst);
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
                let a0_val: UnsignedValue<A::DataWidth> = program.state.regfile_read(a0).into();
                // For a non-abnormal exit, downcast to u8 and set upper bit to 0
                let val_u8 = AsPrimitive::<u8>::as_(a0_val.raw());
                Some(val_u8 & 0b0111_1111)
            }
        } else {
            // Run current inst to completion
            let curr_diffs = &self.inst_stack[self.curr_inst_idx];
            for curr_diff in curr_diffs.iter().skip(self.curr_step_idx) {
                self.program
                    .state
                    .apply_diff(&curr_diff)
                    // TODO
                    .unwrap();
            }
            None
        };
        // hack to get around the fact that exists aren't stored
        if rv == None {
            self.curr_inst_idx += 1;
        }
        self.curr_step_idx = 0;
        rv
    }

    /// Reverts one step of exeuction. Returns None if there are no steps to revert.
    pub fn revert(&mut self) -> Option<()> {
        // When curr_inst_idx == 1, we either fully or partially applied InstResult 0.
        // When curr_inst_idx == 0, we have applied no InstResults
        if self.curr_inst_idx == 0 {
            return None;
        }
        assert!(self.curr_inst_idx <= self.inst_stack.len());
        if self.curr_step_idx == 0 {
            // Go back one instruction
            self.curr_step_idx = self.inst_stack[self.curr_inst_idx - 1].len();
        }
        let curr_diffs = &self.inst_stack[self.curr_inst_idx - 1];
        for i in (0..self.curr_step_idx).rev() {
            self.program.state.revert_diff(&curr_diffs[i]);
        }
        self.curr_inst_idx -= 1;
        self.curr_step_idx = 0;
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
    /// TODO replace this with a function that updates executor state properly
    pub fn run(&mut self) -> u8 {
        self.program.run()
    }
}

pub struct ProgramState<F: ArchFamily<S>, S: DataWidth> {
    pub(crate) user_state: UserState<F, S>,
    pub(crate) priv_state: PrivState<S>,
    pub(crate) phys_state: PhysState,
}

impl<F: ArchFamily<S>, S: DataWidth> Default for ProgramState<F, S> {
    fn default() -> Self {
        // Pray that we never test sbrk when we use default
        ProgramState::new(1, 64, Box::new(AllMappedPt::new()))
    }
}

pub type MemGetResult<F, S, W> = (RegValue<W>, DiffStack<F, S>);

/// TODO put custom types for syscall args
/// TODO put errno on user state at a thread-local statically known location
impl<F: ArchFamily<S>, S: DataWidth> ProgramState<F, S> {
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

    pub fn get_user_pc(&self) -> ByteAddrValue<S> {
        self.user_state.pc
    }

    pub fn set_user_pc(&mut self, addr: ByteAddrValue<S>) {
        self.user_state.pc = addr
    }

    pub fn csr_read(&self, i: usize) -> RegValue<S> {
        self.priv_state.csr_read(i)
    }

    pub fn csr_write(&mut self, i: usize, value: RegValue<S>) {
        self.priv_state.csr_write(i, value);
    }

    pub fn regfile_read(&self, reg: F::Register) -> RegValue<S> {
        self.user_state.regfile.read(reg)
    }

    pub fn regfile_set(&mut self, reg: F::Register, val: RegValue<S>) {
        self.user_state.regfile.set(reg, val);
    }

    pub fn regfile(&self) -> &RegFile<F::Register, S> {
        &self.user_state.regfile
    }

    pub fn get_pc(&self) -> ByteAddrValue<S> {
        self.user_state.pc
    }

    /// Performs a read from memory with the specified data width.
    /// Returns the sequence of state updates on success, or a page fault on failure.
    pub fn memory_get<W: PageIndex>(
        &self,
        vaddr: ByteAddrValue<S>,
    ) -> Result<MemGetResult<F, S, W>, MemFault<S>> {
        // TODO how do we handle lookups spanning multiple pages? how do we handle a PT update that
        // failed on memory access due to an alignment error?
        let PtLookupData {
            diffs: pt_diffs,
            ppn,
            offs,
        } = self.priv_state.page_table.lookup_page(vaddr)?;
        let diffs: Vec<StateDiff<F, S>> = pt_diffs
            .into_iter()
            .map(PtUpdate::into_state_diff)
            .collect();
        Ok((
            self.phys_state
                .memory_get::<W>(ppn, offs)
                .map_err(|_| MemFault::buserror_at_addr(vaddr))?,
            diffs,
        ))
    }

    pub fn memory_set_unsized(
        &self,
        vaddr: ByteAddrValue<S>,
        data: DataEnum,
    ) -> Result<DiffStack<F, S>, MemFault<S>> {
        // TODO see memory_get
        let PtLookupData {
            diffs: pt_diffs,
            ppn,
            offs,
        } = self.priv_state.page_table.lookup_page(vaddr)?;
        let mut diffs: Vec<StateDiff<F, S>> = pt_diffs
            .into_iter()
            .map(PtUpdate::into_state_diff)
            .collect();
        diffs.push(
            self.phys_state
                .memory_set_unsized(ppn, offs, data)
                .map_err(|_| MemFault::buserror_at_addr(vaddr))?
                .into_state_diff(),
        );
        Ok(diffs)
    }

    /// Performs a read to memory with the specified data.
    /// Returns the sequence of state updates on success, or a page fault on failure.
    pub fn memory_set<W: PageIndex>(
        &self,
        vaddr: ByteAddrValue<S>,
        data: RegValue<W>,
    ) -> Result<DiffStack<F, S>, MemFault<S>> {
        // TODO see memory_get
        let PtLookupData {
            diffs: pt_diffs,
            ppn,
            offs,
        } = self.priv_state.page_table.lookup_page(vaddr)?;
        let mut diffs: Vec<StateDiff<F, S>> = pt_diffs
            .into_iter()
            .map(PtUpdate::into_state_diff)
            .collect();
        diffs.push(
            self.phys_state
                .memory_set(ppn, offs, data)
                .map_err(|_| MemFault::buserror_at_addr(vaddr))?
                .into_state_diff(),
        );
        Ok(diffs)
    }

    /// Used to inspect memory. Any page table updates will not be performed.
    pub fn memory_inspect_word(&self, addr: ByteAddrValue<S>) -> DataLword {
        let (v, _diffs) = self.memory_get::<W32b>(addr).unwrap();
        v
    }

    /// Sets the value in memory, performing any page table updates as needed.
    /// The intermediate diffs are not saved.
    /// Panics if the operation fails.
    pub fn memory_force_set<W: PageIndex>(&mut self, addr: ByteAddrValue<S>, data: RegValue<W>) {
        self.apply_diff_stack(self.memory_set(addr, data).unwrap())
            .unwrap();
    }

    /// Used for testing only. Any operations applied in this fashion are noninvertible, as the
    /// history stack only keeps track of full instructions.
    #[cfg(test)]
    pub fn memory_get_word(&mut self, addr: ByteAddrValue<S>) -> DataLword {
        let (v, diffs) = self.memory_get::<W32b>(addr).unwrap();
        self.apply_diff_stack(diffs).unwrap();
        v
    }

    #[cfg(test)]
    pub fn memory_set_word(&mut self, addr: ByteAddrValue<S>, val: DataLword) {
        self.apply_diff_stack(self.memory_set(addr, val).unwrap())
            .unwrap();
    }

    #[cfg(test)]
    pub fn memory_get_doubleword(&mut self, addr: ByteAddrValue<S>) -> DataDword {
        let (v, diffs) = self.memory_get::<W64b>(addr).unwrap();
        self.apply_diff_stack(diffs).unwrap();
        v
    }

    #[cfg(test)]
    pub fn memory_set_doubleword(&mut self, addr: ByteAddrValue<S>, val: DataDword) {
        self.apply_diff_stack(self.memory_set(addr, val).unwrap())
            .unwrap();
    }

    pub fn handle_trap(&self, trap_kind: &TrapKind<S>) -> InstResult<F, S> {
        match trap_kind {
            TrapKind::Ecall => self.dispatch_syscall(),
            TrapKind::MemFault(MemFault {
                user_vaddr: _,
                cause,
            }) => Ok(match cause {
                // even though the OS could attempt to map the page,
                // we requite the user to manually call brk/sbrk/mmap etc.
                MemFaultCause::PageFault => PrivDiff::Terminate(TermCause::SegFault),
                MemFaultCause::SegFault => PrivDiff::Terminate(TermCause::SegFault),
                MemFaultCause::BusError => PrivDiff::Terminate(TermCause::BusError),
            }
            .into_diff_stack()),
            _ => todo!(),
        }
    }

    pub fn dispatch_syscall(&self) -> InstResult<F, S> {
        let rf = &self.user_state.regfile;
        let syscall_number_reg = <F::Syscalls as SyscallConvention<F, S>>::syscall_number_reg();
        let arg_regs = <F::Syscalls as SyscallConvention<F, S>>::syscall_arg_regs();
        let a0 = rf.read(arg_regs[0]);
        let a1 = rf.read(arg_regs[1]);
        let a2 = rf.read(arg_regs[2]);
        if let Some(nr) = <F::Syscalls as SyscallConvention<F, S>>::number_to_syscall(
            self.user_state.regfile.read(syscall_number_reg).into(),
        ) {
            match nr {
                Syscall::Write => self.syscall_write(a0, a1.into(), a2),
                Syscall::Exit => self.syscall_exit(a0),
                Syscall::Brk => self.syscall_brk(a0.into()),
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
        fd: RegValue<S>,
        buf: ByteAddrValue<S>,
        len: RegValue<S>,
    ) -> InstResult<F, S> {
        let len_val: UnsignedValue<S> = len.into();
        let count: usize = len_val.raw().as_();
        let base_addr: UnsignedValue<S> = buf.into();
        let ret_reg = <F::Syscalls as SyscallConvention<F, S>>::syscall_return_regs()[0];
        let mut v = Vec::new();
        let bytes: Vec<u8> = (0..count)
            .map(|i| {
                u8::from({
                    let (val, diffs) = self
                        .memory_get::<W8b>((base_addr + UnsignedValue::<S>::from(i)).into())
                        .unwrap();
                    let byte: DataByte = val;
                    v.extend(diffs);
                    byte
                })
            })
            .collect();
        // Awkward here, but changing type sig makes common case less ergonomic
        v.push(PrivDiff::FileWrite { fd, data: bytes }.into_state_diff());
        v.push(UserDiff::reg_update(&self.user_state, ret_reg, len).into_state_diff());
        Ok(v)
    }

    /// Attempts to move the "program break" up to the indicated location.
    /// For us, this means the OS will attempt to page in memory up to the designated address.
    /// TODO unmap pages if brk goes down, and allocate multiple pages, also check edge case where
    /// brk lands on page boundary
    /// * addr - the address whose page should be mapped afterwards
    fn syscall_brk(&self, addr: ByteAddrValue<S>) -> InstResult<F, S> {
        let old_brk: RegValue<S> = self.priv_state.brk.into();
        let ret_reg = <F::Syscalls as SyscallConvention<F, S>>::syscall_return_regs()[0];
        if let Ok(lookup_result) = self.priv_state.page_table.lookup_page(addr) {
            let mut diffs: Vec<StateDiff<F, S>> = lookup_result
                .diffs
                .into_iter()
                .map(|u| u.into_state_diff())
                .collect();
            diffs.push(
                PrivDiff::BrkUpdate {
                    old: old_brk.into(),
                    new: addr,
                }
                .into_state_diff(),
            );
            diffs.push(
                UserDiff::reg_update(&self.user_state, ret_reg, SignedValue::<S>::zero().into())
                    .into_state_diff(),
            );
            // Update brk, return the old value, and propagate PT state changes
            Ok(diffs)
        } else if let Ok(updates) = self.priv_state.page_table.map_page(addr) {
            let mut diffs: Vec<StateDiff<F, S>> =
                updates.into_iter().map(|u| u.into_state_diff()).collect();
            diffs.push(
                PrivDiff::BrkUpdate {
                    old: old_brk.into(),
                    new: addr,
                }
                .into_state_diff(),
            );
            diffs.push(
                UserDiff::reg_update(&self.user_state, ret_reg, SignedValue::<S>::zero().into())
                    .into_state_diff(),
            );
            Ok(diffs)
        } else {
            Ok(
                UserDiff::reg_update(&self.user_state, ret_reg, SignedValue::from(-1isize).into())
                    .into_diff_stack(),
            )
        }
    }

    /// Exits the program with the provided 32-bit code.
    /// Note that the shell will only see the lower 7-bits.
    fn syscall_exit(&self, code: RegValue<S>) -> InstResult<F, S> {
        // downcast to u32 no matter what
        let val: UnsignedValue<S> = code.into();
        Ok(
            PrivDiff::Terminate(TermCause::Exit(AsPrimitive::<u32>::as_(val.raw())))
                .into_diff_stack(),
        )
    }

    /// Handles an unknown syscall.
    fn syscall_unknown(&self) -> InstResult<F, S> {
        panic!("Unknown syscall")
    }

    pub fn new(
        phys_pg_count: usize,
        pg_ofs_len: usize,
        pt: Box<dyn PageTable<S>>,
    ) -> ProgramState<F, S> {
        ProgramState {
            user_state: UserState::new(),
            // Initialize heap to zero (program initialization will set it properly)
            priv_state: PrivState::new(RegValue::<S>::zero().into(), pt),
            // TODO make endianness/alignment configurable
            phys_state: PhysState::new(Endianness::default(), true, phys_pg_count, pg_ofs_len),
        }
    }

    pub fn reset(&mut self) {
        self.user_state = UserState::new();
        self.priv_state.reset();
        self.phys_state.reset();
    }

    pub fn apply_inst(&mut self, inst: &F::Instruction) -> InstResult<F, S> {
        self.apply_diff_stack(inst.apply(self)?)
    }

    /// Asserts that applying the instruction does not fail.
    #[cfg(test)]
    pub fn apply_inst_test(&mut self, inst: &F::Instruction) {
        self.apply_diff_stack(inst.apply(self).unwrap()).unwrap();
    }

    /// Performs the provided instruction. Returns the applied instruction for ownership reasons.
    /// TODO maybe there's a bug here because if the inst terminates, what happens to the
    /// popped instresult?
    pub fn apply_diff_stack(&mut self, diffs: DiffStack<F, S>) -> InstResult<F, S> {
        for diff in &diffs {
            self.apply_diff(&diff)?;
        }
        Ok(diffs)
    }

    pub fn apply_diff(&mut self, diff: &StateDiff<F, S>) -> Result<(), TermCause> {
        match diff {
            StateDiff::User(u) => {
                self.user_state.apply_diff(u);
                Ok(())
            }
            StateDiff::Priv(p) => self
                .priv_state
                .apply_diff::<F>(&mut self.phys_state.phys_mem, p),
            StateDiff::Phys(p) => {
                self.phys_state.apply_diff(p);
                Ok(())
            }
        }
    }

    /// Reverts the described operation.
    pub fn revert_diff(&mut self, diff: &StateDiff<F, S>) {
        match diff {
            StateDiff::User(u) => self.user_state.revert_diff(u),
            StateDiff::Priv(p) => self
                .priv_state
                .revert_diff::<F>(&mut self.phys_state.phys_mem, p),
            StateDiff::Phys(p) => self.phys_state.revert_diff(p),
        }
    }
}

pub trait SyscallConvention<F: ArchFamily<S>, S: DataWidth> {
    /// Returns the syscall identified by number N, or none if no such syscall exists.
    fn number_to_syscall(n: SignedValue<S>) -> Option<Syscall>;
    /// Returns the number corresponding to the syscall, or -1 if it is unimplemented.
    fn syscall_to_number(syscall: Syscall) -> RegValue<S>;
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
pub struct RegDataChange<S: DataWidth> {
    pub old_value: RegValue<S>,
    pub new_value: RegValue<S>,
}

/// Represents a sequence of diffs produced by a single insruction.
pub type DiffStack<F, S> = Vec<StateDiff<F, S>>;

/// Represents the result of an instruction in terms of its actions on the machine state.
pub type InstResult<F, S> = Result<DiffStack<F, S>, TermCause>;

/// Represents an individual atomic change in the state of the machine.
///
/// These diffs occur at one of three levels: user, OS/kernel, and hardware.
pub enum StateDiff<F: ArchFamily<S>, S: DataWidth> {
    User(UserDiff<F, S>),
    Priv(PrivDiff<S>),
    Phys(PhysDiff),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::architectures::riscv::RiscVRegister::*;
    use crate::architectures::riscv::Rv32;

    /// Makes sure the executor can step and revert instructions.
    #[test]
    fn test_executor() {
        let code = "
            addi a0, zero, 4
            addi a1, zero, 2
            addi a0, zero, 16
            ";
        let mut executor = ProgramExecutor::<Rv32>::new(code.parse::<Program<Rv32>>().unwrap());
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

    /// Checks reset behavior of the executor.
    /// Note the code under test also checks an edge case of loading from an offset of exactly
    /// 0x0FFF_FFFC, which tests wrapping behavior for the auipc/addi expansion of addi.
    #[test]
    fn test_reset() {
        let code = "
            addi a0, zero, 5
            la t0, v
            lw a1, 0(t0)
            sw a0, 0(t0)

            .data
            v: .word 4
            ";
        let mut executor = ProgramExecutor::<Rv32>::new(code.parse::<Program<Rv32>>().unwrap());
        // Sanity check of initialization
        assert_eq!(executor.program.state.regfile_read(A0), 0u32.into());
        assert_eq!(executor.step(), None);
        assert_eq!(executor.program.state.regfile_read(A0), 5u32.into());
        assert_eq!(executor.program.state.regfile_read(A1), 0u32.into());
        // Observe after reset
        executor.reset();
        assert_eq!(executor.program.state.regfile_read(A0), 0u32.into());
        // Shouldn't be able to revert at beginning
        assert_eq!(executor.revert(), None);
        // Step 4 times to skip pseudo-op expansion of la, then the lw
        for _ in 0..4 {
            println!("{:?}", executor.curr_inst());
            assert_eq!(executor.step(), None);
        }
        // Check result of memory load
        assert_eq!(executor.program.state.regfile_read(A1), 4u32.into());
        let v_addr = executor.program.state.regfile_read(T0).as_byte_addr();
        assert_eq!(
            executor.program.state.memory_inspect_word(v_addr),
            4u32.into()
        );
        // Check result of store
        assert_eq!(executor.step(), None);
        assert_eq!(
            executor.program.state.memory_inspect_word(v_addr),
            5u32.into()
        );
        // Reset again, ensure memory reset
        executor.reset();
        assert_eq!(
            executor.program.state.memory_inspect_word(v_addr),
            4u32.into()
        );
    }
}
