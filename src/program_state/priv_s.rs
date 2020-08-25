//! Represents the privileged state of a program: anything manipulated by the OS/kernel pertaining
//! to the running process, such as file descriptors and the page table.

use super::datatypes::*;
use super::memory::*;
use super::program::{InstResult, ProgramState, StateDiff};
use crate::arch::*;

/// Contains program state that is visited only to privileged entities, i.e. a kernel thread.
/// TODO add kernel thread information (tid, file descriptors, etc.)
pub struct PrivState<T: MachineDataWidth> {
    pub page_table: Box<dyn PageTable<T::ByteAddr>>,
    /// Holds the contents of all bytes that have been printed to stdout (used mostly for testing)
    pub(crate) stdout: Vec<u8>,
    pub(crate) stderr: Vec<u8>,
    // file_descriptors: Vec<Vec<u8>>
}

impl<T: MachineDataWidth> Default for PrivState<T> {
    fn default() -> Self {
        PrivState::new()
    }
}

impl PrivState {
    pub fn new() -> Self {
        PrivState {
            stdout: Vec::new(),
            stderr: Vec::new(),
        }
    }

    /// Applies a diff to the privileged state.
    ///
    /// Note that even for operations that seem like they would necessitate changes in user space
    /// (e.g. the write() syscall returns the number of bytes written), no user space changes are
    /// applied because the caller of this function will have determined the result at call time
    /// and generated an appropriate UserDiff.
    ///
    /// Returns a TermCause if the program is terminated.
    pub fn apply_diff<F: ArchFamily<T>, T: MachineDataWidth>(
        &mut self,
        diff: &PrivDiff<T>,
    ) -> Result<(), TermCause> {
        use PrivDiff::*;
        match diff {
            FileWrite { fd, data } => {
                // TODO impl for other files
                let fd_idx: usize = {
                    let num: T::Unsigned = (*fd).into();
                    T::usgn_to_usize(num)
                };
                match fd_idx {
                    1 => {
                        print!("{}", String::from_utf8_lossy(&data));
                        self.stdout.extend(data);
                    }
                    2 => {
                        eprint!("{}", String::from_utf8_lossy(&data));
                        self.stderr.extend(data);
                    }
                    _ => unimplemented!(),
                }
                Ok(())
            }
            Terminate(cause) => Err(*cause),
            PtUpdate(update) => self.page_table.apply_update(),
        }
    }

    /// Reverts a privileged state change.
    pub fn revert_diff<F: ArchFamily<T>, T: MachineDataWidth>(&mut self, diff: &PrivDiff<T>) {
        use PrivDiff::*;
        match diff {
            // TODO delete last len bytes from fd
            FileWrite { fd: _, data: _ } => {}
            PtUpdate(update) => self.page_table.revert_update(),
            _ => unimplemented!(),
        }
    }
}

/// Encodes a change that occurred to the state of the privileged aspects of a program,
/// such as a write to a file.
pub enum PrivDiff<T: MachineDataWidth> {
    /// Indicates that the program is to be terminated.
    Terminate(TermCause),
    /// Represents a file write.
    /// * fd: the file descriptor
    /// * data: the bytes being written
    FileWrite {
        fd: T::RegData,
        data: Vec<u8>,
    },
    PtUpdate(PteUpdate),
}

impl<T: MachineDataWidth> PrivDiff<T> {
    pub fn into_state_diff<F: ArchFamily<T>>(self) -> StateDiff<F, T> {
        StateDiff::Priv(self)
    }

    pub fn into_inst_result<F: ArchFamily<T>>(self) -> InstResult<F, T> {
        InstResult::new(vec![self.into_state_diff()])
    }
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
