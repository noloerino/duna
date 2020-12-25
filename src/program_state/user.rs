//! Represents the user state of a program: the registers and program counter.
//!
//! Although the address space of a program is accessible to a user program, the actual data
//! resides in physical memory and must be accessed through the page table. We're see where that
//! ends up I guess.

use super::datatypes::*;
use super::memory::*;
use super::program::{DiffStack, ProgramState, RegDataChange, StateDiff};
use super::registers::*;
use crate::arch::*;

/// Contains program state that is visible to the user.
pub struct UserState<F: ArchFamily<S>, S: Data> {
    pub pc: ByteAddrValue<S>,
    pub regfile: RegFile<F::Register, S>,
}

impl<F: ArchFamily<S>, S: Data> Default for UserState<F, S> {
    fn default() -> Self {
        UserState::new()
    }
}

impl<F: ArchFamily<S>, S: Data> UserState<F, S> {
    pub fn new() -> Self {
        UserState {
            pc: SignedValue::<S>::zero().as_byte_addr(),
            regfile: RegFile::new(),
        }
    }

    /// Applies a diff to the user state.
    pub fn apply_diff(&mut self, diff: &UserDiff<F, S>) {
        match *diff {
            UserDiff::PcDiff { new_pc, .. } => {
                self.pc = new_pc;
            }
            UserDiff::RegDiff {
                reg,
                change: RegDataChange { new_value, .. },
            } => {
                self.regfile.set(reg, new_value);
            }
            // Trap itself is a noop, but instruction may produce other side effects
            UserDiff::Trap(_trap_kind) => {}
        }
    }

    pub fn revert_diff(&mut self, diff: &UserDiff<F, S>) {
        match *diff {
            UserDiff::PcDiff { old_pc, .. } => {
                self.pc = old_pc;
            }
            UserDiff::RegDiff {
                reg,
                change: RegDataChange { old_value, .. },
            } => {
                self.regfile.set(reg, old_value);
            }
            UserDiff::Trap(_trap_kind) => {}
        }
    }
}

/// Represents an atomic diff that is applied only to the user state of a program.
/// Since traps are synchronous, they're included in here - they can be thought of as a context
/// switch operation.
pub enum UserDiff<F: ArchFamily<S>, S: Data> {
    PcDiff {
        old_pc: ByteAddrValue<S>,
        new_pc: ByteAddrValue<S>,
    },
    RegDiff {
        reg: F::Register,
        change: RegDataChange<S>,
    },
    Trap(TrapKind<S>),
}

impl<F: ArchFamily<S>, S: Data> UserDiff<F, S> {
    pub fn into_state_diff(self) -> StateDiff<F, S> {
        StateDiff::User(self)
    }

    pub fn into_diff_stack(self) -> DiffStack<F, S> {
        vec![self.into_state_diff()]
    }

    /// Advances the program counter by 4.
    pub fn pc_p4(state: &UserState<F, S>) -> Self {
        UserDiff::PcDiff {
            old_pc: state.pc,
            new_pc: state.pc.plus_4(),
        }
    }

    pub fn pc_update_op(state: &UserState<F, S>, new_pc: ByteAddrValue<S>) -> DiffStack<F, S> {
        vec![UserDiff::PcDiff {
            old_pc: state.pc,
            new_pc,
        }
        .into_state_diff()]
    }

    pub fn reg_update(state: &UserState<F, S>, reg: F::Register, rd_val: RegValue<S>) -> Self {
        UserDiff::RegDiff {
            reg,
            change: RegDataChange {
                old_value: state.regfile.read(reg),
                new_value: rd_val,
            },
        }
    }

    pub fn reg_write_op(
        state: &UserState<F, S>,
        new_pc: ByteAddrValue<S>,
        reg: F::Register,
        rd_val: RegValue<S>,
    ) -> DiffStack<F, S> {
        vec![
            UserDiff::RegDiff {
                reg,
                change: RegDataChange {
                    old_value: state.regfile.read(reg),
                    new_value: rd_val,
                },
            },
            UserDiff::PcDiff {
                old_pc: state.pc,
                new_pc,
            },
        ]
        .into_iter()
        .map(|diff| diff.into_state_diff())
        .collect()
    }

    pub fn reg_write_pc_p4(
        state: &UserState<F, S>,
        reg: F::Register,
        val: RegValue<S>,
    ) -> DiffStack<F, S> {
        UserDiff::reg_write_op(state, state.pc.plus_4(), reg, val)
    }

    pub fn mem_write_pc_p4(
        state: &ProgramState<F, S>,
        addr: ByteAddrValue<S>,
        val: DataEnum,
    ) -> Result<DiffStack<F, S>, MemFault<S>> {
        let mut diffs = state.memory_set_unsized(addr, val)?;
        diffs.push(UserDiff::pc_p4(&state.user_state).into_state_diff());
        Ok(diffs)
    }
}

/// Represents the type of trap being raised from user mode.
/// See "Machine Cause Register" in the RISCV privileged spec for details.
#[derive(Copy, Clone)]
pub enum TrapKind<S: Data> {
    /// Corresponds to an ecall instruction issued from user mode.
    Ecall,
    MemFault(MemFault<S>),
}

/// Converts a memory fault into a trap.
impl<S: Data> From<MemFault<S>> for TrapKind<S> {
    fn from(fault: MemFault<S>) -> TrapKind<S> {
        TrapKind::MemFault(fault)
    }
}
