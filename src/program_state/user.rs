//! Represents the user state of a program: the registers and program counter.
//!
//! Although the address space of a program is accessible to a user program, the actual data
//! resides in physical memory and must be accessed through the page table. We're see where that
//! ends up I guess.

use super::datatypes::*;
use super::memory::*;
use super::program::{InstResult, RegDataChange, StateDiff};
use super::registers::*;
use crate::arch::*;

/// Contains program state that is visible to the user.
pub struct UserState<F: ArchFamily<T>, T: MachineDataWidth> {
    pub pc: T::ByteAddr,
    pub regfile: RegFile<F::Register, T>,
    pub memory: Box<dyn Memory<T::ByteAddr>>,
}

#[cfg(test)]
impl<F: ArchFamily<T>, T: MachineDataWidth> Default for UserState<F, T> {
    fn default() -> Self {
        UserState::new(Box::new(SimpleMemory::new()))
    }
}

impl<F: ArchFamily<T>, T: MachineDataWidth> UserState<F, T> {
    pub fn new(memory: Box<dyn Memory<T::ByteAddr>>) -> Self {
        UserState {
            pc: T::sgn_zero().into(),
            regfile: RegFile::new(),
            memory,
        }
    }

    /// Applies a diff to the user state.
    pub fn apply_diff(&mut self, diff: &UserDiff<F, T>) {
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
            UserDiff::MemDiff { addr, new_val, .. } => self.memory.set(addr, new_val).unwrap(),
            // Trap itself is a noop, but instruction may produce other side effects
            UserDiff::Trap(_trap_kind) => {}
        }
    }

    pub fn revert_diff(&mut self, diff: &UserDiff<F, T>) {
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
            UserDiff::MemDiff { addr, old_val, .. } => self.memory.set(addr, old_val).unwrap(),
            UserDiff::Trap(_trap_kind) => {}
        }
    }
}

/// Represents an atomic diff that is applied only to the user state of a program.
/// Since traps are synchronous, they're included in here - they can be thought of as a context
/// switch operation.
pub enum UserDiff<F: ArchFamily<T>, T: MachineDataWidth> {
    PcDiff {
        old_pc: T::ByteAddr,
        new_pc: T::ByteAddr,
    },
    RegDiff {
        reg: F::Register,
        change: RegDataChange<T>,
    },
    MemDiff {
        addr: T::ByteAddr,
        old_val: DataEnum,
        new_val: DataEnum,
    },
    Trap(TrapKind<T::ByteAddr>),
}

impl<F: ArchFamily<T>, T: MachineDataWidth> UserDiff<F, T> {
    pub fn into_state_diff(self) -> StateDiff<F, T> {
        StateDiff::User(self)
    }

    pub fn into_inst_result(self) -> InstResult<F, T> {
        InstResult::new(vec![self.into_state_diff()])
    }

    /// Advances the program counter by 4.
    pub fn pc_p4(state: &UserState<F, T>) -> Self {
        UserDiff::PcDiff {
            old_pc: state.pc,
            new_pc: state.pc.plus_4(),
        }
    }

    pub fn pc_update_op(state: &UserState<F, T>, new_pc: T::ByteAddr) -> InstResult<F, T> {
        InstResult::new(vec![UserDiff::PcDiff {
            old_pc: state.pc,
            new_pc,
        }
        .into_state_diff()])
    }

    pub fn reg_write_op(
        state: &UserState<F, T>,
        new_pc: T::ByteAddr,
        reg: F::Register,
        rd_val: T::RegData,
    ) -> InstResult<F, T> {
        InstResult::new(
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
            .collect(),
        )
    }

    pub fn reg_write_pc_p4(
        state: &UserState<F, T>,
        reg: F::Register,
        val: T::RegData,
    ) -> InstResult<F, T> {
        UserDiff::reg_write_op(state, state.pc.plus_4(), reg, val)
    }

    /// Performs a memory write operation.
    /// This may trap to the OS in the event of exceptional events like a page fault.
    pub fn mem_write_op(
        state: &UserState<F, T>,
        addr: T::ByteAddr,
        val: DataEnum,
    ) -> Result<InstResult<F, T>, MemFault<T::ByteAddr>> {
        Ok(InstResult::new(
            vec![
                UserDiff::MemDiff {
                    addr,
                    old_val: state.memory.get(addr, val.width())?,
                    new_val: val,
                },
                UserDiff::pc_p4(state),
            ]
            .into_iter()
            .map(|diff| diff.into_state_diff())
            .collect(),
        ))
    }
}

/// Represents the type of trap being raised from user mode.
/// See "Machine Cause Register" in the RISCV privileged spec for details.
#[derive(Copy, Clone)]
pub enum TrapKind<T: ByteAddress> {
    /// Corresponds to an ecall instruction issued from user mode.
    Ecall,
    MemFault(MemFault<T>),
}

/// Converts a memory fault into a trap.
impl<T: ByteAddress> From<MemFault<T>> for TrapKind<T> {
    fn from(fault: MemFault<T>) -> TrapKind<T> {
        TrapKind::MemFault(fault)
    }
}