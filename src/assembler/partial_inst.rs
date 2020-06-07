use super::parser::{LabelDef, LabelRef};
use crate::arch::*;

pub(crate) enum NeededRegs<S: Architecture<T>, T: MachineDataWidth> {
    Two {
        assemble: fn(S::Register, S::Register, T::RegData) -> S::Instruction,
        reg1: S::Register,
        reg2: S::Register,
    },
    One {
        assemble: fn(S::Register, T::RegData) -> S::Instruction,
        reg: S::Register,
    },
    Zero {
        assemble: fn(T::RegData) -> S::Instruction,
    },
}

pub(crate) struct NeedsLabel<S: Architecture<T>, T: MachineDataWidth> {
    tpe: NeededRegs<S, T>,
    needed_label: LabelRef,
}

impl<S: Architecture<T>, T: MachineDataWidth> NeedsLabel<S, T> {
    /// Attempts to replace the needed label with the provided immediate
    pub fn fulfill_label(&self, imm: T::RegData) -> S::Instruction {
        use NeededRegs::*;
        match self.tpe {
            Two {
                assemble,
                reg1,
                reg2,
            } => assemble(reg1, reg2, imm),
            One { assemble, reg } => assemble(reg, imm),
            Zero { assemble } => assemble(imm),
        }
    }
}

pub(crate) enum PartialInstType<S: Architecture<T>, T: MachineDataWidth> {
    Complete(S::Instruction),
    NeedsLabelRef(NeedsLabel<S, T>),
}

pub struct PartialInst<S: Architecture<T>, T: MachineDataWidth> {
    pub(crate) tpe: PartialInstType<S, T>,
    /// A label pointing to this instructions.
    pub label: Option<LabelDef>,
}

impl<S: Architecture<T>, T: MachineDataWidth> PartialInst<S, T> {
    pub fn new_complete(inst: S::Instruction) -> PartialInst<S, T> {
        PartialInst {
            tpe: PartialInstType::Complete(inst),
            label: None,
        }
    }

    fn new_needs_label(data: NeedsLabel<S, T>) -> PartialInst<S, T> {
        PartialInst {
            tpe: PartialInstType::NeedsLabelRef(data),
            label: None,
        }
    }

    pub fn new_two_reg_needs_label(
        assemble: fn(S::Register, S::Register, T::RegData) -> S::Instruction,
        reg1: S::Register,
        reg2: S::Register,
        needed: LabelRef,
    ) -> PartialInst<S, T> {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::Two {
                assemble,
                reg1,
                reg2,
            },
            needed_label: needed,
        })
    }

    pub fn new_one_reg_needs_label(
        assemble: fn(S::Register, T::RegData) -> S::Instruction,
        reg: S::Register,
        needed: LabelRef,
    ) -> PartialInst<S, T> {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::One { assemble, reg },
            needed_label: needed,
        })
    }

    pub fn new_no_reg_needs_label(
        assemble: fn(T::RegData) -> S::Instruction,
        needed: LabelRef,
    ) -> PartialInst<S, T> {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::Zero { assemble },
            needed_label: needed,
        })
    }

    /// Attaches a label to this instruction. Panics if there's already a label.
    pub fn with_label(self, label: LabelDef) -> PartialInst<S, T> {
        match self.label {
            None => PartialInst {
                tpe: self.tpe,
                label: Some(label),
            },
            Some(_) => panic!("instruction already had label"),
        }
    }

    pub fn get_needed_label(&self) -> Option<&LabelRef> {
        match &self.tpe {
            PartialInstType::NeedsLabelRef(NeedsLabel { needed_label, .. }) => Some(&needed_label),
            PartialInstType::Complete(..) => None,
        }
    }

    /// Creates a concrete inst, or returns the needed label on error.
    pub fn into_concrete_inst(self) -> Result<S::Instruction, LabelRef> {
        match self.tpe {
            PartialInstType::Complete(concrete_inst) => Ok(concrete_inst),
            PartialInstType::NeedsLabelRef(NeedsLabel { needed_label, .. }) => Err(needed_label),
        }
    }

    /// Attempts to coerce this partially-completed instruction into a ConcreteInst.
    pub fn try_into_concrete_inst(self) -> S::Instruction {
        match self.tpe {
            PartialInstType::Complete(concrete_inst) => concrete_inst,
            PartialInstType::NeedsLabelRef(NeedsLabel { needed_label, .. }) => panic!(
                "Failed to coerce into concrete instruction (missing label {})",
                needed_label.target
            ),
        }
    }
}
