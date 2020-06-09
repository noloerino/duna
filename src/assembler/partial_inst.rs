use super::parser::{LabelDef, LabelRef};
use crate::arch::*;

pub(crate) enum NeededRegs<S: Architecture> {
    Two {
        assemble: fn(
            S::Register,
            S::Register,
            <S::DataWidth as MachineDataWidth>::RegData,
        ) -> S::Instruction,
        reg1: S::Register,
        reg2: S::Register,
    },
    One {
        assemble: fn(S::Register, <S::DataWidth as MachineDataWidth>::RegData) -> S::Instruction,
        reg: S::Register,
    },
    Zero {
        assemble: fn(<S::DataWidth as MachineDataWidth>::RegData) -> S::Instruction,
    },
}

pub(crate) struct NeedsLabel<S: Architecture> {
    tpe: NeededRegs<S>,
    needed_label: LabelRef,
}

impl<S: Architecture> NeedsLabel<S> {
    /// Attempts to replace the needed label with the provided immediate
    pub fn fulfill_label(
        &self,
        imm: <S::DataWidth as MachineDataWidth>::RegData,
    ) -> S::Instruction {
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

pub(crate) enum PartialInstType<S: Architecture> {
    Complete(S::Instruction),
    NeedsLabelRef(NeedsLabel<S>),
}

pub struct PartialInst<S: Architecture> {
    pub(crate) tpe: PartialInstType<S>,
    /// A label pointing to this instructions.
    pub label: Option<LabelDef>,
}

impl<S: Architecture> PartialInst<S> {
    pub fn new_complete(inst: S::Instruction) -> PartialInst<S> {
        PartialInst {
            tpe: PartialInstType::Complete(inst),
            label: None,
        }
    }

    fn new_needs_label(data: NeedsLabel<S>) -> PartialInst<S> {
        PartialInst {
            tpe: PartialInstType::NeedsLabelRef(data),
            label: None,
        }
    }

    pub fn new_two_reg_needs_label(
        assemble: fn(
            S::Register,
            S::Register,
            <S::DataWidth as MachineDataWidth>::RegData,
        ) -> S::Instruction,
        reg1: S::Register,
        reg2: S::Register,
        needed: LabelRef,
    ) -> PartialInst<S> {
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
        assemble: fn(S::Register, <S::DataWidth as MachineDataWidth>::RegData) -> S::Instruction,
        reg: S::Register,
        needed: LabelRef,
    ) -> PartialInst<S> {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::One { assemble, reg },
            needed_label: needed,
        })
    }

    pub fn new_no_reg_needs_label(
        assemble: fn(<S::DataWidth as MachineDataWidth>::RegData) -> S::Instruction,
        needed: LabelRef,
    ) -> PartialInst<S> {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::Zero { assemble },
            needed_label: needed,
        })
    }

    /// Attaches a label to this instruction. Panics if there's already a label.
    pub fn with_label(self, label: LabelDef) -> PartialInst<S> {
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
