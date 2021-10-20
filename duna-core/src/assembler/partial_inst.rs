use super::parser::{LabelDef, LabelRef};
use crate::{arch::*, data_structures::*};

pub(crate) enum NeededRegs<F: ArchFamily<S>, S: DataWidth> {
    Two {
        assemble: fn(F::Register, F::Register, RegValue<S>) -> F::Instruction,
        reg1: F::Register,
        reg2: F::Register,
    },
    One {
        assemble: fn(F::Register, RegValue<S>) -> F::Instruction,
        reg: F::Register,
    },
    Zero {
        assemble: fn(RegValue<S>) -> F::Instruction,
    },
}

pub(crate) struct NeedsLabel<F: ArchFamily<S>, S: DataWidth> {
    tpe: NeededRegs<F, S>,
    needed_label: LabelRef,
}

impl<F: ArchFamily<S>, S: DataWidth> NeedsLabel<F, S> {
    /// Attempts to replace the needed label with the provided immediate
    pub fn fulfill_label(&self, imm: RegValue<S>) -> F::Instruction {
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

pub(crate) enum PartialInstType<F: ArchFamily<S>, S: DataWidth> {
    Complete(F::Instruction),
    NeedsLabelRef(NeedsLabel<F, S>),
}

pub struct PartialInst<F: ArchFamily<S>, S: DataWidth> {
    pub(crate) tpe: PartialInstType<F, S>,
    /// A label pointing to this instructions.
    pub label: Option<LabelDef>,
}

impl<F: ArchFamily<S>, S: DataWidth> PartialInst<F, S> {
    pub fn new_complete(inst: F::Instruction) -> PartialInst<F, S> {
        PartialInst {
            tpe: PartialInstType::Complete(inst),
            label: None,
        }
    }

    fn new_needs_label(data: NeedsLabel<F, S>) -> PartialInst<F, S> {
        PartialInst {
            tpe: PartialInstType::NeedsLabelRef(data),
            label: None,
        }
    }

    pub fn new_two_reg_needs_label(
        assemble: fn(F::Register, F::Register, RegValue<S>) -> F::Instruction,
        reg1: F::Register,
        reg2: F::Register,
        needed: LabelRef,
    ) -> PartialInst<F, S> {
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
        assemble: fn(F::Register, RegValue<S>) -> F::Instruction,
        reg: F::Register,
        needed: LabelRef,
    ) -> PartialInst<F, S> {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::One { assemble, reg },
            needed_label: needed,
        })
    }

    pub fn new_no_reg_needs_label(
        assemble: fn(RegValue<S>) -> F::Instruction,
        needed: LabelRef,
    ) -> PartialInst<F, S> {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::Zero { assemble },
            needed_label: needed,
        })
    }

    /// Attaches a label to this instruction. Panics if there's already a label.
    pub fn with_label(self, label: LabelDef) -> PartialInst<F, S> {
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
            PartialInstType::NeedsLabelRef(NeedsLabel { needed_label, .. }) => Some(needed_label),
            PartialInstType::Complete(..) => None,
        }
    }

    /// Creates a concrete inst, or returns the needed label on error.
    pub fn into_concrete_inst(self) -> Result<F::Instruction, LabelRef> {
        match self.tpe {
            PartialInstType::Complete(concrete_inst) => Ok(concrete_inst),
            PartialInstType::NeedsLabelRef(NeedsLabel { needed_label, .. }) => Err(needed_label),
        }
    }

    /// Attempts to coerce this partially-completed instruction into a ConcreteInst.
    pub fn try_into_concrete_inst(self) -> F::Instruction {
        match self.tpe {
            PartialInstType::Complete(concrete_inst) => concrete_inst,
            PartialInstType::NeedsLabelRef(NeedsLabel { needed_label, .. }) => panic!(
                "Failed to coerce into concrete instruction (missing label {})",
                needed_label.target
            ),
        }
    }
}
