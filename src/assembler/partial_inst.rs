use super::parser::{LabelDef, LabelRef};
use crate::arch::*;
use crate::instruction::ConcreteInst;
use crate::program_state::IRegister;

pub(crate) enum NeededRegs<R: IRegister, S: ConcreteInst<R, T>, T: MachineDataWidth> {
    Two {
        assemble: fn(R, R, T::RegData) -> S,
        reg1: R,
        reg2: R,
    },
    One {
        assemble: fn(R, T::RegData) -> S,
        reg: R,
    },
    Zero {
        assemble: fn(T::RegData) -> S,
    },
}

pub(crate) struct NeedsLabel<R: IRegister, S: ConcreteInst<R, T>, T: MachineDataWidth> {
    tpe: NeededRegs<R, S, T>,
    needed_label: LabelRef,
}

impl<R: IRegister, S: ConcreteInst<R, T>, T: MachineDataWidth> NeedsLabel<R, S, T> {
    /// Attempts to replace the needed label with the provided immediate
    pub fn fulfill_label(&self, imm: T::RegData) -> S {
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

pub(crate) enum PartialInstType<R: IRegister, S: ConcreteInst<R, T>, T: MachineDataWidth> {
    Complete(S),
    NeedsLabelRef(NeedsLabel<R, S, T>),
}

pub struct PartialInst<R: IRegister, S: ConcreteInst<R, T>, T: MachineDataWidth> {
    pub(crate) tpe: PartialInstType<R, S, T>,
    /// A label pointing to this instructions.
    pub label: Option<LabelDef>,
}

impl<R: IRegister, S: ConcreteInst<R, T>, T: MachineDataWidth> PartialInst<R, S, T> {
    pub fn new_complete(inst: S) -> PartialInst<R, S, T> {
        PartialInst {
            tpe: PartialInstType::Complete(inst),
            label: None,
        }
    }

    fn new_needs_label(data: NeedsLabel<R, S, T>) -> PartialInst<R, S, T> {
        PartialInst {
            tpe: PartialInstType::NeedsLabelRef(data),
            label: None,
        }
    }

    pub fn new_two_reg_needs_label(
        assemble: fn(R, R, T::RegData) -> S,
        reg1: R,
        reg2: R,
        needed: LabelRef,
    ) -> PartialInst<R, S, T> {
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
        assemble: fn(R, T::RegData) -> S,
        reg: R,
        needed: LabelRef,
    ) -> PartialInst<R, S, T> {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::One { assemble, reg },
            needed_label: needed,
        })
    }

    pub fn new_no_reg_needs_label(
        assemble: fn(T::RegData) -> S,
        needed: LabelRef,
    ) -> PartialInst<R, S, T> {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::Zero { assemble },
            needed_label: needed,
        })
    }

    /// Attaches a label to this instruction. Panics if there's already a label.
    pub fn with_label(self, label: LabelDef) -> PartialInst<R, S, T> {
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
    pub fn into_concrete_inst(self) -> Result<S, LabelRef> {
        match self.tpe {
            PartialInstType::Complete(concrete_inst) => Ok(concrete_inst),
            PartialInstType::NeedsLabelRef(NeedsLabel { needed_label, .. }) => Err(needed_label),
        }
    }

    /// Attempts to coerce this partially-completed instruction into a ConcreteInst.
    pub fn try_into_concrete_inst(self) -> S {
        match self.tpe {
            PartialInstType::Complete(concrete_inst) => concrete_inst,
            PartialInstType::NeedsLabelRef(NeedsLabel { needed_label, .. }) => panic!(
                "Failed to coerce into concrete instruction (missing label {})",
                needed_label.target
            ),
        }
    }
}
