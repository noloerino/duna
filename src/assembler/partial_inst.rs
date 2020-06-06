use super::parser::{LabelDef, LabelRef};
use crate::instruction::ConcreteInst;
use crate::program_state::{IRegister, MachineDataWidth};

pub(crate) enum NeededRegs<T: MachineDataWidth> {
    Two {
        assemble: fn(IRegister, IRegister, T::RegData) -> ConcreteInst<T>,
        reg1: IRegister,
        reg2: IRegister,
    },
    One {
        assemble: fn(IRegister, T::RegData) -> ConcreteInst<T>,
        reg: IRegister,
    },
    Zero {
        assemble: fn(T::RegData) -> ConcreteInst<T>,
    },
}

pub(crate) struct NeedsLabel<T: MachineDataWidth> {
    tpe: NeededRegs<T>,
    needed_label: LabelRef,
}

impl<T: MachineDataWidth> NeedsLabel<T> {
    /// Attempts to replace the needed label with the provided immediate
    pub fn fulfill_label(&self, imm: T::RegData) -> ConcreteInst<T> {
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

pub(crate) enum PartialInstType<T: MachineDataWidth> {
    Complete(ConcreteInst<T>),
    NeedsLabelRef(NeedsLabel<T>),
}

pub struct PartialInst<T: MachineDataWidth> {
    pub(crate) tpe: PartialInstType<T>,
    /// A label pointing to this instructions.
    pub label: Option<LabelDef>,
}

impl<T: MachineDataWidth> PartialInst<T> {
    pub fn new_complete(inst: ConcreteInst<T>) -> PartialInst<T> {
        PartialInst {
            tpe: PartialInstType::Complete(inst),
            label: None,
        }
    }

    fn new_needs_label(data: NeedsLabel<T>) -> PartialInst<T> {
        PartialInst {
            tpe: PartialInstType::NeedsLabelRef(data),
            label: None,
        }
    }

    pub fn new_two_reg_needs_label(
        assemble: fn(IRegister, IRegister, T::RegData) -> ConcreteInst<T>,
        reg1: IRegister,
        reg2: IRegister,
        needed: LabelRef,
    ) -> PartialInst<T> {
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
        assemble: fn(IRegister, T::RegData) -> ConcreteInst<T>,
        reg: IRegister,
        needed: LabelRef,
    ) -> PartialInst<T> {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::One { assemble, reg },
            needed_label: needed,
        })
    }

    pub fn new_no_reg_needs_label(
        assemble: fn(T::RegData) -> ConcreteInst<T>,
        needed: LabelRef,
    ) -> PartialInst<T> {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::Zero { assemble },
            needed_label: needed,
        })
    }

    /// Attaches a label to this instruction. Panics if there's already a label.
    pub fn with_label(self, label: LabelDef) -> PartialInst<T> {
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
    pub fn into_concrete_inst(self) -> Result<ConcreteInst<T>, LabelRef> {
        match self.tpe {
            PartialInstType::Complete(concrete_inst) => Ok(concrete_inst),
            PartialInstType::NeedsLabelRef(NeedsLabel { needed_label, .. }) => Err(needed_label),
        }
    }

    /// Attempts to coerce this partially-completed instruction into a ConcreteInst.
    pub fn try_into_concrete_inst(self) -> ConcreteInst<T> {
        match self.tpe {
            PartialInstType::Complete(concrete_inst) => concrete_inst,
            PartialInstType::NeedsLabelRef(NeedsLabel { needed_label, .. }) => panic!(
                "Failed to coerce into concrete instruction (missing label {})",
                needed_label.target
            ),
        }
    }
}
