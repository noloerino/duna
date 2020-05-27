use super::parser::Label;
use crate::instruction::ConcreteInst;
use crate::program_state::{DataWord, IRegister};

pub(crate) enum NeededRegs {
    Two {
        assemble: fn(IRegister, IRegister, DataWord) -> ConcreteInst,
        reg1: IRegister,
        reg2: IRegister,
    },
    One {
        assemble: fn(IRegister, DataWord) -> ConcreteInst,
        reg: IRegister,
    },
    Zero {
        assemble: fn(DataWord) -> ConcreteInst,
    },
}

pub(crate) struct NeedsLabel {
    tpe: NeededRegs,
    needed_label: Label,
}

pub(crate) enum PartialInstType {
    Complete(ConcreteInst),
    NeedsLabel(NeedsLabel),
}

pub struct PartialInst {
    pub(crate) tpe: PartialInstType,
    pub label: Option<Label>,
}

impl PartialInst {
    pub fn new_complete(inst: ConcreteInst) -> PartialInst {
        PartialInst {
            tpe: PartialInstType::Complete(inst),
            label: None,
        }
    }

    fn new_needs_label(data: NeedsLabel) -> PartialInst {
        PartialInst {
            tpe: PartialInstType::NeedsLabel(data),
            label: None,
        }
    }

    pub fn new_two_reg_needs_label(
        assemble: fn(IRegister, IRegister, DataWord) -> ConcreteInst,
        reg1: IRegister,
        reg2: IRegister,
        needed: Label,
    ) -> PartialInst {
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
        assemble: fn(IRegister, DataWord) -> ConcreteInst,
        reg: IRegister,
        needed: Label,
    ) -> PartialInst {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::One { assemble, reg },
            needed_label: needed,
        })
    }

    pub fn new_no_reg_needs_label(
        assemble: fn(DataWord) -> ConcreteInst,
        needed: Label,
    ) -> PartialInst {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeededRegs::Zero { assemble },
            needed_label: needed,
        })
    }

    /// Attaches a label to this instruction. Panics if there's already a label.
    pub fn with_label(self, label: Label) -> PartialInst {
        match self.label {
            None => PartialInst {
                tpe: self.tpe,
                label: Some(label),
            },
            Some(_) => panic!("instruction already had label"),
        }
    }

    pub fn get_needed_label(&self) -> Option<&Label> {
        match &self.tpe {
            PartialInstType::NeedsLabel(NeedsLabel { needed_label, .. }) => Some(&needed_label),
            PartialInstType::Complete(..) => None,
        }
    }

    /// Attempts to coerce this partially-completed instruction into a ConcreteInst.
    pub fn try_into_concrete_inst(self) -> ConcreteInst {
        use PartialInstType::*;
        if let Complete(concrete_inst) = self.tpe {
            concrete_inst
        } else {
            panic!(
                "Failed to coerce into concrete instruction (missing label {:?})",
                self.get_needed_label()
            )
        }
    }

    /// Attempts to replace the needed label with the provided immediate
    /// TODO move this onto NeedsLabel instead?
    pub fn fulfill_label(&self, imm: DataWord) -> ConcreteInst {
        use NeededRegs::*;
        use PartialInstType::*;
        match &self.tpe {
            NeedsLabel(data) => match data.tpe {
                Two {
                    assemble,
                    reg1,
                    reg2,
                } => assemble(reg1, reg2, imm),
                One { assemble, reg } => assemble(reg, imm),
                Zero { assemble } => assemble(imm),
            },
            Complete(..) => panic!("Cannot fulfill label for complete instruction"),
        }
    }
}
