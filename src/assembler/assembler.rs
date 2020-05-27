use super::lexer::Lexer;
use super::parser::ParseError;
use super::parser::RiscVParser;
use crate::instruction::ConcreteInst;
use crate::program_state::{DataWord, IRegister, RiscVProgram};
use std::collections::HashMap;
use std::mem;

pub struct Assembler {
    lexer: Lexer,
}

impl Assembler {
    pub fn from_file(path: &str) -> Assembler {
        Assembler {
            lexer: Lexer::from_file(path),
        }
    }

    pub fn from_string(contents: String) -> Assembler {
        Assembler {
            lexer: Lexer::from_string(contents),
        }
    }

    pub fn assemble(self) -> Result<UnlinkedProgram, Vec<ParseError>> {
        let (toks, lex_errs) = self.lexer.lex();
        let (insts, parse_errs) = RiscVParser::from_tokens(toks).parse();
        let mut all_errs = lex_errs;
        all_errs.extend(parse_errs);
        if all_errs.is_empty() {
            Ok(UnlinkedProgram::new(insts))
        } else {
            Err(all_errs)
        }
    }
}

pub type Label = String;

pub(crate) enum NeedsLabelType {
    TwoReg {
        assemble: fn(IRegister, IRegister, DataWord) -> ConcreteInst,
        reg1: IRegister,
        reg2: IRegister,
    },
    OneReg {
        assemble: fn(IRegister, DataWord) -> ConcreteInst,
        reg: IRegister,
    },
    ZeroReg {
        assemble: fn(DataWord) -> ConcreteInst,
    },
}

pub(crate) struct NeedsLabel {
    tpe: NeedsLabelType,
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
            tpe: NeedsLabelType::TwoReg {
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
            tpe: NeedsLabelType::OneReg { assemble, reg },
            needed_label: needed,
        })
    }

    pub fn new_no_reg_needs_label(
        assemble: fn(DataWord) -> ConcreteInst,
        needed: Label,
    ) -> PartialInst {
        PartialInst::new_needs_label(NeedsLabel {
            tpe: NeedsLabelType::ZeroReg { assemble },
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
            panic!("Failed to coerce into concrete instruction")
        }
    }

    /// Attempts to replace the needed label with the provided immediate
    /// TODO move this onto NeedsLabel instead?
    pub fn fulfill_label(&self, imm: DataWord) -> ConcreteInst {
        use NeedsLabelType::*;
        use PartialInstType::*;
        match &self.tpe {
            NeedsLabel(data) => match &data.tpe {
                &TwoReg {
                    assemble,
                    reg1,
                    reg2,
                } => assemble(reg1, reg2, imm),
                &OneReg { assemble, reg } => assemble(reg, imm),
                &ZeroReg { assemble } => assemble(imm),
            },
            Complete(..) => panic!("Cannot fulfill label for complete instruction"),
        }
    }
}

/// Replaces index i of the instruction vector with new_inst
fn replace_inst(insts: &mut Vec<PartialInst>, i: usize, new_inst: PartialInst) {
    mem::replace(&mut insts[i], new_inst);
}

/// The parser must perform two passes in order to locate/process labels.
/// This struct encodes data for a program that still needs to be passed to the assembler.
pub struct UnlinkedProgram {
    pub insts: Vec<PartialInst>,
    /// Maps labels to the index of the insts that define them
    local_labels: HashMap<Label, usize>,
    /// Maps needed labels to the index of the insts that need them
    pub needed_labels: HashMap<Label, usize>,
    // a potential optimization is to store generated labels and needed labels in independent vecs
    // instead of a hashmap, another vec can be used to lookup the corresponding PartialInst
}

impl UnlinkedProgram {
    /// Constructs an instance of an UnlinkedProgram from an instruction stream.
    pub fn new(insts: Vec<PartialInst>) -> UnlinkedProgram {
        let local_labels = insts
            .iter()
            .enumerate()
            .filter_map(|(i, partial_inst)| {
                partial_inst.label.as_ref().map(|label| (label.clone(), i))
            })
            .collect();
        let needed_labels = insts
            .iter()
            .enumerate()
            .filter_map(|(i, partial_inst)| (Some((partial_inst.get_needed_label()?.clone(), i))))
            .collect();
        UnlinkedProgram {
            insts,
            local_labels,
            needed_labels,
        }
    }

    /// Attempts to match needed labels to locally defined labels.
    /// Theoretically, this is idempotent, i.e. calling it multiple times will just produce the
    /// same program.
    pub fn link_self(self) -> UnlinkedProgram {
        let UnlinkedProgram {
            mut insts,
            local_labels,
            needed_labels,
        } = self;
        let mut unresolved_labels = HashMap::new();
        for (label, inst_index) in needed_labels.into_iter() {
            match local_labels.get(&label) {
                Some(&tgt_index) => {
                    // Figure out how many instructions we need to jump
                    let inst_distance = (tgt_index as isize) - (inst_index as isize);
                    let byte_distance: isize = inst_distance * 4;
                    let new_inst =
                        insts[inst_index].fulfill_label(DataWord::from(byte_distance as i32));
                    replace_inst(&mut insts, inst_index, PartialInst::new_complete(new_inst));
                }
                None => {
                    unresolved_labels.insert(label, inst_index);
                }
            }
        }
        UnlinkedProgram {
            insts,
            local_labels,
            needed_labels: unresolved_labels,
        }
    }

    /// Attempts to produce an instance of RiscVProgram. Panics if some labels are needed
    /// but not found within the body of this program.
    pub fn try_into_program(self) -> RiscVProgram {
        use PartialInstType::*;
        let linked = self.link_self();
        RiscVProgram::new(
            linked
                .insts
                .into_iter()
                .map(|partial_inst| match partial_inst.tpe {
                    Complete(inst) => inst,
                    _ => panic!("inst needed label {:?}", partial_inst.get_needed_label()),
                })
                .collect(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::BType;
    use crate::isa::Beq;
    use crate::program_state::IRegister::ZERO;

    #[test]
    fn test_forward_local_label() {
        let program = "beq x0, x0, l1\nnop\nnop\nl1:nop";
        let unlinked = Assembler::from_string(program.to_string())
            .assemble()
            .expect("Assembler errored out");
        // should automatically attempt to link
        let concrete = unlinked.try_into_program();
        println!("{:?}", concrete.insts);
        assert_eq!(concrete.insts[0], Beq::new(ZERO, ZERO, DataWord::from(12)));
    }

    #[test]
    fn test_backward_local_label() {
        let program = "\nl1:nop\nnop\nnop\nbeq x0, x0, l1";
        let unlinked = Assembler::from_string(program.to_string())
            .assemble()
            .expect("Assembler errored out");
        // should automatically attempt to link
        let concrete = unlinked.try_into_program();
        println!("{:?}", concrete.insts);
        assert_eq!(
            concrete.insts.last().unwrap(),
            &Beq::new(ZERO, ZERO, DataWord::from(-12))
        );
    }
}
