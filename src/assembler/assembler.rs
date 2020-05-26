use super::lexer::Lexer;
use super::parser::ParseError;
use super::parser::RiscVParser;
use crate::instruction::ConcreteInst;
use crate::program_state::{DataWord, IRegister, RiscVProgram};
use std::collections::HashMap;

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

enum PartialInstType {
    Complete(ConcreteInst),
    TwoRegNeedsLabel {
        assemble: fn(IRegister, IRegister, DataWord) -> ConcreteInst,
        needed_label: Label,
    },
    OneRegNeedsLabel {
        assemble: fn(IRegister, DataWord) -> ConcreteInst,
        needed_label: Label,
    },
}

pub struct PartialInst {
    tpe: PartialInstType,
    pub label: Option<Label>,
}

impl PartialInst {
    pub fn new_complete(inst: ConcreteInst) -> PartialInst {
        PartialInst {
            tpe: PartialInstType::Complete(inst),
            label: None,
        }
    }

    pub fn new_two_reg_needs_label(
        assemble: fn(IRegister, IRegister, DataWord) -> ConcreteInst,
        needed: Label,
    ) -> PartialInst {
        PartialInst {
            tpe: PartialInstType::TwoRegNeedsLabel {
                assemble,
                needed_label: needed,
            },
            label: None,
        }
    }

    pub fn new_one_reg_needs_label(
        assemble: fn(IRegister, DataWord) -> ConcreteInst,
        needed: Label,
    ) -> PartialInst {
        PartialInst {
            tpe: PartialInstType::OneRegNeedsLabel {
                assemble,
                needed_label: needed,
            },
            label: None,
        }
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
        use PartialInstType::*;
        match &self.tpe {
            TwoRegNeedsLabel { needed_label, .. } | OneRegNeedsLabel { needed_label, .. } => {
                Some(&needed_label)
            }
            Complete(..) => None,
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
}

/// The parser must perform two passes in order to locate/process labels.
/// This struct encodes data for a program that still needs to be passed to the assembler.
pub struct UnlinkedProgram {
    pub insts: Vec<PartialInst>,
    /// Maps labels to the index of the insts that define them
    pub local_labels: HashMap<Label, usize>,
    /// Maps needed labels to the index of the insts that need them
    pub needed_labels: HashMap<Label, usize>,
    // a potential optimization is to store generated labels and needed labels in independent vecs
    // instead of a hashmap, another vec can be used to lookup the corresponding PartialInst
}

impl UnlinkedProgram {
    pub fn new(insts: Vec<PartialInst>) -> UnlinkedProgram {
        use PartialInstType::*;
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
            .filter_map(|(i, partial_inst)| match &partial_inst.tpe {
                TwoRegNeedsLabel { needed_label, .. } | OneRegNeedsLabel { needed_label, .. } => {
                    Some((needed_label.clone(), i))
                }
                _ => None,
            })
            .collect();
        UnlinkedProgram {
            insts,
            local_labels,
            needed_labels,
        }
    }

    /// Attempts to produce an instance of RiscVProgram. Panics if some labels are needed
    /// but not found within the body of this program.
    pub fn try_into_program(self) -> RiscVProgram {
        use PartialInstType::*;
        RiscVProgram::new(
            self.insts
                .into_iter()
                .map(|partial_inst| match partial_inst.tpe {
                    Complete(inst) => inst,
                    _ => panic!("inst needed labels (error handling unimplented)"),
                })
                .collect(),
        )
    }
}
