use super::parse_error::ParseErrorReport;
use super::parser::{Label, ParseResult, RiscVParser};
use super::partial_inst::PartialInst;
use crate::program_state::{MachineDataWidth, RiscVProgram, Width32b};
use std::collections::HashMap;
use std::fmt;

pub struct Assembler;

impl Assembler {
    pub fn assemble_file(path: &str) -> Result<UnlinkedProgram<Width32b>, ParseErrorReport> {
        Assembler::assemble(RiscVParser::parse_file(path))
    }

    pub fn assemble_str(contents: &str) -> Result<UnlinkedProgram<Width32b>, ParseErrorReport> {
        Assembler::assemble(RiscVParser::parse_str(contents))
    }

    fn assemble(
        parse_result: ParseResult<Width32b>,
    ) -> Result<UnlinkedProgram<Width32b>, ParseErrorReport> {
        let ParseResult {
            insts,
            sections,
            report,
        } = parse_result;
        if report.is_empty() {
            Ok(UnlinkedProgram::new(insts, sections))
        } else {
            Err(report)
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum ProgramSection {
    Text,
    Data,
    Rodata,
    // Bss,
}

impl fmt::Display for ProgramSection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ProgramSection::*;
        write!(
            f,
            "{}",
            match self {
                Text => "text",
                Data => "data",
                Rodata => "rodata",
            }
        )
    }
}

pub struct SectionStore {
    /// Stores the contents of the .data section. The first element is at the lowest address.
    pub data: Vec<u8>,
    /// Stores the contents of the .rodata section. The first element is at the lowest address.
    pub rodata: Vec<u8>,
}

impl SectionStore {
    pub fn new() -> Self {
        SectionStore {
            data: Vec::new(),
            rodata: Vec::new(),
        }
    }

    pub fn add_byte(&mut self, section: ProgramSection, val: u8) {
        use ProgramSection::*;
        match section {
            Data => &mut self.data,
            Rodata => &mut self.rodata,
            Text => panic!("adding data in text is currently unsupported"),
        }
        .push(val)
    }

    pub fn add_half(&mut self, section: ProgramSection, val: u16) {
        self.add_byte(section, val as u8);
        self.add_byte(section, (val >> 8) as u8);
    }

    pub fn add_word(&mut self, section: ProgramSection, val: u32) {
        self.add_half(section, val as u16);
        self.add_half(section, (val >> 16) as u16);
    }

    pub fn add_doubleword(&mut self, section: ProgramSection, val: u64) {
        self.add_word(section, val as u32);
        self.add_word(section, (val >> 32) as u32);
    }
}

impl Default for SectionStore {
    fn default() -> Self {
        SectionStore::new()
    }
}

/// The parser must perform two passes in order to locate/process labels.
/// This struct encodes data for a program that still needs to be passed to the assembler.
pub struct UnlinkedProgram<T: MachineDataWidth> {
    /// The list of instructions, which will be placed in the text segment in the order in which
    /// they appear.
    pub insts: Vec<PartialInst<T>>,
    // a potential optimization is to store generated labels and needed labels in independent vecs
    // instead of a hashmap, another vec can be used to lookup the corresponding PartialInst
    // TODO put labels in sections
    /// Maps labels to the index of the insts that define them
    local_labels: HashMap<Label, usize>,
    /// Maps needed labels to the index of the insts that need them
    pub needed_labels: HashMap<Label, usize>,
    sections: SectionStore,
}

impl UnlinkedProgram<Width32b> {
    /// Constructs an instance of an UnlinkedProgram from an instruction stream.
    pub fn new(
        insts: Vec<PartialInst<Width32b>>,
        sections: SectionStore,
    ) -> UnlinkedProgram<Width32b> {
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
            sections,
        }
    }

    /// Attempts to match needed labels to locally defined labels.
    /// Theoretically, this is idempotent, i.e. calling it multiple times will just produce the
    /// same program.
    pub fn link_self(self) -> UnlinkedProgram<Width32b> {
        let UnlinkedProgram {
            mut insts,
            local_labels,
            needed_labels,
            sections,
        } = self;
        let mut unresolved_labels = HashMap::new();
        for (label, inst_index) in needed_labels.into_iter() {
            match local_labels.get(&label) {
                Some(&tgt_index) => {
                    // Figure out how many instructions we need to jump
                    let inst_distance = (tgt_index as isize) - (inst_index as isize);
                    let byte_distance = (inst_distance * 4) as i64;
                    let new_inst = insts[inst_index].fulfill_label(byte_distance.into());
                    insts[inst_index] = PartialInst::new_complete(new_inst);
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
            sections,
        }
    }

    /// Attempts to produce an instance of RiscVProgram. Panics if some labels are needed
    /// but not found within the body of this program.
    pub fn try_into_program(self) -> RiscVProgram<Width32b> {
        let linked = self.link_self();
        RiscVProgram::new(
            linked
                .insts
                .into_iter()
                .map(|partial_inst| partial_inst.try_into_concrete_inst())
                .collect(),
            linked.sections,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::BType;
    use crate::isa::Beq;
    use crate::program_state::DataWord;
    use crate::program_state::IRegister::ZERO;

    #[test]
    fn test_forward_local_label() {
        let program = "beq x0, x0, l1\nnop\nnop\nl1:nop";
        let unlinked = Assembler::assemble_str(program).expect("Assembler errored out");
        // should automatically attempt to link
        let concrete = unlinked.try_into_program();
        println!("{:?}", concrete.insts);
        assert_eq!(concrete.insts[0], Beq::new(ZERO, ZERO, DataWord::from(12)));
    }

    #[test]
    fn test_backward_local_label() {
        let program = "\nl1:nop\nnop\nnop\nbeq x0, x0, l1";
        let unlinked = Assembler::assemble_str(program).expect("Assembler errored out");
        // should automatically attempt to link
        let concrete = unlinked.try_into_program();
        println!("{:?}", concrete.insts);
        assert_eq!(
            concrete.insts.last().unwrap(),
            &Beq::new(ZERO, ZERO, DataWord::from(-12))
        );
    }
}
