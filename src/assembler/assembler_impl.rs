use super::datatypes::*;
use super::parse_error::{ParseError, ParseErrorReporter};
use super::parser::{Label, LabelRef, ParseResult, Parser};
use super::partial_inst::{PartialInst, PartialInstType};
use crate::arch::*;
use crate::program_state::Program;
use std::collections::{HashMap, HashSet};
use std::fmt;

pub struct Assembler;

impl Assembler {
    pub fn assemble_str<S: Architecture<T>, T: MachineDataWidth>(
        file_id: FileId,
        contents: &str,
    ) -> (UnlinkedProgram<S, T>, ParseErrorReporter) {
        Assembler::assemble(<S::Parser>::parse_str(file_id, contents))
    }

    fn assemble<S: Architecture<T>, T: MachineDataWidth>(
        parse_result: ParseResult<S, T>,
    ) -> (UnlinkedProgram<S, T>, ParseErrorReporter) {
        let ParseResult {
            file_id,
            insts,
            sections,
            declared_globals,
            mut reporter,
        } = parse_result;
        let (program, selflink_reporter) = UnlinkedProgram::new(
            insts.into_iter().map(|inst| (file_id, inst)).collect(),
            sections,
            declared_globals,
        );
        reporter.merge(selflink_reporter);
        (program, reporter)
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
pub struct UnlinkedProgram<S: Architecture<T>, T: MachineDataWidth> {
    /// A list of (source file id, instruction), which will be placed in the text segment in the
    /// order in which they appear.
    pub(super) insts: Vec<(FileId, PartialInst<S, T>)>,
    // a potential optimization is to store generated labels and needed labels in independent vecs
    // instead of a hashmap, another vec can be used to lookup the corresponding PartialInst
    // TODO put labels in sections
    /// Maps index of an instruction to the label it needs.
    pub(super) needed_labels: HashMap<usize, LabelRef>,
    /// Maps global labels to its location, as well as the index of the insts that define them
    pub(super) defined_global_labels: HashMap<Label, (Location, usize)>,
    pub(super) sections: SectionStore,
}

impl<S: Architecture<T>, T: MachineDataWidth> UnlinkedProgram<S, T> {
    /// Constructs an instance of an UnlinkedProgram from a stream of (file name, instruction).
    /// Also attempts to match needed labels to locally defined labels, and populates the needed
    /// and global symbol tables.
    /// A ParseErrorReporter is also returned to allow the linker to proceed with partial information
    /// in the event of a non-fatal error in this program.
    pub(super) fn new(
        mut insts: Vec<(FileId, PartialInst<S, T>)>,
        sections: SectionStore,
        declared_globals: HashSet<String>,
    ) -> (UnlinkedProgram<S, T>, ParseErrorReporter) {
        let mut reporter = ParseErrorReporter::new();
        let mut local_labels: HashMap<Label, (Location, usize)> = Default::default();
        for (i, (_, partial_inst)) in insts.iter().enumerate() {
            if let Some(label_def) = &partial_inst.label {
                let key = label_def.name.clone();
                if local_labels.contains_key(&key) {
                    // If already defined, don't touch the original def
                    reporter.add_error(ParseError::redefined_label(label_def));
                } else {
                    local_labels.insert(label_def.name.clone(), (label_def.location, i));
                }
            }
        }
        let all_needed_labels: HashMap<usize, LabelRef> = insts
            .iter()
            .enumerate()
            .filter_map(|(i, (_, partial_inst))| {
                Some((i, partial_inst.get_needed_label()?.clone()))
            })
            .collect();
        let defined_global_labels: HashMap<Label, (Location, usize)> = local_labels
            .iter()
            .filter_map(|(label, index)| {
                if declared_globals.contains(label) {
                    Some((label.to_string(), *index))
                } else {
                    None
                }
            })
            .collect();
        // map of labels after resolving local ones
        let mut needed_labels = HashMap::new();
        for (inst_index, label) in all_needed_labels.into_iter() {
            if let Some(&(_, tgt_index)) = local_labels.get(&label.target) {
                // Figure out how many instructions we need to jump
                let inst_distance = (tgt_index as isize) - (inst_index as isize);
                let byte_distance = (inst_distance * 4) as i64;
                let (file_id, old_inst) = &insts[inst_index];
                if let PartialInstType::NeedsLabelRef(inst) = &old_inst.tpe {
                    insts[inst_index] = (
                        *file_id,
                        PartialInst::new_complete(inst.fulfill_label(byte_distance.into())),
                    )
                } else {
                    panic!("cannot fulfill label for complete instruction")
                };
            } else if declared_globals.contains(&label.target) {
                needed_labels.insert(inst_index, label);
            } else {
                reporter.add_error(ParseError::undeclared_label(&label));
            }
        }
        (
            UnlinkedProgram {
                insts,
                needed_labels,
                defined_global_labels,
                sections,
            },
            reporter,
        )
    }

    pub fn into_program(self) -> Result<S::Program, ParseErrorReporter> {
        let mut reporter = ParseErrorReporter::new();
        let insts = self
            .insts
            .into_iter()
            .filter_map(
                |(_, partial_inst)| match partial_inst.into_concrete_inst() {
                    Ok(concrete_inst) => Some(concrete_inst),
                    Err(needed_label) => {
                        reporter.add_error(ParseError::undefined_label(&needed_label));
                        None
                    }
                },
            )
            .collect();
        if reporter.is_empty() {
            Ok(<S::Program>::new(insts, self.sections))
        } else {
            Err(reporter)
        }
    }

    /// Attempts to produce an instance of RiscVProgram. Panics if some labels are needed
    /// but not found within the body of this program.
    pub fn try_into_program(self) -> S::Program {
        <S::Program>::new(
            self.insts
                .into_iter()
                .map(|(_, partial_inst)| partial_inst.try_into_concrete_inst())
                .collect(),
            self.sections,
        )
    }
}
