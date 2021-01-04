use super::{
    datatypes::*,
    error::{ErrorReporter, ParseError},
    parser::{Label, LabelDef, LabelRef, ParseResult, Parser},
    partial_inst::{PartialInst, PartialInstType},
};
use crate::{arch::*, config::*, data_structures::*, program_state::*};
use num_traits::cast::AsPrimitive;
use std::{
    collections::{HashMap, HashSet},
    fmt,
};

pub struct Assembler;

impl Assembler {
    pub fn assemble_str<A: Architecture>(
        file_id: FileId,
        contents: &str,
    ) -> (UnlinkedProgram<A>, ErrorReporter) {
        Assembler::assemble(Parser::<A>::parse_str(file_id, contents))
    }

    fn assemble<A: Architecture>(
        parse_result: ParseResult<A::Family, A::DataWidth>,
    ) -> (UnlinkedProgram<A>, ErrorReporter) {
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

#[derive(Clone)]
pub struct SectionStore {
    /// Stores the contents of the .data section. The first element is at the lowest address.
    data: Vec<u8>,
    /// Stores the contents of the .rodata section. The first element is at the lowest address.
    rodata: Vec<u8>,
    labels: Vec<(LabelDef, ProgramSection, usize)>,
    require_align: bool,
}

impl SectionStore {
    pub fn new() -> Self {
        SectionStore {
            data: Vec::new(),
            rodata: Vec::new(),
            labels: Vec::new(),
            require_align: true,
        }
    }

    pub fn data(&self) -> &[u8] {
        &self.data
    }

    pub fn rodata(&self) -> &[u8] {
        &self.rodata
    }

    pub fn labels(&self) -> &[(LabelDef, ProgramSection, usize)] {
        &self.labels
    }

    /// Adds a label to the next element in that section.
    fn add_label_here(&mut self, section: ProgramSection, label: LabelDef) {
        use ProgramSection::*;
        self.labels.push((
            label,
            section,
            match section {
                Data => &self.data,
                Rodata => &self.rodata,
                Text => panic!("adding data in text is currently unsupported"),
            }
            .len(),
        ));
    }

    fn byte_len(&self, section: ProgramSection) -> usize {
        use ProgramSection::*;
        match section {
            Data => &self.data,
            Rodata => &self.rodata,
            Text => panic!("adding data in text is currently unsupported"),
        }
        .len()
    }

    pub fn add(&mut self, section: ProgramSection, maybe_label: Option<LabelDef>, val: DataEnum) {
        // handle alignment first so the label is placed at the correct index
        if self.require_align {
            let pad_requirement = match val {
                Byte(_) => 1,
                Half(_) => 2,
                Lword(_) => 4,
                Dword(_) => 8,
            };
            while self.byte_len(section) % pad_requirement != 0 {
                self.add_byte(section, 0);
            }
        }
        if let Some(label) = maybe_label {
            self.add_label_here(section, label);
        }
        use DataEnum::*;
        match val {
            Byte(n) => self.add_byte(section, n.into()),
            Half(n) => self.add_half(section, n.into()),
            Lword(n) => self.add_word(section, n.into()),
            Dword(n) => self.add_doubleword(section, n.into()),
        }
    }

    /// Adds zero bytes until the total number of bytes in the section is a multiple of 8, making it
    /// aligned to a doubleword.
    pub fn zero_pad_until_doubleword_aligned(&mut self) {
        use ProgramSection::*;
        for &section in &[Data, Rodata] {
            while self.byte_len(section) % 8 != 0 {
                self.add_byte(section, 0);
            }
        }
    }

    /// Consumes the other SectionStore, joining it with this one.
    /// Agnostic to alignment.
    pub fn join(&mut self, other: SectionStore) {
        let old_data_len = self.byte_len(ProgramSection::Data);
        self.data.extend(other.data);
        self.rodata.extend(other.rodata);
        // combine labels
        for (label_def, section, idx) in other.labels.into_iter() {
            match section {
                ProgramSection::Data => self.labels.push((label_def, section, old_data_len + idx)),
                _ => unimplemented!(),
            }
        }
    }

    fn add_byte(&mut self, section: ProgramSection, val: u8) {
        use ProgramSection::*;
        match section {
            Data => &mut self.data,
            Rodata => &mut self.rodata,
            Text => panic!("adding data in text is currently unsupported"),
        }
        .push(val)
    }

    fn add_half(&mut self, section: ProgramSection, val: u16) {
        self.add_byte(section, val as u8);
        self.add_byte(section, (val >> 8) as u8);
    }

    fn add_word(&mut self, section: ProgramSection, val: u32) {
        self.add_half(section, val as u16);
        self.add_half(section, (val >> 16) as u16);
    }

    // TODO add option for these to preserve alignment
    fn add_doubleword(&mut self, section: ProgramSection, val: u64) {
        self.add_word(section, val as u32);
        self.add_word(section, (val >> 32) as u32);
    }
}

impl Default for SectionStore {
    fn default() -> Self {
        SectionStore::new()
    }
}

pub type FileIdAndInst<S> = (
    FileId,
    PartialInst<<S as Architecture>::Family, <S as Architecture>::DataWidth>,
);

/// The parser must perform two passes in order to locate/process labels.
/// This struct encodes data for a program that still needs to be passed to the assembler.
pub struct UnlinkedProgram<S: Architecture> {
    /// A list of (source file id, instruction), which will be placed in the text segment in the
    /// order in which they appear.
    pub(super) insts: Vec<FileIdAndInst<S>>,
    // a potential optimization is to store generated labels and needed labels in independent vecs
    // instead of a hashmap, another vec can be used to lookup the corresponding PartialInst
    // TODO put labels in sections
    /// Maps index of an instruction to the label it needs.
    pub(super) needed_labels: HashMap<usize, LabelRef>,
    /// Maps global labels to its token location and program location.
    pub(super) defined_global_labels: HashMap<Label, LabelTarget>,
    /// Stores literal values declared by directives, as well as labels that reference those values.
    pub(super) sections: SectionStore,
}

/// Determintes whether the label points to an instruction or the data section.
#[derive(Copy, Clone)]
pub enum LabelTarget {
    Inst {
        location: Location,
        idx: usize,
    },
    Data {
        location: Location,
        section: ProgramSection,
        idx: usize,
    },
}

impl LabelTarget {
    pub fn location(self) -> Location {
        use LabelTarget::*;
        match self {
            Inst { location, .. } | Data { location, .. } => location,
        }
    }
}

impl<A: Architecture> UnlinkedProgram<A> {
    /// Constructs an instance of an UnlinkedProgram from a stream of (file name, instruction).
    /// Also attempts to match needed labels to locally defined labels, and populates the needed
    /// and global symbol tables.
    /// A ParseErrorReporter is also returned to allow the linker to proceed with partial information
    /// in the event of a non-fatal error in this program.
    pub(super) fn new(
        mut insts: Vec<FileIdAndInst<A>>,
        sections: SectionStore,
        declared_globals: HashSet<String>,
    ) -> (UnlinkedProgram<A>, ErrorReporter) {
        let mut reporter = ErrorReporter::new();
        let mut local_labels: HashMap<Label, LabelTarget> = Default::default();
        // Label definitions in instructions
        for (i, (_, partial_inst)) in insts.iter().enumerate() {
            if let Some(label_def) = &partial_inst.label {
                if local_labels.contains_key(&label_def.name) {
                    // If already defined, don't touch the original def
                    reporter.add_error(ParseError::redefined_label(label_def));
                } else {
                    local_labels.insert(
                        label_def.name.clone(),
                        LabelTarget::Inst {
                            location: label_def.location,
                            idx: i,
                        },
                    );
                }
            }
        }
        // Label definitions in data sections
        for (label_def, section, idx) in sections.labels.iter().cloned() {
            if local_labels.contains_key(&label_def.name) {
                reporter.add_error(ParseError::redefined_label(&label_def));
            } else {
                local_labels.insert(
                    label_def.name.clone(),
                    LabelTarget::Data {
                        location: label_def.location,
                        section,
                        idx,
                    },
                );
            }
        }
        let all_needed_labels: HashMap<usize, LabelRef> = insts
            .iter()
            .enumerate()
            .filter_map(|(i, (_, partial_inst))| {
                Some((i, partial_inst.get_needed_label()?.clone()))
            })
            .collect();
        let defined_global_labels: HashMap<Label, LabelTarget> = local_labels
            .iter()
            .filter_map(|(label, tgt)| {
                if declared_globals.contains(label) {
                    Some((label.to_string(), *tgt))
                } else {
                    None
                }
            })
            .collect();
        // map of labels after resolving local ones
        let mut needed_labels = HashMap::new();
        for (inst_index, label) in all_needed_labels.into_iter() {
            if let Some(&target_type) = local_labels.get(&label.target) {
                let byte_distance: i64 = match target_type {
                    LabelTarget::Inst {
                        location: _,
                        idx: tgt_index,
                    } => {
                        // Figure out how many instructions we need to jump
                        let inst_distance = (tgt_index as isize) - (inst_index as isize);
                        (inst_distance * 4) as i64
                    }
                    LabelTarget::Data {
                        location: _,
                        section,
                        idx: data_index,
                    } => {
                        // If the instruction is at (TEXT_START + x) and the target occurs at
                        // (DATA_START + y), then we can compute the offset to be
                        // (DATA_START - TEXT_START + y - x)
                        // TODO make this more configurable
                        let segment_starts = SegmentStarts::default();
                        let text_start: SignedValue<A::DataWidth> =
                            segment_starts.text::<A::DataWidth>().into();
                        let data_start: SignedValue<A::DataWidth> =
                            segment_starts.data::<A::DataWidth>().into();
                        if section != ProgramSection::Data {
                            unimplemented!()
                        }
                        let data_loc = data_start + (data_index as i64).into();
                        // inst index is in words, so multiply by 4
                        let pc = text_start + ((inst_index * 4) as i64).into();
                        let byte_distance = data_loc - pc;
                        AsPrimitive::<i64>::as_(byte_distance.raw())
                    }
                };
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

    /// Produces a program, or an error report if some instructions are still missing labels.
    pub fn into_program(self, config: &MachineConfig) -> Result<Program<A>, ErrorReporter> {
        let mut reporter = ErrorReporter::new();
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
        // TODO configure _start`
        // For now, the initial PC is set to the location of the global main label
        // or else the location of the first instruction
        let main_inst_idx: usize = if let Some(tgt) = self.defined_global_labels.get("main") {
            match *tgt {
                LabelTarget::Inst { idx, .. } => idx,
                LabelTarget::Data { location, .. } => {
                    reporter.add_error(ParseError::bad_main_def(&location));
                    0
                }
            }
        } else {
            0
        };
        if reporter.is_empty() {
            Ok(Program::<A>::new(
                insts,
                main_inst_idx,
                config.segment_starts,
                self.sections,
                config.mem_config,
            ))
        } else {
            Err(reporter)
        }
    }

    /// Attempts to produce an instance of the program. Panics if some labels are needed
    /// but not found within the body of this program.
    pub fn try_into_program(self, config: &MachineConfig) -> Program<A> {
        self.into_program(config).unwrap()
    }
}
