use super::{
    assembler_impl::{Assembler, LabelTarget, SectionStore, UnlinkedProgram},
    datatypes::*,
    error::{ErrorReport, ErrorReporter, ParseError},
    parser::{Label, LabelDef},
};
use crate::{arch::*, config::*, program_state::Program};
use std::{collections::HashMap, fs};

/// Links programs together.
///
/// When the link method is called, this struct is responsble for owning the file name string as well
/// as the strings of the contents of all files. All other stages (assembler, parser, lexer) should
/// be taking references to these strings.
pub struct Linker {
    /// Maps a FileId to FileData. Must be nonempty.
    ///
    /// The "main" file is assumed to exist at index 0.
    file_map: FileMap,
    /// Keeps track of how many unnamed files have been added.
    unnamed_count: usize,
}

impl Linker {
    pub fn with_main(path: &str) -> Linker {
        Linker {
            file_map: vec![FileData {
                file_name: path.to_string(),
                content: Linker::read_file(path),
            }],
            unnamed_count: 0,
        }
    }

    pub fn with_main_str(contents: &str) -> Linker {
        Linker {
            file_map: vec![FileData {
                file_name: "<main>".to_string(),
                content: contents.to_string(),
            }],
            unnamed_count: 1,
        }
    }

    pub fn with_file(mut self, path: &str) -> Linker {
        self.file_map.push(FileData {
            file_name: path.to_string(),
            content: Linker::read_file(path),
        });
        self
    }

    pub fn with_str(mut self, contents: &str) -> Linker {
        self.file_map.push(FileData {
            file_name: format!("<unnamed file {}>", self.unnamed_count),
            content: contents.to_string(),
        });
        self.unnamed_count += 1;
        self
    }

    fn read_file(path: &str) -> String {
        fs::read_to_string(path).expect("Failed to open file")
    }

    /// Attempts to link the provided programs together into a single executable.
    pub fn link<A: Architecture>(self, config: AsmConfig) -> Result<Program<A>, ErrorReport> {
        assert!(
            !self.file_map.is_empty(),
            "Linker is missing a main program"
        );
        let mut reporter = ErrorReporter::new();
        // Link other programs' local labels
        let mut programs: Vec<UnlinkedProgram<A>> = Vec::new();
        for (i, FileData { content, .. }) in self.file_map.iter().enumerate() {
            let (prog, new_reporter) = Assembler::assemble_str(i, content);
            programs.push(prog);
            reporter.merge(new_reporter);
        }
        // Even if errors have so far been reported, we can proceed to try to link anyway

        // We essentially produce a single giant unlinked program from all constituent programs.
        // First, resolve all local labels, then combine all the programs together and consider
        // the union of all the global symbol tables as the new "local" symbol table.
        let mut all_insts = Vec::new();
        let mut needed_labels: HashMap<usize, Label> = Default::default();
        let mut defined_global_labels: HashMap<Label, LabelTarget> = Default::default();
        let mut combined_sections = SectionStore::new();

        for program in programs.into_iter() {
            let UnlinkedProgram {
                insts: mut new_insts,
                needed_labels: new_needed_labels,
                defined_global_labels: new_global_labels,
                sections,
                ..
            } = program;
            // TODO implement for other sections
            // Ensure that the section is properly aligned for the next file
            combined_sections.zero_pad_until_doubleword_aligned();
            let prev_data_size = combined_sections.data().len();
            combined_sections.join(sections);
            let prev_inst_size = all_insts.len();
            all_insts.append(&mut new_insts);
            for (idx, label) in new_needed_labels.into_iter() {
                needed_labels.insert(idx + prev_inst_size, label.target);
            }
            for (label, target_type) in new_global_labels {
                // Check for previous definition and preserve original
                if defined_global_labels.contains_key(&label) {
                    reporter.add_error(ParseError::redefined_label(&LabelDef {
                        name: label.clone(),
                        location: target_type.location(),
                    }))
                } else {
                    match target_type {
                        LabelTarget::Inst { location, idx } => {
                            defined_global_labels.insert(
                                label.clone(),
                                LabelTarget::Inst {
                                    location,
                                    idx: idx + prev_inst_size,
                                },
                            );
                        }
                        LabelTarget::Data {
                            location,
                            section,
                            idx,
                        } => {
                            defined_global_labels.insert(
                                label.clone(),
                                LabelTarget::Data {
                                    location,
                                    section,
                                    idx: idx + prev_data_size,
                                },
                            );
                        }
                    }
                }
            }
        }
        if reporter.is_empty() {
            let (linked, errs) = UnlinkedProgram::<A>::new(
                all_insts,
                combined_sections,
                // Need to keep track of definitions for _start/main
                defined_global_labels.keys().cloned().collect(),
            );
            if errs.is_empty() {
                // handles errantly undefined labels, although they should've already been caught
                linked
                    .into_program(&config.machine)
                    .map_err(|r| r.into_report_with_file_map(self.file_map))
            } else {
                // handles undeclared labels
                Err(errs.into_report_with_file_map(self.file_map))
            }
        } else {
            Err(reporter.into_report_with_file_map(self.file_map))
        }
    }
}
