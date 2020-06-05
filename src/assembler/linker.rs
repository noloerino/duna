use super::assembler_impl::{Assembler, UnlinkedProgram};
use super::lexer::Location;
use super::parse_error::{ParseError, ParseErrorReport, ParseErrorReporter};
use crate::program_state::{RiscVProgram, Width32b};

pub struct Linker {
    main_path: String,
    other_paths: Vec<String>, // main: UnlinkedProgram<Width32b>,
                              // programs: Vec<UnlinkedProgram<Width32b>>
}

impl Linker {
    pub fn with_main(path: &str) -> Linker {
        Linker {
            main_path: path.to_string(),
            other_paths: Vec::new(),
        }
    }

    pub fn with_file(mut self, path: &str) -> Linker {
        self.other_paths.push(path.to_string());
        self
    }

    /// Attempts to link the provided programs together into a single executable.
    pub fn link(self) -> Result<RiscVProgram<Width32b>, ParseErrorReport> {
        // Link main local labels
        let main_result: Result<UnlinkedProgram<Width32b>, ParseErrorReport> =
            Assembler::assemble_file(&self.main_path);
        let mut report = ParseErrorReporter::new("TODO".to_string()).into_report();
        // Link other programs' local labels
        let programs: Vec<UnlinkedProgram<Width32b>> = self
            .other_paths
            .into_iter()
            .filter_map(|path| match Assembler::assemble_file(&path) {
                Ok(prog) => Some(prog),
                Err(new_report) => {
                    report.merge(new_report);
                    None
                }
            })
            .collect();
        let main = match main_result {
            Ok(prog) => prog,
            Err(mut main_report) => {
                main_report.merge(report);
                return Err(main_report);
            }
        };
        if !report.is_empty() {
            return Err(report);
        }
        let mut reporter = ParseErrorReporter::new("TODO".to_string());

        // We essentially produce a single giant unlinked program from all constituent programs.
        // First, resolve all local labels, then combine all the programs together and consider
        // the union of all the global symbol tables as the new "local" symbol table.
        let UnlinkedProgram {
            insts: mut all_insts,
            sections,
            mut needed_labels,
            mut defined_global_labels,
            ..
        } = main;
        for program in programs {
            let UnlinkedProgram {
                insts: mut new_insts,
                needed_labels: new_needed_labels,
                defined_global_labels: new_global_labels,
                // TODO combine sections
                // sections,
                ..
            } = program;
            let prev_inst_size = all_insts.len();
            all_insts.append(&mut new_insts);
            for (idx, label) in new_needed_labels.into_iter() {
                needed_labels.insert(idx + prev_inst_size, label);
            }
            for (label, idx) in new_global_labels {
                if defined_global_labels.contains_key(&label) {
                    reporter.add_error(ParseError::redefined_label(
                        &Location {
                            file_name: "TODO".to_string(),
                            lineno: 0,
                            offs: 0,
                        },
                        &label,
                    ))
                }
                defined_global_labels.insert(label, idx + prev_inst_size);
            }
        }
        if reporter.is_empty() {
            let (linked, errs) = UnlinkedProgram::new(all_insts, sections, Default::default());
            if errs.is_empty() {
                Ok(linked.into_program()?)
            } else {
                Err(errs)
            }
        } else {
            Err(reporter.into_report())
        }
    }
}
