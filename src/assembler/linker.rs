use super::assembler_impl::{Assembler, UnlinkedProgram};
use super::parse_error::ParseErrorReport;
use crate::program_state::{RiscVProgram, Width32b};

pub struct Linker {
    main: UnlinkedProgram<Width32b>,
    programs: Vec<UnlinkedProgram<Width32b>>,
}

impl Linker {
    pub fn with_main(path: &str) -> Result<Linker, ParseErrorReport> {
        Ok(Linker {
            main: Assembler::assemble_file(path)?,
            programs: Vec::new(),
        })
    }

    pub fn with_file(mut self, path: &str) -> Result<Linker, ParseErrorReport> {
        self.programs.push(Assembler::assemble_file(path)?);
        Ok(self)
    }

    /// Attempts to link the provided programs together into a single executable.
    /// TODO implement global labels
    pub fn link(self) -> Result<RiscVProgram<Width32b>, Vec<LinkError>> {
        // We essentially produce a single giant unlinked program from all constituent programs.
        // First, resolve all local labels, then combine all the programs together and consider
        // the union of all the global symbol tables as the new "local" symbol table.
        let mut errs = Vec::new();
        let UnlinkedProgram {
            insts: mut all_insts,
            sections,
            mut needed_labels,
            mut defined_global_labels,
            ..
        } = self.main;
        for program in self.programs {
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
                    errs.push(LinkError {
                        file_name: "TODO".to_string(),
                        content: format!("multiple definitions for global symbol {}", label),
                    })
                }
                defined_global_labels.insert(label, idx + prev_inst_size);
            }
        }
        if errs.is_empty() {
            // TODO handle missing labels here
            Ok(
                UnlinkedProgram::new(None, all_insts, sections, Default::default())
                    .try_into_program(),
            )
        } else {
            Err(errs)
        }
    }
}

// #[derive(PartialEq, Hash)]
// struct FileLineNo {
//     file_name: String,
//     line: LineNo,
// }

#[derive(Debug)]
pub struct LinkError {
    file_name: String,
    content: String,
}

// pub struct LinkErrorReport {
//     lines: HashMap<FileLineNo, String>,
//     pub errs: Vec<LinkError>,
// }

// impl LinkErrorReport {
//     fn new() -> LinkErrorReport {
//         LinkErrorReport {
//             lines: HashMap::new(),
//             errs: Vec::new(),
//         }
//     }
// }
