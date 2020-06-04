use super::assembler_impl::{Assembler, UnlinkedProgram};
use super::parse_error::ParseErrorReport;
use crate::program_state::{RiscVProgram, Width32b};
use std::collections::HashMap;

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
        let mut new_program = UnlinkedProgram {
            // TODO move global labels
            local_labels: HashMap::new(),
            ..self.main.link_self()
        };
        for program in self.programs {
            let UnlinkedProgram {
                mut insts,
                needed_labels,
                // TODO combine sections
                // sections,
                ..
            } = program.link_self();
            let prev_inst_size = new_program.insts.len();
            new_program.insts.append(&mut insts);
            for (label, idx) in needed_labels.into_iter() {
                if new_program.needed_labels.contains_key(&label) {
                    errs.push(LinkError {
                        file_name: "TODO".to_string(),
                        content: format!("redeclared label: {}", label),
                    });
                } else {
                    new_program
                        .needed_labels
                        .insert(label, idx + prev_inst_size);
                }
            }
        }
        if errs.is_empty() {
            // TODO handle missing labels here
            Ok(new_program.try_into_program())
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
