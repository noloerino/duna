use super::assembler_impl::{Assembler, SectionStore, UnlinkedProgram};
use super::lexer::Location;
use super::parse_error::{ErrMetadata, ParseError, ParseErrorReport, ParseErrorReporter};
use super::parser::Label;
use crate::program_state::{RiscVProgram, Width32b};
use std::collections::HashMap;
use std::fs;

pub struct FileData {
    pub file_name: String,
    pub content: String,
}

impl FileData {
    #[cfg(test)]
    pub fn from_test_program(content: &str) -> FileData {
        FileData {
            file_name: "test".to_string(),
            content: content.to_string(),
        }
    }
}

/// Used to make error reporting easier without having to worry about lifetimes with string
/// references. Should map to a FileData somehow.
pub type FileId = usize;
pub type FileMap = Vec<FileData>;

/// Links programs together.
///
/// When the link method is called, this struct is responsble for owning the file name string as well
/// as the strings of the contents of all files. All other stages (assembler, parser, lexer) should
/// be taking references to these strings.
/// TODO add ability to just add in raw strings and generate file names accordingly
pub struct Linker {
    /// Maps a FileId to FileData. Must be nonempty.
    ///
    /// The "main" file is assumed to exist at index 0.
    file_map: FileMap,
}

impl Linker {
    pub fn with_main(path: &str) -> Linker {
        Linker {
            file_map: vec![FileData {
                file_name: path.to_string(),
                content: Linker::read_file(path),
            }],
        }
    }

    pub fn with_main_str(contents: &str) -> Linker {
        Linker {
            file_map: vec![FileData {
                file_name: "<main>".to_string(),
                content: contents.to_string(),
            }],
        }
    }

    pub fn with_file(mut self, path: &str) -> Linker {
        self.file_map.push(FileData {
            file_name: path.to_string(),
            content: Linker::read_file(path),
        });
        self
    }

    fn read_file(path: &str) -> String {
        fs::read_to_string(path).expect("Failed to open file")
    }

    /// Attempts to link the provided programs together into a single executable.
    pub fn link(self) -> Result<RiscVProgram<Width32b>, ParseErrorReport> {
        assert!(self.file_map.len() > 0, "Linker is missing a main program");
        let mut reporter = ParseErrorReporter::new();
        // Link other programs' local labels
        let mut programs: Vec<UnlinkedProgram<Width32b>> = Vec::new();
        for (i, FileData { content, .. }) in self.file_map.iter().enumerate() {
            match Assembler::assemble_str(i, &content) {
                Ok(prog) => programs.push(prog),
                Err(new_reporter) => {
                    reporter.merge(new_reporter);
                }
            }
        }
        // Even if errors have so far been reported, we can proceed to try to link anyway

        // We essentially produce a single giant unlinked program from all constituent programs.
        // First, resolve all local labels, then combine all the programs together and consider
        // the union of all the global symbol tables as the new "local" symbol table.
        let mut all_insts = Vec::new();
        let mut needed_labels: HashMap<usize, Label> = Default::default();
        let mut defined_global_labels: HashMap<Label, usize> = Default::default();
        let mut combined_sections = SectionStore::new();

        for (file_id, program) in programs.into_iter().enumerate() {
            let UnlinkedProgram {
                insts: mut new_insts,
                needed_labels: new_needed_labels,
                defined_global_labels: new_global_labels,
                sections,
                ..
            } = program;
            // TODO combine sections (currently just takes first)
            if file_id == 1 {
                combined_sections = sections;
            }
            let prev_inst_size = all_insts.len();
            all_insts.append(&mut new_insts);
            for (idx, label) in new_needed_labels.into_iter() {
                needed_labels.insert(idx + prev_inst_size, label);
            }
            for (label, idx) in new_global_labels {
                // Check for previous definition
                if let Some(_prev_idx) = defined_global_labels.get(&label) {
                    // let (decl_file_id, _) = &all_insts[*prev_idx];
                    reporter.add_error(ParseError::redefined_label(
                        ErrMetadata::new(&Location {
                            file_id,
                            lineno: 0,
                            offs: 0,
                        }),
                        &label,
                    ))
                }
                defined_global_labels.insert(label, idx + prev_inst_size);
            }
        }
        if reporter.is_empty() {
            let (linked, errs) =
                UnlinkedProgram::new(all_insts, combined_sections, Default::default());
            if errs.is_empty() {
                // handles errantly undefined labels, although they should've already been caught
                linked
                    .into_program()
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
