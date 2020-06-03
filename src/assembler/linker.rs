use super::assembler_impl::{Assembler, UnlinkedProgram};
use super::parse_error::ParseErrorReport;
use crate::program_state::{RiscVProgram, Width32b};

pub struct Linker {
    main: UnlinkedProgram<Width32b>,
    programs: Vec<UnlinkedProgram<Width32b>>,
}

impl Linker {
    pub fn with_main(self, path: &str) -> Result<Linker, ParseErrorReport> {
        Ok(Linker {
            main: Assembler::assemble_file(path)?,
            programs: Vec::new(),
        })
    }

    pub fn with_file(mut self, path: &str) -> Result<Linker, ParseErrorReport> {
        self.programs.push(Assembler::assemble_file(path)?);
        Ok(self)
    }

    pub fn link(self) -> Result<RiscVProgram<Width32b>, ParseErrorReport> {
        unimplemented!()
    }
}
