use super::assembler_impl::{ProgramSection, SectionStore};
use super::datatypes::*;
use super::lexer::*;
use super::parse_error::{ParseError, ParseErrorReporter};
use super::partial_inst::PartialInst;
use crate::arch::*;
use crate::instruction::ConcreteInst;
use crate::program_state::IRegister;
use std::collections::HashSet;
use std::iter::Peekable;
use std::vec::IntoIter;

pub type Label = String;

/// Represents a reference to a label in a program, e.g. as the target of a jump.
#[derive(Clone)]
pub struct LabelRef {
    pub target: Label,
    /// The location at which the reference occurs.
    pub location: Location,
}

impl LabelRef {
    pub fn new(target: Label, location: Location) -> LabelRef {
        LabelRef { target, location }
    }
}

/// Represents a definition of a label in a program.
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct LabelDef {
    pub name: Label,
    /// The location at which the definition is made occurs.
    pub location: Location,
}

impl LabelDef {
    pub fn new(name: Label, location: Location) -> LabelDef {
        LabelDef { name, location }
    }
}

pub type ParsedInstStream<F: ArchFamily<T>, T> = Vec<PartialInst<F, T>>;
pub type LineParseResult<F: ArchFamily<T>, T> = Result<ParsedInstStream<F, T>, ParseError>;
pub struct ParseResult<F, T>
where
    F: ArchFamily<T>,
    T: MachineDataWidth,
{
    pub file_id: FileId,
    pub insts: ParsedInstStream<F, T>,
    pub sections: SectionStore,
    pub declared_globals: HashSet<String>,
    pub reporter: ParseErrorReporter,
}

/// State about the program being parsed.
pub struct ParseState {
    /// The section in which parsed values should be placed.
    /// This should default to text.
    pub curr_section: ProgramSection,
    pub sections: SectionStore,
    /// These labels were given to a .global declaration, which means either the
    /// current file defined the symbol and is making it visible to the linker, or
    /// the current file will look for the symbol in another file.
    pub declared_globals: HashSet<String>,
}

impl ParseState {
    pub fn new() -> ParseState {
        ParseState {
            curr_section: ProgramSection::Text,
            sections: SectionStore::new(),
            declared_globals: HashSet::new(),
        }
    }
}

pub type TokenIter = Peekable<IntoIter<Token>>;

pub trait Parser<F: ArchFamily<T>, T>
where
    F: ArchFamily<T>,
    T: MachineDataWidth,
{
    fn parse_str(file_id: FileId, contents: &str) -> ParseResult<F, T> {
        Self::parse_lex_result(Lexer::lex_str(file_id, contents))
    }

    fn parse_lex_result(lex_result: LexResult) -> ParseResult<F, T>;
}
