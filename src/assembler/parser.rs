use super::assembler_impl::{ProgramSection, SectionStore};
use super::datatypes::*;
use super::lexer::*;
use super::parse_error::{ParseError, ParseErrorReporter};
use super::partial_inst::PartialInst;
use crate::arch::*;
use crate::program_state::*;
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

/// Stores literals that were parsed from a directive such as .byte.
pub struct DirectiveLiterals {
    pub section: ProgramSection,
    pub data: Vec<DataEnum>,
}

impl DirectiveLiterals {
    pub fn new(section: ProgramSection) -> Self {
        DirectiveLiterals {
            section,
            data: Vec::new(),
        }
    }

    pub fn add_byte(&mut self, val: u8) {
        self.data.push(DataEnum::Byte(val.into()));
    }

    pub fn add_half(&mut self, val: u16) {
        self.data.push(DataEnum::Half(val.into()));
    }

    pub fn add_word(&mut self, val: u32) {
        self.data.push(DataEnum::Word(val.into()));
    }

    pub fn add_doubleword(&mut self, val: u64) {
        self.data.push(DataEnum::DoubleWord(val.into()));
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

pub enum OkParseResult<F: ArchFamily<S>, S: Data> {
    Insts(ParsedInstStream<F, S>),
    Literals(DirectiveLiterals),
    None,
}

pub type ParsedInstStream<F, S> = Vec<PartialInst<F, S>>;
pub type InstParseResult<F, S> = Result<ParsedInstStream<F, S>, ParseError>;
pub type LineParseResult<F, S> = Result<OkParseResult<F, S>, ParseError>;

pub struct ParseResult<F, S>
where
    F: ArchFamily<S>,
    S: Data,
{
    pub file_id: FileId,
    pub insts: ParsedInstStream<F, S>,
    pub sections: SectionStore,
    pub declared_globals: HashSet<String>,
    pub reporter: ParseErrorReporter,
}

/// State about the program being parsed.
pub struct ParseState {
    /// The section in which parsed values should be placed.
    /// This should default to text.
    pub curr_section: ProgramSection,
    /// These labels were given to a .global declaration, which means either the
    /// current file defined the symbol and is making it visible to the linker, or
    /// the current file will look for the symbol in another file.
    pub declared_globals: HashSet<String>,
}

impl ParseState {
    pub fn new() -> ParseState {
        ParseState {
            curr_section: ProgramSection::Text,
            declared_globals: HashSet::new(),
        }
    }
}

impl Default for ParseState {
    fn default() -> Self {
        Self::new()
    }
}

pub type TokenIter = Peekable<IntoIter<Token>>;

pub trait Parser<F, S>
where
    F: ArchFamily<S>,
    S: Data,
{
    fn parse_str(file_id: FileId, contents: &str) -> ParseResult<F, S> {
        Self::parse_lex_result(Lexer::lex_str(file_id, contents))
    }

    fn parse_lex_result(lex_result: LexResult) -> ParseResult<F, S>;
}
