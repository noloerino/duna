//! Describes traits needed for parsing an assembly language. Still WIP, so relatively sparse
//! documentation.
//!
//! Most features are tested by the RISC-V module.
mod directive_parser;
mod inst_parser;
mod line_parser;

use super::{
    assembler_impl::{ProgramSection, SectionStore},
    datatypes::*,
    error::{ErrMetadata, ErrorReporter, ParseError},
    lexer::*,
    partial_inst::PartialInst,
};
use crate::{arch::*, data_structures::*, program_state::IRegister};
pub(crate) use directive_parser::DirectiveParser;
pub use inst_parser::*;
use line_parser::*;
use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
    vec::IntoIter,
};

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
        self.data.push(DataEnum::Lword(val.into()));
    }

    pub fn add_doubleword(&mut self, val: u64) {
        self.data.push(DataEnum::Dword(val.into()));
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

pub enum OkParseResult<F: ArchFamily<S>, S: DataWidth> {
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
    S: DataWidth,
{
    pub file_id: FileId,
    pub insts: ParsedInstStream<F, S>,
    pub sections: SectionStore,
    pub declared_globals: HashSet<String>,
    pub reporter: ErrorReporter,
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

pub struct TokenIter(Peekable<IntoIter<Token>>);

impl std::iter::Iterator for TokenIter {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.0.next()
    }
}

impl TokenIter {
    pub fn peek(&mut self) -> Option<&Token> {
        self.0.peek()
    }

    /// Checks this line's iterator to ensure that there are no more tokens remaining, save
    /// for a possible comment.
    /// This is used for situations where a fixed number of arguments is expected, as we're
    /// free to consume the iterator since more tokens would be an error regardless.
    pub fn check_no_more_args(&mut self, name: &str, needed: u8) -> Result<(), ParseError> {
        let next = self.next();
        if let Some(tok) = next {
            if let TokenType::Comment(_) = tok.data {
                Ok(())
            } else {
                Err(ParseError::too_many_args(
                    ErrMetadata::new(&tok.location),
                    name,
                    needed,
                ))
            }
        } else {
            Ok(())
        }
    }

    /// Attempts to advance the next token of the iterator, returning a ParseError if there are none.
    pub fn try_next_tok(
        &mut self,
        head_loc: &Location,
        name: &str,
        needed_args: u8,
        found_so_far: u8,
    ) -> Result<Token, ParseError> {
        if let Some(tok) = self.next() {
            Ok(tok)
        } else {
            Err(ParseError::wrong_argc(
                ErrMetadata::new(head_loc),
                name,
                needed_args,
                found_so_far,
            ))
        }
    }

    /// Attempts to consume possibly comma-separate arguments from the iterator.
    /// The first and last tokens cannot be commas. If repeated commas appear anywhere,
    /// an error is returned.
    /// This consumes until a comment token or the end of the iterator is reached.
    pub fn consume_unbounded_commasep_args(&mut self) -> Result<Vec<Token>, ParseError> {
        use TokenType::*;
        let mut toks = Vec::new();
        // track if we just visited a comma
        // initialize to true to prevent leading commas
        let mut was_comma = true;
        for tok in self {
            match tok.data {
                Name(..) | Immediate(..) | StringLiteral(..) => {
                    was_comma = false;
                    toks.push(tok)
                }
                Comma => {
                    if was_comma {
                        return Err(ParseError::bad_arg(
                            ErrMetadata::new(&tok.location),
                            &format!("{:?}", tok.data),
                        ));
                    }
                    was_comma = true;
                }
                Comment(..) => return Ok(toks),
                _ => {
                    return Err(ParseError::bad_arg(
                        ErrMetadata::new(&tok.location),
                        &format!("{:?}", tok.data),
                    ));
                }
            }
        }
        Ok(toks)
    }
}

pub struct ParserData<R: 'static + IRegister, ParseType: 'static> {
    inst_expansion_table: &'static HashMap<String, ParseType>,
    reg_expansion_table: &'static HashMap<String, R>,
}

// pain
pub(crate) type _ParserData<A> = ParserData<
    <<A as Architecture>::Family as ArchFamily<<A as Architecture>::DataWidth>>::Register,
    <<A as Architecture>::InstParser as InstParser<
        <A as Architecture>::Family,
        <A as Architecture>::DataWidth,
    >>::ParseType,
>;

pub struct Parser<A: Architecture> {
    file_id: FileId,
    lines: LineTokenStream,
    reporter: ErrorReporter,
    state: ParseState,
    parser_data: _ParserData<A>,
}

impl<A> Parser<A>
where
    A: Architecture,
{
    pub fn parse_str(file_id: FileId, contents: &str) -> ParseResult<A::Family, A::DataWidth> {
        Self::parse_lex_result(Lexer::lex_str(file_id, contents))
    }

    pub fn parse_lex_result(lex_result: LexResult) -> ParseResult<A::Family, A::DataWidth> {
        Self {
            file_id: lex_result.file_id,
            lines: lex_result.lines,
            reporter: lex_result.reporter,
            state: ParseState::new(),
            parser_data: ParserData {
                inst_expansion_table:
                    <A::InstParser as InstParser<A::Family, A::DataWidth>>::inst_expansion_table(),
                reg_expansion_table:
                    <A::InstParser as InstParser<A::Family, A::DataWidth>>::reg_expansion_table(),
            },
        }
        .parse()
    }

    fn parse(mut self) -> ParseResult<A::Family, A::DataWidth> {
        let mut insts = Vec::<PartialInst<A::Family, A::DataWidth>>::new();
        let mut last_label: Option<LabelDef> = None;
        let mut sections = SectionStore::new();
        let parser_data = &self.parser_data;
        for line in self.lines {
            // line is an iterator over tokens
            // contents is the raw string
            let (found_label, parse_result) =
                LineParser::<A>::new(parser_data, line, &last_label, &mut self.state).parse();
            match parse_result {
                Ok(ok_result) => {
                    // each branch should return the label to apply to the next instruction
                    last_label = match ok_result {
                        OkParseResult::Insts(mut new_insts) => {
                            // if insts is not empty, then that means the label gets used
                            if new_insts.is_empty() {
                                found_label
                            } else {
                                // stick label onto first inst
                                if let Some(new_label) = found_label {
                                    let head_inst = new_insts.remove(0).with_label(new_label);
                                    new_insts.insert(0, head_inst);
                                }
                                insts.extend(new_insts);
                                None
                            }
                        }
                        OkParseResult::Literals(DirectiveLiterals { section, data }) => {
                            let mut data_iter = data.into_iter();
                            let first: Option<DataEnum> = data_iter.next();
                            // if literals is not empty, then the label is going to be used
                            match first {
                                None => found_label,
                                Some(first_val) => {
                                    // stick label onto the first literal
                                    sections.add(section, found_label, first_val);
                                    for val in data_iter {
                                        sections.add(section, None, val);
                                    }
                                    None
                                }
                            }
                        }
                        OkParseResult::None => found_label,
                    }
                }
                Err(new_err) => self.reporter.add_error(new_err),
            }
        }
        ParseResult {
            file_id: self.file_id,
            insts,
            sections,
            declared_globals: self.state.declared_globals,
            reporter: self.reporter,
        }
    }
}

// Utility functions

/// Convenience method to stuff a PartialInst into a Vec<PartialInst>
pub fn ok_vec<F: ArchFamily<S>, S: AtLeast32b>(inst: PartialInst<F, S>) -> InstParseResult<F, S> {
    Ok(vec![inst])
}

/// Convenience method to stuff an Inst into Ok(vec![PartialInst(...)])
pub fn ok_wrap_concr<F: ArchFamily<S>, S: AtLeast32b>(
    inst: F::Instruction,
) -> InstParseResult<F, S> {
    ok_vec(PartialInst::new_complete(inst))
}

/// Convenience method to turn a Vec<Inst<S>> into Ok(Vec<PartialInst>)
pub fn ok_wrap_expanded<F: ArchFamily<S>, S: AtLeast32b>(
    inst: Vec<F::Instruction>,
) -> InstParseResult<F, S> {
    Ok(inst.into_iter().map(PartialInst::new_complete).collect())
}

/// Parses an immediate that is required to be at most n bits.
/// If the provided immediate is a negative, then the upper (64 - n + 1) bits must all be 1.
/// An i64 is returned, which should then be converted into a RegData type by a parser.
pub fn try_parse_imm(n: u8, token: Token) -> Result<i64, ParseError> {
    match token.data {
        // Check lower n bits
        // We give a pass to negative numbers with high bits set
        TokenType::Immediate(val, radix) => {
            let mask = if n == 64 {
                // Prevent shift overflow
                0
            } else {
                (-1) << (if val < 0 {
                    // Allow the sign bit to be part of the mask
                    n - 1
                } else {
                    n
                })
            };
            let mask_result = val & mask;
            if mask_result != 0 && mask_result != mask {
                Err(ParseError::imm_too_big(
                    ErrMetadata::new(&token.location),
                    n,
                    &radix.format(val),
                ))
            } else {
                Ok(val)
            }
        }
        _ => Err(ParseError::unexpected_type(
            ErrMetadata::new(&token.location),
            "immediate",
            token.data,
        )),
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    /// Lexes a program. Asserts that the lex has no errors.
    pub fn lex(prog: &str) -> LexResult {
        let result = Lexer::lex_str(0, prog);
        assert_eq!(result.reporter.get_errs(), &[]);
        result
    }

    /// Parses and lexes the provided string, assuming that there are no errors in either phase.
    /// Assumes that there were no lex errors.
    pub fn parse_and_lex<A: Architecture>(prog: &str) -> Vec<PartialInst<A::Family, A::DataWidth>> {
        let ParseResult {
            insts, reporter, ..
        } = Parser::<A>::parse_lex_result(lex(prog));
        assert!(reporter.is_empty(), "{:?}", reporter);
        insts
    }

    /// Parses and lexes a string assuming it contains instructions that don't need expanding.
    pub fn parse_and_lex_concr<A: Architecture>(
        prog: &str,
    ) -> Vec<<A::Family as ArchFamily<A::DataWidth>>::Instruction> {
        parse_and_lex::<A>(prog)
            .into_iter()
            .map(|inst| inst.try_into_concrete_inst())
            .collect()
    }
}
