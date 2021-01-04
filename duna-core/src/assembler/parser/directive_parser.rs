use super::*;
use crate::{assembler::ErrMetadata, data_structures::*};

/// Responsible for parsing a line with a directive.
pub struct DirectiveParser<'a> {
    iter: TokenIter,
    state: &'a mut ParseState,
    head_loc: &'a Location,
    head_directive: &'a str,
}

type DirectiveParseResult = Result<Option<DirectiveLiterals>, ParseError>;

impl<'a> DirectiveParser<'a> {
    pub fn new(
        iter: TokenIter,
        state: &'a mut ParseState,
        head_loc: &'a Location,
        head_directive: &'a str,
    ) -> DirectiveParser<'a> {
        DirectiveParser {
            iter,
            state,
            head_loc,
            head_directive,
        }
    }

    pub fn parse(self) -> DirectiveParseResult {
        match self.head_directive {
            // sections
            "section" => self.parse_section(),
            "text" => {
                self.state.curr_section = ProgramSection::Text;
                self.ok(0)
            }
            "data" => {
                self.state.curr_section = ProgramSection::Data;
                self.ok(0)
            }
            "rodata" => {
                self.state.curr_section = ProgramSection::Rodata;
                self.ok(0)
            }
            // literal insertions
            "byte" => self.parse_data(DataWidthEnum::Byte),
            "2byte" | "half" | "short" => self.parse_data(DataWidthEnum::Half),
            "4byte" | "word" | "long" => self.parse_data(DataWidthEnum::Lword),
            "8byte" | "dword" | "quad" => self.parse_data(DataWidthEnum::Dword),
            "zero" => self.parse_zero(),
            "ascii" => self.parse_string(false),
            "asciz" | "string" => self.parse_string(true),
            // symbol declarations
            "global" | "globl" => self.parse_global_label(),
            // TODO: equ, set, equiv (refactor symbol table to have enum value)
            _ => Err(ParseError::unsupported_directive(
                ErrMetadata::new(&self.head_loc),
                self.head_directive,
            )),
        }
    }

    /// Emits zero or more integer literal declarations through .byte, .word, etc.
    fn parse_data(mut self, kind: DataWidthEnum) -> DirectiveParseResult {
        use ProgramSection::*;
        match self.state.curr_section {
            Text => Err(ParseError::unimplemented(
                ErrMetadata::new(&self.head_loc),
                "cannot insert literals in .text section (only instructions allowed)",
            )),
            section => {
                let toks = self.consume_unbounded_commasep_args()?;
                let mut data = DirectiveLiterals::new(section);
                for tok in toks {
                    use DataWidthEnum::*;
                    match kind {
                        Byte => {
                            let val: u8 = self.try_parse_imm(8, tok)? as u8;
                            data.add_byte(val);
                        }
                        Half => {
                            let val: u16 = self.try_parse_imm(16, tok)? as u16;
                            data.add_half(val);
                        }
                        Lword => {
                            let val: u32 = self.try_parse_imm(32, tok)? as u32;
                            data.add_word(val);
                        }
                        Dword => {
                            let val: u64 = self.try_parse_imm(32, tok)? as u64;
                            data.add_doubleword(val);
                        }
                    }
                }
                // should never fail since we've consumed the whole iterator
                self.ok(0)?;
                Ok(Some(data))
            }
        }
    }

    fn parse_section(mut self) -> DirectiveParseResult {
        let next_tok = self.try_next_tok(1, 0)?;
        if let TokenType::Directive(s) = &next_tok.data {
            match s.as_str() {
                "text" => {
                    self.state.curr_section = ProgramSection::Text;
                    return self.ok(1);
                }
                "data" => {
                    self.state.curr_section = ProgramSection::Data;
                    return self.ok(1);
                }
                "rodata" => {
                    self.state.curr_section = ProgramSection::Rodata;
                    return self.ok(1);
                }
                "bss" => {
                    return Err(ParseError::unimplemented(
                        ErrMetadata::new(&next_tok.location),
                        "bss section",
                    ))
                }
                _ => {}
            }
        }
        Err(ParseError::unexpected_type(
            ErrMetadata::new(&next_tok.location),
            "one of [.text, .bss, .data, .rodata]",
            next_tok.data,
        ))
    }

    /// Emits zero or more string literals.
    ///
    /// See https://sourceware.org/binutils/docs/as/Ascii.html#Ascii
    fn parse_string(mut self, null_terminated: bool) -> DirectiveParseResult {
        if let ProgramSection::Text = self.state.curr_section {
            return Err(ParseError::unimplemented(
                ErrMetadata::new(&self.head_loc),
                "cannot insert literals in .text section (only instructions allowed)",
            ));
        }
        let toks = self.consume_unbounded_commasep_args()?;
        let mut data = DirectiveLiterals::new(self.state.curr_section);
        for tok in toks {
            if let TokenType::StringLiteral(s) = &tok.data {
                for c in s.chars() {
                    data.add_byte(c as u8)
                }
                if null_terminated {
                    data.add_byte(0);
                }
            } else {
                return Err(ParseError::unexpected_type(
                    ErrMetadata::new(&tok.location),
                    "string literal",
                    tok.data,
                ));
            }
        }
        self.ok(0)?;
        Ok(Some(data))
    }

    fn parse_zero(mut self) -> DirectiveParseResult {
        if let ProgramSection::Text = self.state.curr_section {
            return Err(ParseError::unimplemented(
                ErrMetadata::new(&self.head_loc),
                "cannot insert literals in .text section (only instructions allowed)",
            ));
        }
        let next_tok = self.try_next_tok(1, 0)?;
        if let TokenType::Immediate(n, ..) = next_tok.data {
            // insert n bytes of zeroes
            if n < 0 {
                Err(ParseError::unexpected_type(
                    ErrMetadata::new(&next_tok.location),
                    "positive integer literal",
                    next_tok.data,
                ))
            } else {
                let mut data = DirectiveLiterals::new(self.state.curr_section);
                for _ in 0..n {
                    data.add_byte(0);
                }
                self.ok(1)?;
                Ok(Some(data))
            }
        } else {
            Err(ParseError::unexpected_type(
                ErrMetadata::new(&next_tok.location),
                "integer literal",
                next_tok.data,
            ))
        }
    }

    /// Indicates that a symbol is declared globally.
    fn parse_global_label(mut self) -> DirectiveParseResult {
        let next_tok = self.try_next_tok(1, 0)?;
        if let TokenType::Name(name) = next_tok.data {
            // announcing a variable as global multiple times is ok, so just insert without checking
            self.state.declared_globals.insert(name);
            self.ok(1)
        } else {
            Err(ParseError::unexpected_type(
                ErrMetadata::new(&next_tok.location),
                "integer literal",
                next_tok.data,
            ))
        }
    }

    fn try_next_tok(&mut self, needed_args: u8, found_so_far: u8) -> Result<Token, ParseError> {
        self.iter.try_next_tok(
            self.head_loc,
            self.head_directive,
            needed_args,
            found_so_far,
        )
    }

    fn try_parse_imm(&self, n: u8, token: Token) -> Result<i64, ParseError> {
        try_parse_imm(n, token)
    }

    fn consume_unbounded_commasep_args(&mut self) -> Result<Vec<Token>, ParseError> {
        self.iter.consume_unbounded_commasep_args()
    }

    /// Checks that the iterator has run out of tokens; returns ok if so.
    /// needed_argc is the number of arguments that were needed in total, not the number that
    /// still need to be consumed (which is always 0, since we're checking if we ran out of args).
    fn ok(mut self, needed_argc: u8) -> DirectiveParseResult {
        self.iter
            .check_no_more_args(self.head_directive, needed_argc)?;
        Ok(None)
    }
}
