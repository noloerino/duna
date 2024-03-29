use super::{
    datatypes::*,
    error::{ErrMetadata, ErrorReporter, ParseError},
};
use std::{
    fmt,
    iter::{Enumerate, Peekable},
    str::Chars,
};

pub struct LexResult<'a> {
    pub file_id: FileId,
    pub lines: LineTokenStream,
    pub contents: &'a str,
    pub reporter: ErrorReporter,
}

#[derive(Eq, PartialEq, Debug)]
pub struct Token {
    pub location: Location,
    pub data: TokenType,
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum ImmRenderType {
    Bin,
    Hex,
    Dec,
}

use ImmRenderType::*;
impl ImmRenderType {
    fn radix(self) -> u32 {
        match self {
            Bin => 2,
            Dec => 10,
            Hex => 16,
        }
    }

    pub fn format(self, n: i64) -> String {
        match self {
            Bin => format!("{:#b}", n),
            Dec => format!("{}", n),
            Hex => format!("{:#X}", n),
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum TokenType {
    /// A token possibly representing the name of an instruction, reg, or branch target.
    Name(String),
    /// A token representing the name of a label, without the trailing colon.
    LabelDef(String),
    /// A token representing an assembly directive, without the leading period.
    Directive(String),
    Comment(String),
    Comma,
    Immediate(i64, ImmRenderType),
    /// A token representing a string literal, without the surrounding quote marks.
    /// Escape sequences are translated.
    StringLiteral(String),
    LParen,
    RParen,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenType::*;
        match self {
            Name(name) => write!(f, "{}", name),
            LabelDef(label) => write!(f, "{}:", label),
            Directive(directive) => write!(f, ".{}", directive),
            Comment(comment) => write!(f, "#{}", comment),
            Comma => write!(f, ","),
            Immediate(n, render_type) => write!(f, "\"{}\"", render_type.format(*n)),
            StringLiteral(s) => write!(f, "{}", s),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
        }
    }
}

const DELIMS: [char; 6] = ['#', ':', ',', '(', ')', ' '];

pub type TokenStream = Vec<Token>;
pub type LineTokenStream = Vec<TokenStream>;
type LineIter<'a> = Peekable<Enumerate<Chars<'a>>>;

struct LexState {
    head: char,
    location: Location,
}

fn string_from_utf8(cs: Vec<u8>) -> String {
    String::from_utf8(cs).expect("Non-UTF-8 token while lexing")
}

fn string_from_chars(cs: Vec<char>) -> String {
    cs.into_iter().collect()
}

fn is_delim(c: char) -> bool {
    c.is_whitespace() || DELIMS.contains(&c)
}

fn is_name_start(c: char) -> bool {
    // minus signs and numbers cannot start names; for now we're conservative
    // dollar signs prefix registers in MIPS
    c == '$' || c == '_' || c.is_alphabetic()
}

fn is_imm_start(c: char) -> bool {
    c == '-' || c == '+' || c.is_ascii_digit()
}

struct LineLexer<'a> {
    file_id: FileId,
    lineno: LineNo,
    iter: LineIter<'a>,
    reporter: &'a mut ErrorReporter,
}

impl<'a> LineLexer<'a> {
    /// Advances the iterator until a delimiter token or end of line is encountered.
    /// Returns the tokens that were consumed.
    fn max_munch_on_error(&mut self) -> Vec<char> {
        let mut cs = Vec::new();
        let iter = &mut self.iter;
        while let Some((_, c)) = iter.peek() {
            if is_delim(*c) {
                break;
            }
            cs.push(*c);
            iter.next();
        }
        cs
    }

    fn build_comment(&mut self) -> Result<TokenType, ParseError> {
        // assume leading # already consumed
        // just consume the rest of the line
        let mut cs = Vec::<u8>::new();
        for (_, c) in &mut self.iter {
            cs.push(c as u8);
        }
        Ok(TokenType::Comment(string_from_utf8(cs)))
    }

    fn build_directive(&mut self) -> Result<TokenType, ParseError> {
        // assume leading . already consumed
        let mut cs = Vec::<u8>::new();
        while let Some((_, c_ref)) = self.iter.peek() {
            let c = *c_ref;
            if is_delim(c) {
                // sike, it's a label
                if c == ':' {
                    self.iter.next();
                    cs.insert(0, b'.');
                    return Ok(TokenType::LabelDef(string_from_utf8(cs)));
                }
                break;
            }
            cs.push(c as u8);
            self.iter.next();
        }
        Ok(TokenType::Directive(string_from_utf8(cs)))
    }

    fn build_name(&mut self, state: &LexState) -> Result<TokenType, ParseError> {
        let mut cs = vec![state.head as u8];
        while let Some((_, c_ref)) = self.iter.peek() {
            // colon acts as a delimiter - if we hit a colon, we should start parsing the next token
            let c = *c_ref;
            if c == ':' {
                // consume the trailing token so nothing else gets it by accident
                self.iter.next();
                return Ok(TokenType::LabelDef(string_from_utf8(cs)));
            }
            if is_delim(c) {
                break;
            }
            cs.push(c as u8);
            self.iter.next();
        }
        Ok(TokenType::Name(string_from_utf8(cs)))
    }

    fn build_imm(&mut self, state: &LexState) -> Result<TokenType, ParseError> {
        let LexState { head, .. } = *state;
        // determines whether we negate at end
        let negate = head == '-';
        let mut digits = Vec::<char>::new();
        let mut fmt = Dec;
        // first two chars are special because they determine the number format
        let c1 = if head != '-' && head != '+' {
            head
        } else {
            match self.iter.peek() {
                Some((offs_ref, c_ref)) => {
                    let c = *c_ref;
                    let offs = *offs_ref;
                    if c.is_ascii_digit() {
                        self.iter.next();
                        c
                    } else {
                        self.max_munch_on_error();
                        return Err(ParseError::generic(
                            ErrMetadata::new(&Location {
                                offs,
                                ..state.location
                            }),
                            &format!(
                                "cannot parse number literal {}",
                                string_from_utf8(vec![head as u8, c as u8])
                            ),
                        ));
                    }
                }
                None => {
                    self.max_munch_on_error();
                    return Err(ParseError::generic(
                        ErrMetadata::new(&state.location),
                        &format!(
                            "ran out of characters while parsing number literal {}",
                            head,
                        ),
                    ));
                }
            }
        };
        // check if we should keep going after first 2 chars
        let mut consume_done = false;
        match self.iter.peek() {
            // if only one char, return it as base 10 literal
            None => digits.push(c1),
            Some((_, c_ref)) => {
                let c = *c_ref;
                // possibly look for format specifier
                if c1 == '0' && (c == 'x' || c == 'X') {
                    fmt = Hex;
                } else if c1 == '0' && (c == 'b' || c == 'B') {
                    fmt = Bin;
                } else {
                    // definitely base 10
                    digits.push(c1);
                    if DELIMS.contains(&c) {
                        consume_done = true;
                    } else {
                        digits.push(c);
                        if !c.is_ascii_digit() {
                            self.max_munch_on_error();
                            return Err(ParseError::generic(
                                ErrMetadata::new(&state.location),
                                &format!(
                                    "unexpected character when parsing base 10 integer literal {}",
                                    string_from_chars(digits)
                                ),
                            ));
                        }
                    }
                }
            }
        }
        if !consume_done {
            self.iter.next();
            while let Some((_, c_ref)) = self.iter.peek() {
                let c = *c_ref;
                let push: bool = match fmt {
                    Bin => c == '0' || c == '1',
                    Hex => c.is_ascii_hexdigit(),
                    Dec => c.is_ascii_digit(),
                };
                if push {
                    digits.push(c);
                    self.iter.next();
                } else {
                    // allow separators for hex literals
                    if fmt == Hex && c == '_' {
                        self.iter.next();
                    } else if DELIMS.contains(&c) {
                        break;
                    } else {
                        digits.extend(self.max_munch_on_error());
                        // TODO separators and format specifier are ignored
                        return Err(ParseError::bad_int_literal(
                            ErrMetadata::new(&state.location),
                            fmt,
                            string_from_chars(digits),
                        ));
                    }
                }
            }
        }
        let digit_str = digits.into_iter().collect::<String>();
        // We need to use u64 to avoid overflow for large literals like 0xFFFF_FFFF_FFFF_FFFF
        if let Ok(unsgn_val) = u64::from_str_radix(&digit_str, fmt.radix()) {
            let val = unsgn_val as i64;
            Ok(TokenType::Immediate(if negate { -val } else { val }, fmt))
        } else {
            Err(ParseError::bad_int_literal(
                ErrMetadata::new(&state.location),
                fmt,
                digit_str,
            ))
        }
    }

    fn build_string_literal(&mut self, state: &LexState) -> Result<TokenType, ParseError> {
        // assume leading double quote mark was already consumed
        let mut cs = Vec::<char>::new();
        #[allow(clippy::while_let_on_iterator)]
        while let Some((_, c)) = self.iter.next() {
            match c {
                // found end quote, so return
                '\"' => return Ok(TokenType::StringLiteral(string_from_chars(cs))),
                // look for escape
                '\\' => {
                    let escaped = self.iter.next();
                    match escaped {
                        Some((offs2, c2)) => cs.push(match c2 {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '\"' => '\"',
                            '\\' => '\\',
                            _ => {
                                return Err(ParseError::bad_escape(
                                    ErrMetadata::new(&Location {
                                        offs: offs2,
                                        ..state.location
                                    }),
                                    c2,
                                ))
                            }
                        }),
                        None => {
                            return Err(ParseError::unclosed_string_literal(ErrMetadata::new(
                                &state.location,
                            )))
                        }
                    }
                }
                _ => cs.push(c),
            }
        }
        // iterator ran out - return error
        Err(ParseError::unclosed_string_literal(ErrMetadata::new(
            &state.location,
        )))
    }

    fn new(
        file_id: FileId,
        content: &'a str,
        lineno: LineNo,
        reporter: &'a mut ErrorReporter,
    ) -> LineLexer<'a> {
        LineLexer {
            file_id,
            lineno,
            iter: content.chars().enumerate().peekable(),
            reporter,
        }
    }

    /// Generates a TokenStream for a line in a file.
    fn lex(mut self) -> TokenStream {
        let mut toks = Vec::<Token>::new();
        let lineno = self.lineno;
        while let Some((start_offs, c)) = self.iter.next() {
            let state = LexState {
                head: c,
                location: Location {
                    file_id: self.file_id,
                    lineno,
                    offs: start_offs,
                },
            };
            let maybe_tok = if is_name_start(c) {
                self.build_name(&state)
            } else if is_imm_start(c) {
                self.build_imm(&state)
            } else {
                match c {
                    '.' => self.build_directive(),
                    ',' => Ok(TokenType::Comma),
                    '#' => self.build_comment(),
                    '(' => Ok(TokenType::LParen),
                    ')' => Ok(TokenType::RParen),
                    '\"' => self.build_string_literal(&state),
                    ' ' | '\t' => continue,
                    _ => Err(ParseError::generic(
                        ErrMetadata::new(&state.location),
                        &format!("unexpected token {}", c),
                    )),
                }
            };
            match maybe_tok {
                Ok(tok) => toks.push(Token {
                    location: Location {
                        file_id: self.file_id,
                        lineno,
                        offs: start_offs,
                    },
                    data: tok,
                }),
                Err(err) => self.reporter.add_error(err),
            }
        }
        // To prevent a redundant (and perhaps misleading) error appearing in the parser, we don't
        // return any of the lexed tokens.
        // However, if there are labels, we still emit them so the error doesn't propagate to the
        // assembler/linker.
        if self.reporter.is_empty() {
            toks
        } else {
            toks.into_iter()
                .filter(|tok| matches!(tok.data, TokenType::LabelDef(..)))
                .collect()
        }
    }
}

pub struct Lexer<'a> {
    file_id: FileId,
    contents: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn lex_str(file_id: FileId, contents: &'a str) -> LexResult<'a> {
        Lexer { file_id, contents }.lex()
    }

    /// Consume the lexer's iterator to produce a stream of tokens and any possible errors.
    fn lex(self) -> LexResult<'a> {
        let mut toks = Vec::<TokenStream>::new();
        let mut reporter = ErrorReporter::new();
        for (lineno, line) in self.contents.lines().enumerate() {
            toks.push(LineLexer::new(self.file_id, line, lineno, &mut reporter).lex());
        }
        LexResult {
            file_id: self.file_id,
            lines: toks,
            contents: self.contents,
            reporter,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_test_reporter() -> ErrorReporter {
        ErrorReporter::new()
    }

    // Gets a LineLexer instance and an initialized state for more fine-grained testing.
    fn get_line_lexer<'a>(
        reporter: &'a mut ErrorReporter,
        line: &'a str,
    ) -> (LineLexer<'a>, LexState) {
        let head = line.chars().next().unwrap();
        let lexer = LineLexer::new(0, line.get(1..).unwrap(), 0, reporter);
        (
            lexer,
            LexState {
                head,
                location: Location {
                    file_id: 0,
                    lineno: 0,
                    offs: 0,
                },
            },
        )
    }

    #[test]
    fn test_simple_lex() {
        let LexResult {
            lines, reporter, ..
        } = Lexer::lex_str(0, "addi x0, x1, x2");
        assert!(reporter.is_empty());
        let toks = &lines[0];
        // check actual data
        assert_eq!(toks[0].data, TokenType::Name("addi".to_string()));
        assert_eq!(toks[1].data, TokenType::Name("x0".to_string()));
        assert_eq!(toks[2].data, TokenType::Comma);
        assert_eq!(toks[3].data, TokenType::Name("x1".to_string()));
        assert_eq!(toks[4].data, TokenType::Comma);
        assert_eq!(toks[5].data, TokenType::Name("x2".to_string()));
        // check line offsets
        for tok in toks {
            assert_eq!(tok.location.lineno, 0);
        }
        assert_eq!(toks[0].location.offs, 0);
        assert_eq!(toks[1].location.offs, 5);
        assert_eq!(toks[2].location.offs, 7);
        assert_eq!(toks[3].location.offs, 9);
        assert_eq!(toks[4].location.offs, 11);
        assert_eq!(toks[5].location.offs, 13);
    }

    #[test]
    fn test_imm_builder_good() {
        use ImmRenderType::*;
        let cases = vec![
            ("0x1a3", Hex, 0x1a3),
            ("0b101", Bin, 0b101),
            ("3", Dec, 3),
            ("874", Dec, 874),
            ("-0x1a3", Hex, -0x1a3),
            ("-0b101", Bin, -0b101),
            ("-3", Dec, -3),
            ("-874", Dec, -874),
            ("0xABCD_0123", Hex, 0xABCD_0123_i64),
        ];
        for (line, fmt, exp) in cases {
            let mut reporter = get_test_reporter();
            let (mut lexer, state) = get_line_lexer(&mut reporter, line);
            let result = lexer.build_imm(&state);
            assert!(reporter.is_empty());
            assert_eq!(result, Ok(TokenType::Immediate(exp, fmt)));
        }
    }

    /// Tests that a bad immediate is max munched.
    #[test]
    fn test_bad_imm_report() {
        let line = "addi x1 0xggg1, x2";
        let mut reporter = get_test_reporter();
        let (lexer, _state) = get_line_lexer(&mut reporter, line);
        let tokens = lexer.lex();
        let report = reporter.into_report_with_file_map(vec![FileData::from_test_program(line)]);
        assert!(!report.is_empty());
        // TODO make this into 0xggg1 to ensure it gets the whole thing
        assert!(format!("{:?}", report).contains("ggg1"));
        assert!(tokens.is_empty());
    }

    /// Tests that if there are multiple malformed immediates on the same line, all are reported.
    /// Though no instruction requires multiple immediates, this is implemented to check if in
    /// general, the presence of multiple syntax errors is handled properly.
    #[test]
    fn test_multi_bad_imm_report() {
        let line = "addi x1 1ggg1, 12kjkj03";
        let mut reporter = get_test_reporter();
        let (lexer, _state) = get_line_lexer(&mut reporter, line);
        let _tokens = lexer.lex();
        let report = reporter.into_report_with_file_map(vec![FileData::from_test_program(line)]);
        assert_eq!(report.get_errs().len(), 2);
    }

    #[test]
    fn test_bad_string_literals() {
        let line = "
            .string \"there's no end quote at the end of this line -->
            .string \"this is an invalid escape \\,\"
            nop
            ";
        let mut reporter = get_test_reporter();
        let (lexer, _state) = get_line_lexer(&mut reporter, line);
        let _tokens = lexer.lex();
        let report = reporter.into_report_with_file_map(vec![FileData::from_test_program(line)]);
        assert_eq!(report.get_errs().len(), 2);
    }

    #[test]
    fn test_string_literal() {
        // the program sees a carefully escaped newline
        let line = ".string \"howdy world\\n\"";
        let LexResult {
            lines, reporter, ..
        } = Lexer::lex_str(0, line);
        assert!(reporter.is_empty());
        assert_eq!(lines.len(), 1);
        let toks = &lines[0];
        assert_eq!(toks.len(), 2);
        assert_eq!(toks[0].data, TokenType::Directive("string".to_string()));
        assert_eq!(
            toks[1].data,
            TokenType::StringLiteral("howdy world\n".to_string())
        );
    }

    /// Tests labels that start with a period to make sure that they don't get parsed
    /// as directives by mistake.
    #[test]
    fn test_label_leading_period() {
        let line = ".L1:";
        let LexResult {
            lines, reporter, ..
        } = Lexer::lex_str(0, line);
        assert!(reporter.is_empty());
        assert_eq!(lines.len(), 1);
        let toks = &lines[0];
        assert_eq!(toks.len(), 1);
        assert_eq!(toks[0].data, TokenType::LabelDef(".L1".to_string()));
    }
}
