use super::parser::{ParseError, ParseErrorReporter};
use std::borrow::Cow;
use std::fmt;
use std::fs;
use std::iter::Enumerate;
use std::iter::Peekable;
use std::str::Chars;

pub struct LexResult {
    pub lines: LineTokenStream,
    pub reporter: ParseErrorReporter,
}

impl LexResult {
    #[cfg(test)]
    pub fn has_errors(&self) -> bool {
        !self.reporter.errs.is_empty()
    }
}

// The line number of a token.
pub type LineNo = usize;
// The offset of a token within a line.
pub type LineOffs = usize;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Location {
    /// Holds a reference to the line from which this came (used for debugging)
    // line_contents: &'a str,
    lineno: LineNo,
    offs: LineOffs,
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.lineno, self.offs)
    }
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

    pub fn format(self, n: i32) -> String {
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
    Immediate(i32, ImmRenderType),
    LParen,
    RParen,
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
    String::from_utf8(cs).expect("Non-UTF08 token while lexing")
}

fn string_from_chars(cs: Vec<char>) -> String {
    cs.into_iter().collect()
}

fn is_delim(c: char) -> bool {
    c.is_whitespace() || DELIMS.contains(&c)
}

fn is_name_start(c: char) -> bool {
    // minus signs and numbers cannot start names; for now we're conservative
    c == '_' || c.is_alphabetic()
}

fn is_imm_start(c: char) -> bool {
    c == '-' || c == '+' || c.is_ascii_digit()
}

struct LineLexer<'a> {
    // line_contents: &'a str,
    lineno: LineNo,
    iter: LineIter<'a>,
    reporter: &'a mut ParseErrorReporter,
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
                break;
            }
            cs.push(c as u8);
            self.iter.next();
        }
        Ok(TokenType::Directive(string_from_utf8(cs)))
    }

    fn build_name(&mut self, state: &LexState) -> Result<TokenType, ParseError> {
        let mut cs = Vec::<u8>::new();
        cs.push(state.head as u8);
        while let Some((_, c_ref)) = self.iter.peek() {
            // colon acts as a delimiter - if we hit a colon, we should start parsing the next token
            let c = *c_ref;
            if c == ':' {
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
                        return Err(ParseError::new(
                            Location {
                                offs,
                                ..state.location
                            },
                            "Cannot parse number literal",
                            &string_from_utf8(vec![head as u8, c as u8]),
                        ));
                    }
                }
                None => {
                    self.max_munch_on_error();
                    return Err(ParseError::new(
                        state.location,
                        "Ran out of characters while parsing number literal",
                        &head.to_string(),
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
                if c1 == '0' && c == 'x' {
                    fmt = Hex;
                } else if c1 == '0' && c == 'b' {
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
                            return Err(ParseError::new(
                                state.location,
                                "Error parsing base 10 integer literal",
                                &string_from_chars(digits),
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
                        return Err(ParseError::new(
                            state.location,
                            &format!(
                                "Error parsing {} integer literal",
                                match fmt {
                                    Bin => "binary",
                                    Hex => "hexademical",
                                    Dec => "decimal",
                                }
                            ),
                            &string_from_chars(digits),
                        ));
                    }
                }
            }
        }
        let digit_str = digits.into_iter().collect::<String>();
        // We need to use u32 to avoid overflow for large literals like 0xFFFF_FFFF
        if let Ok(unsgn_val) = u32::from_str_radix(&digit_str, fmt.radix()) {
            let val = unsgn_val as i32;
            Ok(TokenType::Immediate(if negate { -val } else { val }, fmt))
        } else {
            Err(ParseError::new(
                state.location,
                &format!("Malformed base {} immediate", fmt.radix()),
                &digit_str,
            ))
        }
    }

    fn new(lineno: LineNo, line: &'a str, reporter: &'a mut ParseErrorReporter) -> LineLexer<'a> {
        LineLexer {
            // line_contents: line,
            lineno,
            iter: line.chars().enumerate().peekable(),
            reporter,
        }
    }

    /// Generates a TokenStream for a line in a file.
    fn lex(mut self) -> TokenStream {
        let mut toks = Vec::<Token>::new();
        // let line_contents = self.line_contents;
        let lineno = self.lineno;
        while let Some((start_offs, c)) = self.iter.next() {
            let state = LexState {
                head: c,
                location: Location {
                    // line_contents,
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
                    _ => continue,
                }
            };
            match maybe_tok {
                Ok(tok) => toks.push(Token {
                    location: Location {
                        // line_contents,
                        lineno,
                        offs: start_offs,
                    },
                    data: tok,
                }),
                Err(err) => self.reporter.add_error(err),
            }
        }
        toks
    }
}

pub struct Lexer<'a> {
    contents: Cow<'a, str>,
}

impl<'a> Lexer<'a> {
    pub fn from_file(path: &str) -> Lexer {
        let contents = fs::read_to_string(path).expect("Failed to open file");
        Lexer {
            contents: Cow::Owned(contents),
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn from_str(contents: &str) -> Lexer {
        Lexer {
            contents: Cow::Borrowed(contents),
        }
    }

    /// Consume the lexer's iterator to produce a stream of tokens and any possible errors.
    pub fn lex(self) -> LexResult {
        let mut toks = Vec::<TokenStream>::new();
        let mut reporter = ParseErrorReporter::new();
        for (lineno, line) in self.contents.lines().enumerate() {
            toks.push(LineLexer::new(lineno, line, &mut reporter).lex());
        }
        LexResult {
            lines: toks,
            reporter,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_lex() {
        let LexResult { lines, reporter } = Lexer::from_str("addi x0, x1, x2").lex();
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
            ("0xABCD_0123", Hex, 0xABCD_0123u32 as i32),
        ];
        for (line, fmt, exp) in cases {
            let head = line.chars().next().unwrap();
            let mut reporter = Default::default();
            let mut lexer = LineLexer::new(0, line.get(1..).unwrap(), &mut reporter);
            let result = lexer.build_imm(&LexState {
                head,
                location: Location {
                    // line_contents: line,
                    lineno: 0,
                    offs: 0,
                },
            });
            assert_eq!(result, Ok(TokenType::Immediate(exp, fmt)));
        }
    }

    #[test]
    fn test_bad_imm_recovery() {
        let line = "addi x1 0xggg1, x2";
        let LexResult {
            lines, reporter, ..
        } = Lexer::from_str(line).lex();
        let errs = reporter.errs;
        assert_eq!(errs.len(), 1);
        assert_eq!(lines.len(), 1);
        let parsed = &lines[0];
        assert_eq!(parsed[0].data, TokenType::Name("addi".to_string()));
        assert_eq!(parsed[1].data, TokenType::Name("x1".to_string()));
        assert_eq!(parsed[2].data, TokenType::Comma);
        assert_eq!(parsed[3].data, TokenType::Name("x2".to_string()));
    }
}
