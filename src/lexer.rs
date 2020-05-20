use crate::parser::ParseError;
use std::fs;
use std::iter::Enumerate;
use std::iter::Peekable;
use std::str::Chars;

// The line number of a token.
pub type LineNo = usize;
// The offset of a token within a line.
pub type LineOffs = usize;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Location {
    lineno: LineNo,
    offs: LineOffs,
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
            Bin => format!("{}", n),
            Dec => format!("{:#b}", n),
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
    /// A token representing a section label, without the leading period.
    SectionDef(String),
    Comment(String),
    Comma,
    Immediate(i32, ImmRenderType),
    LParen,
    RParen,
}

const DELIMS: [char; 5] = ['#', ':', ',', '(', ')'];

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
    lineno: LineNo,
    iter: LineIter<'a>,
}

impl LineLexer<'_> {
    fn build_comment(&mut self) -> Result<TokenType, ParseError> {
        // assume leading # already consumed
        // just consume the rest of the line
        let mut cs = Vec::<u8>::new();
        for (_, c) in &mut self.iter {
            cs.push(c as u8);
        }
        // TODO reimplement this uzing unzip
        Ok(TokenType::Comment(string_from_utf8(cs)))
    }

    fn build_section_def(&mut self) -> Result<TokenType, ParseError> {
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
        Ok(TokenType::SectionDef(string_from_utf8(cs)))
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
        // TODO implement max munch for error handling
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
                Some((offs, c_ref)) => {
                    let c = *c_ref;
                    if c.is_ascii_digit() {
                        self.iter.next();
                        c
                    } else {
                        return Err(ParseError::new(
                            Location {
                                offs: *offs,
                                ..state.location
                            },
                            "Cannot parse number literal".to_string(),
                            string_from_utf8(vec![head as u8, c as u8]),
                        ));
                    }
                }
                None => {
                    return Err(ParseError::new(
                        state.location,
                        "Ran out of characters while parsing number literal".to_string(),
                        head.to_string(),
                    ))
                }
            }
        };
        match self.iter.peek() {
            // if only one char, return it as base 10 literal
            None => digits.push(c1),
            Some((_, c_ref)) => {
                let c = *c_ref;
                // possibly look for format specifier
                if c1 == '0' {
                    if c == 'x' {
                        fmt = Hex;
                    } else if c == 'b' {
                        fmt = Bin;
                    } else {
                        // assume base 10
                        digits.push(c1);
                        digits.push(c);
                        if !c.is_ascii_digit() {
                            return Err(ParseError::new(
                                state.location,
                                "Error parsing base 10 integer literal".to_string(),
                                digits.into_iter().collect(),
                            ));
                        }
                    }
                } else {
                    // definitely base 10
                    digits.push(c1);
                    digits.push(c);
                    if !c.is_ascii_digit() {
                        return Err(ParseError::new(
                            state.location,
                            "Error parsing base 10 integer literal".to_string(),
                            digits.into_iter().collect(),
                        ));
                    }
                }
            }
        }
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
                return Err(ParseError::new(
                    state.location,
                    format!(
                        "Error parsing {} integer literal",
                        match fmt {
                            Bin => "binary",
                            Hex => "hexademical",
                            Dec => "decimal",
                        }
                    ),
                    digits.into_iter().collect(),
                ));
            }
        }
        if let Ok(val) =
            i32::from_str_radix(digits.into_iter().collect::<String>().as_str(), fmt.radix())
        {
            Ok(TokenType::Immediate(if negate { -val } else { val }, fmt))
        } else {
            unimplemented!()
        }
    }

    fn new(lineno: LineNo, iter: LineIter) -> LineLexer {
        LineLexer { lineno, iter }
    }

    /// Generates a TokenStream for a line in a file.
    fn lex(mut self, errs: &mut Vec<ParseError>) -> TokenStream {
        let mut toks = Vec::<Token>::new();
        while let Some((start_offs, c)) = self.iter.next() {
            let state = LexState {
                head: c,
                location: Location {
                    lineno: self.lineno,
                    offs: start_offs,
                },
            };
            let maybe_tok = if is_name_start(c) {
                self.build_name(&state)
            } else if is_imm_start(c) {
                self.build_imm(&state)
            } else {
                match c {
                    '.' => self.build_section_def(),
                    ',' => Ok(TokenType::Comma),
                    '#' => self.build_comment(),
                    '(' => Ok(TokenType::LParen),
                    ')' => Ok(TokenType::RParen),
                    // ':' => Some(build_err("Invalid token", char::to_string(c))),
                    _ => continue,
                }
            };
            match maybe_tok {
                Ok(tok) => toks.push(Token {
                    location: Location {
                        lineno: self.lineno,
                        offs: start_offs,
                    },
                    data: tok,
                }),
                Err(err) => errs.push(err),
            }
        }
        toks
    }
}

pub struct Lexer {
    contents: String,
}

impl Lexer {
    #[allow(dead_code)]
    pub fn from_file(path: &str) -> Lexer {
        let contents = fs::read_to_string(path).expect("Failed to open file");
        Lexer::from_string(contents)
    }

    pub fn from_string(contents: String) -> Lexer {
        Lexer { contents }
    }

    /// Consume the lexer's iterator to produce a stream of tokens and any possible errors.
    pub fn lex(self) -> (LineTokenStream, Vec<ParseError>) {
        let mut toks = Vec::<TokenStream>::new();
        let mut errs = Vec::<ParseError>::new();
        for (lineno, line) in self.contents.lines().enumerate() {
            toks.push(LineLexer::new(lineno, line.chars().enumerate().peekable()).lex(&mut errs));
        }
        (toks, errs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_lex() {
        let (lines, errs) = Lexer::from_string("addi x0, x1, x2".to_string()).lex();
        let toks = &lines[0];
        assert!(errs.is_empty());
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
        ];
        for (line, fmt, exp) in cases {
            let mut iter = line.chars().enumerate().peekable();
            let (_, head) = iter.next().unwrap();
            let mut lexer = LineLexer::new(0, iter);
            let result = lexer.build_imm(&LexState {
                head,
                location: Location { lineno: 0, offs: 0 },
            });
            assert_eq!(result, Ok(TokenType::Immediate(exp, fmt)));
        }
    }
}
