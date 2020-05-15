use std::fs;
use std::iter::Enumerate;
use std::iter::Peekable;
use std::str::Chars;

// The line number of a token.
type LineNo = usize;
// The offset of a token within a line.
type LineOffs = usize;

#[derive(Eq, PartialEq, Debug)]
struct LexErrorData {
    msg: String,
    contents: String,
}

#[derive(Eq, PartialEq, Debug)]
struct LexError {
    lineno: LineNo,
    offs: LineOffs,
    data: LexErrorData,
}

impl LexError {
    fn new(state: &LexState, msg: String, contents: String) -> Self {
        LexError {
            lineno: state.lineno,
            offs: state.offs,
            data: LexErrorData { msg, contents },
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
struct Token {
    lineno: LineNo,
    offs: LineOffs,
    data: TokenType,
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
enum ImmRenderType {
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
}

#[derive(Eq, PartialEq, Debug)]
enum TokenType {
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

type TokenStream = Vec<Token>;
type LineTokenStream = Vec<TokenStream>;
type LineIter<'a> = Peekable<Enumerate<Chars<'a>>>;

struct LexState {
    head: char,
    lineno: LineNo,
    offs: LineOffs,
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

fn build_comment(iter: &mut LineIter) -> Result<TokenType, LexError> {
    // assume leading # already consumed
    // just consume the rest of the line
    let mut cs = Vec::<u8>::new();
    for (_, c) in iter {
        cs.push(c as u8);
    }
    // TODO reimplement this uzing unzip
    Ok(TokenType::Comment(string_from_utf8(cs)))
}

fn build_section_def(iter: &mut LineIter) -> Result<TokenType, LexError> {
    // assume leading . already consumed
    let mut cs = Vec::<u8>::new();
    while let Some((_, c_ref)) = iter.peek() {
        let c = *c_ref;
        if is_delim(c) {
            break;
        }
        cs.push(c as u8);
        iter.next();
    }
    Ok(TokenType::SectionDef(string_from_utf8(cs)))
}

fn build_name(iter: &mut LineIter, state: &LexState) -> Result<TokenType, LexError> {
    let mut cs = Vec::<u8>::new();
    cs.push(state.head as u8);
    while let Some((_, c_ref)) = iter.peek() {
        // colon acts as a delimiter - if we hit a colon, we should start parsing the next token
        let c = *c_ref;
        if c == ':' {
            return Ok(TokenType::LabelDef(string_from_utf8(cs)));
        }
        if is_delim(c) {
            break;
        }
        cs.push(c as u8);
        iter.next();
    }
    Ok(TokenType::Name(string_from_utf8(cs)))
}

fn build_imm(iter: &mut LineIter, state: &LexState) -> Result<TokenType, LexError> {
    let LexState { head, .. } = *state;
    // determines whether we negate at end
    let negate = head == '-';
    let mut digits = Vec::<char>::new();
    let mut fmt = Dec;
    // first two chars are special because they determine the number format
    let c1 = if head != '-' && head != '+' {
        head
    } else {
        match iter.peek() {
            Some((offs, c_ref)) => {
                let c = *c_ref;
                if c.is_ascii_digit() {
                    iter.next();
                    c
                } else {
                    return Err(LexError::new(
                        &LexState {
                            offs: *offs,
                            ..*state
                        },
                        "Cannot parse number literal".to_string(),
                        string_from_utf8(vec![head as u8, c as u8]),
                    ));
                }
            }
            None => {
                return Err(LexError::new(
                    state,
                    "Ran out of characters while parsing number literal".to_string(),
                    head.to_string(),
                ))
            }
        }
    };
    match iter.peek() {
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
                    println!("{}", c);
                    // assume base 10
                    digits.push(c1);
                    digits.push(c);
                    if !c.is_ascii_digit() {
                        return Err(LexError::new(
                            state,
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
                    return Err(LexError::new(
                        state,
                        "Error parsing base 10 integer literal".to_string(),
                        digits.into_iter().collect(),
                    ));
                }
            }
        }
    }
    iter.next();
    while let Some((_, c_ref)) = iter.peek() {
        let c = *c_ref;
        let push: bool = match fmt {
            Bin => c == '0' || c == '1',
            Hex => c.is_ascii_hexdigit(),
            Dec => c.is_ascii_digit(),
        };
        if push {
            digits.push(c);
            iter.next();
        } else {
            return Err(LexError::new(
                state,
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

#[allow(dead_code)]
fn lex_file(path: String) -> (LineTokenStream, Vec<LexError>) {
    lex_string(fs::read_to_string(path).expect("Failed to open file"))
}

fn lex_string(contents: String) -> (LineTokenStream, Vec<LexError>) {
    let mut toks = Vec::<TokenStream>::new();
    let mut errs = Vec::<LexError>::new();
    let lines = contents.as_str().lines();
    for (lineno, line) in lines.enumerate() {
        let iter = &mut line.chars().enumerate().peekable();
        toks.push(lex_line(iter, lineno, &mut errs));
    }
    (toks, errs)
}

/// Generates a TokenStream for a line in a file.
fn lex_line(iter: &mut LineIter, lineno: LineNo, errs: &mut Vec<LexError>) -> TokenStream {
    let mut toks = Vec::<Token>::new();
    while let Some((start_offs, c)) = iter.next() {
        let state = LexState {
            head: c,
            lineno,
            offs: start_offs,
        };
        let maybe_tok = if is_name_start(c) {
            build_name(iter, &state)
        } else if is_imm_start(c) {
            build_imm(iter, &state)
        } else {
            match c {
                '.' => build_section_def(iter),
                ',' => Ok(TokenType::Comma),
                '#' => build_comment(iter),
                '(' => Ok(TokenType::LParen),
                ')' => Ok(TokenType::RParen),
                // ':' => Some(build_err("Invalid token", char::to_string(c))),
                _ => continue,
            }
        };
        match maybe_tok {
            Ok(tok) => toks.push(Token {
                lineno,
                offs: start_offs,
                data: tok,
            }),
            Err(err) => errs.push(err),
        }
    }
    toks
}

#[cfg(test)]
mod tests {
    use crate::lexer::*;

    #[test]
    fn test_simple_lex() {
        let (lines, errs) = lex_string("addi x0, x1, x2".to_string());
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
            assert_eq!(tok.lineno, 0);
        }
        assert_eq!(toks[0].offs, 0);
        assert_eq!(toks[1].offs, 5);
        assert_eq!(toks[2].offs, 7);
        assert_eq!(toks[3].offs, 9);
        assert_eq!(toks[4].offs, 11);
        assert_eq!(toks[5].offs, 13);
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
            let iter = &mut line.chars().enumerate().peekable();
            let (_, head) = iter.next().unwrap();
            let result = build_imm(
                iter,
                &LexState {
                    head,
                    lineno: 0,
                    offs: 0,
                },
            );
            assert_eq!(result, Ok(TokenType::Immediate(exp, fmt)));
        }
    }
}
