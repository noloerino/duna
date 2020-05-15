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

impl ImmRenderType {
    fn radix(self) -> u32 {
        match self {
            ImmRenderType::Bin => 2,
            ImmRenderType::Dec => 10,
            ImmRenderType::Hex => 16,
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

fn string_from_utf8(cs: Vec<u8>) -> String {
    String::from_utf8(cs).expect("Non-UTF08 token while lexing")
}

// fn build_err(msg: &str, contents: String) -> LexError {
//     LexError {
//         msg: msg.to_string(),
//         contents,
//     }
// }

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

fn build_comment(iter: &mut LineIter) -> TokenType {
    // assume leading # already consumed
    // just consume the rest of the line
    let mut cs = Vec::<u8>::new();
    for (_, c) in iter {
        cs.push(c as u8);
    }
    // TODO reimplement this uzing unzip
    TokenType::Comment(string_from_utf8(cs))
}

fn build_section_def(iter: &mut LineIter) -> TokenType {
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
    TokenType::SectionDef(string_from_utf8(cs))
}

fn build_name(iter: &mut LineIter, head: char) -> TokenType {
    let mut cs = Vec::<u8>::new();
    cs.push(head as u8);
    while let Some((_, c_ref)) = iter.peek() {
        // colon acts as a delimiter - if we hit a colon, we should start parsing the next token
        let c = *c_ref;
        if c == ':' {
            return TokenType::LabelDef(string_from_utf8(cs));
        }
        if is_delim(c) {
            break;
        }
        cs.push(c as u8);
        iter.next();
    }
    TokenType::Name(string_from_utf8(cs))
}

fn build_imm(iter: &mut LineIter, head: char) -> TokenType {
    // determines whether we negate at end
    let negate = head == '-';
    let mut digits = Vec::<char>::new();
    let mut fmt = ImmRenderType::Dec;
    // first two chars are special because they determine the number format
    let c1 = if head != '-' && head != '+' {
        head
    } else {
        match iter.peek() {
            Some((_, c_ref)) => {
                let c = *c_ref;
                if c.is_ascii_digit() {
                    iter.next();
                    c
                } else {
                    unimplemented!()
                }
            }
            None => unimplemented!(),
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
                    fmt = ImmRenderType::Hex;
                } else if c == 'b' {
                    fmt = ImmRenderType::Bin;
                } else {
                    println!("{}", c);
                    // assume base 10
                    if c.is_ascii_digit() {
                        digits.push(c1);
                        digits.push(c);
                    } else {
                        unimplemented!();
                    }
                }
            } else {
                // definitely base 10
                if c.is_ascii_digit() {
                    digits.push(c1);
                    digits.push(c);
                } else {
                    unimplemented!()
                }
            }
        }
    }
    iter.next();
    while let Some((_, c_ref)) = iter.peek() {
        let c = *c_ref;
        let push: bool = match fmt {
            ImmRenderType::Bin => c == '0' || c == '1',
            ImmRenderType::Hex => c.is_ascii_hexdigit(),
            ImmRenderType::Dec => c.is_ascii_digit(),
        };
        if push {
            digits.push(c);
            iter.next();
        } else {
            // quit
            unimplemented!();
        }
    }
    if let Ok(val) =
        i32::from_str_radix(digits.into_iter().collect::<String>().as_str(), fmt.radix())
    {
        TokenType::Immediate(if negate { -val } else { val }, fmt)
    } else {
        unimplemented!()
    }
}

fn lex_file(path: String) -> LineTokenStream {
    lex_string(fs::read_to_string(path).expect("Failed to open file"))
}

fn lex_string(contents: String) -> LineTokenStream {
    let mut toks = Vec::<TokenStream>::new();
    let lines = contents.as_str().lines();
    for (lineno, line) in lines.enumerate() {
        let iter = &mut line.chars().enumerate().peekable();
        toks.push(lex_line(iter, lineno));
    }
    toks
}

/// Generates a TokenStream for a line in a file.
fn lex_line(iter: &mut LineIter, lineno: LineNo) -> TokenStream {
    let mut toks = Vec::<Token>::new();
    while let Some((start_offs, c)) = iter.next() {
        // TODO refactor into Result to allow error reporting
        let maybe_next_tok = if is_name_start(c) {
            Some(build_name(iter, c))
        } else if is_imm_start(c) {
            Some(build_imm(iter, c))
        } else {
            match c {
                '.' => Some(build_section_def(iter)),
                ',' => Some(TokenType::Comma),
                '#' => Some(build_comment(iter)),
                '(' => Some(TokenType::LParen),
                ')' => Some(TokenType::RParen),
                // ':' => Some(build_err("Invalid token", char::to_string(c))),
                _ => None,
            }
        };
        if let Some(tok) = maybe_next_tok {
            toks.push(Token {
                lineno,
                offs: start_offs,
                data: tok,
            });
        }
    }
    toks
}

#[cfg(test)]
mod tests {
    use crate::lexer::*;

    #[test]
    fn test_simple_lex() {
        let toks = &lex_string("addi x0, x1, x2".to_string())[0];
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
            ("0x123", Hex, 0x123),
            ("0b101", Bin, 0b101),
            ("3", Dec, 3),
            ("874", Dec, 874),
            ("-0x123", Hex, -0x123),
            ("-0b101", Bin, -0b101),
            ("-3", Dec, -3),
            ("-874", Dec, -874),
        ];
        for (line, fmt, exp) in cases {
            let iter = &mut line.chars().enumerate().peekable();
            let (_, head) = iter.next().unwrap();
            let result = build_imm(iter, head);
            assert_eq!(result, TokenType::Immediate(exp, fmt));
        }
    }
}
