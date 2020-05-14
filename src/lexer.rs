use std::fs;
use std::iter::Enumerate;
use std::iter::Peekable;
use std::str::Chars;

// The line number of a token.
type LineNo = usize;
// The offset of a token within a line.
type LineOffs = usize;

struct LexError {
    lineno: LineNo,
    offs: LineOffs,
    msg: String,
    contents: String,
}

#[derive(Eq, PartialEq, Debug)]
struct Token {
    lineno: LineNo,
    offs: LineOffs,
    data: TokenType,
}

#[allow(dead_code)]
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
    Immediate(i32),
    LParen,
    RParen,
    Newline,
}

const DELIMS: [char; 5] = ['#', ':', ',', '(', ')'];

type TokenStream = Vec<Token>;
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
    c == '-' || c.is_ascii_digit()
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
    unimplemented!()
}

fn lex_file(path: String) -> TokenStream {
    lex_string(fs::read_to_string(path).expect("Failed to open file"))
}

fn lex_string(contents: String) -> TokenStream {
    let mut toks = Vec::<Token>::new();
    let lines = contents.as_str().lines();
    for (lineno, line) in lines.enumerate() {
        let iter = &mut line.chars().enumerate().peekable();
        toks.extend(lex_line(iter, lineno));
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
                // rules for section definitions are same for rules of names
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
        let toks = lex_string("addi x0, x1, x2".to_string());
        // check actual data
        assert_eq!(toks[0].data, TokenType::Name("addi".to_string()));
        assert_eq!(toks[1].data, TokenType::Name("x0".to_string()));
        assert_eq!(toks[2].data, TokenType::Comma);
        assert_eq!(toks[3].data, TokenType::Name("x1".to_string()));
        assert_eq!(toks[4].data, TokenType::Comma);
        assert_eq!(toks[5].data, TokenType::Name("x2".to_string()));
        // check line offsets
        for tok in &toks {
            assert_eq!(tok.lineno, 0);
        }
        assert_eq!(toks[0].offs, 0);
        assert_eq!(toks[1].offs, 5);
        assert_eq!(toks[2].offs, 7);
        assert_eq!(toks[3].offs, 9);
        assert_eq!(toks[4].offs, 11);
        assert_eq!(toks[5].offs, 13);
    }
}
