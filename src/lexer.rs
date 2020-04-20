use std::fs;

enum Token {
    Name(String),
    Comma,
    Immediate(i32),
    Comment(String),
}

type TokenStream = Vec<Token>;

fn string_from_utf8(cs: Vec<u8>) -> String {
    String::from_utf8(cs).expect("Non-UTF08 token while lexing")
}

fn build_comment(iter: &mut dyn Iterator<Item = char>) -> Token {
    let mut cs = Vec::<u8>::new();
    while let Some(c) = iter.next() {
        match c {
            '\n' => break,
            _ => cs.push(c as u8),
        }
    }
    Token::Comment(string_from_utf8(cs))
}

fn build_name(iter: &mut dyn Iterator<Item = char>) -> Token {
    let mut cs = Vec::<u8>::new();
    while let Some(c) = iter.next() {
        if c.is_whitespace() {
            break;
        }
        cs.push(c as u8)
    }
    Token::Name(string_from_utf8(cs))
}

fn lex_file(path: String) -> TokenStream {
    let contents = fs::read_to_string(path).expect("Failed to open file");
    let mut toks = Vec::<Token>::new();
    let iter = &mut contents.chars();
    while let Some(c) = iter.next() {
        let maybe_next_tok = match c {
            ',' => Some(Token::Comma),
            '#' => Some(build_comment(iter)),
            '\n' | ' ' => None,
            _ => None,
        };
        if let Some(tok) = maybe_next_tok {
            toks.push(tok);
        }
    }
    toks
}
