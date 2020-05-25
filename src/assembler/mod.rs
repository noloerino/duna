mod lexer;
mod parser;

use crate::program_state::RiscVProgram;
use lexer::Lexer;
pub use parser::ParseError;
use parser::RiscVParser;

pub struct Assembler {
    lexer: Lexer,
}

impl Assembler {
    pub fn from_file(path: &str) -> Assembler {
        Assembler {
            lexer: Lexer::from_file(path),
        }
    }

    pub fn from_string(contents: String) -> Assembler {
        Assembler {
            lexer: Lexer::from_string(contents),
        }
    }

    pub fn assemble(self) -> Result<RiscVProgram, Vec<ParseError>> {
        let (toks, lex_errs) = self.lexer.lex();
        let (insts, parse_errs) = RiscVParser::from_tokens(toks).parse();
        let mut all_errs = lex_errs;
        all_errs.extend(parse_errs);
        if all_errs.is_empty() {
            Ok(RiscVProgram::new(insts))
        } else {
            Err(all_errs)
        }
    }
}
