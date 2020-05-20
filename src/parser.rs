use crate::instruction::*;
use crate::isa;
use crate::lexer::*;
use crate::program_state::IRegister;
use std::collections::HashMap;
use std::iter::Peekable;

#[derive(Eq, PartialEq, Debug)]
struct ParseErrorData {
    msg: String,
    contents: String,
}

#[derive(Eq, PartialEq, Debug)]
struct ParseError {
    location: Location,
    data: ParseErrorData,
}

impl ParseError {
    fn new(location: Location, msg: String, contents: String) -> Self {
        ParseError {
            location,
            data: ParseErrorData { msg, contents },
        }
    }

    fn bad_head(location: Location, contents: String) -> Self {
        ParseError::new(
            location,
            "Expected label, section, or instruction".to_string(),
            contents,
        )
    }

    fn bad_arg(location: Location, contents: String) -> Self {
        ParseError::new(
            location,
            "Expected register name or immediate".to_string(),
            contents,
        )
    }

    fn bad_reg(location: Location, reg_name: String) -> Self {
        ParseError::new(location, "Expected register name".to_string(), reg_name)
    }

    fn not_enough_args(location: Location, contents: String) -> Self {
        ParseError::new(location, "Not enough arguments".to_string(), contents)
    }

    fn unexpected(location: Location, contents: String) -> Self {
        ParseError::new(location, "Unexpected token".to_string(), contents)
    }
}

struct RiscVProgram {
    insts: Vec<ConcreteInst>,
    // TODO add symbol table, relocation data, etc.?
}

#[derive(Clone)]
enum ParseType {
    R(&'static dyn Fn(IRegister, IRegister, IRegister) -> ConcreteInst),
    Arith,
    Env,
    MemL,
    MemS,
    B,
    Jal,
    Jalr,
    U,
}

struct RiscVParser {
    inst_expansion_table: HashMap<String, ParseType>,
    reg_expansion_table: HashMap<String, IRegister>,
}

type TokenIter<'a> = Peekable<&'a mut dyn Iterator<Item = Token>>;

impl RiscVParser {
    pub fn new() -> Self {
        use isa::*;
        use ParseType::*;
        let inst_expansion_table = [
            ("add", R(&Add::new)),
            ("addi", Arith),
            ("and", R(&And::new)),
            ("andi", Arith),
            ("auipc", U),
            ("beq", B),
            ("bge", B),
            ("bgeu", B),
            ("blt", B),
            ("bltu", B),
            ("bne", B),
            ("ebreak", Env),
            ("ecall", Env),
            ("jal", ParseType::Jal),
            ("jalr", ParseType::Jalr),
            ("lb", MemL),
            ("lbu", MemL),
            ("lh", MemL),
            ("lhu", MemL),
            ("lui", U),
            ("lw", MemL),
            // ("or", R),
            ("ori", Arith),
            ("sb", MemS),
            ("sh", MemS),
            // ("sll", R),
            ("slli", Arith),
            // ("slt", R),
            ("slti", Arith),
            ("sltiu", Arith),
            // ("sltu", R),
            // ("sra", R),
            ("srai", Arith),
            // ("srl", R),
            ("srli", Arith),
            // ("sub", R),
            ("sw", MemS),
            // ("xor", R),
            ("xori", Arith),
        ]
        .iter()
        .cloned()
        .map(|(s, t)| (s.to_owned(), t))
        .collect();
        let mut reg_expansion_table: HashMap<String, IRegister> = IRegister::REG_ARRAY
            .iter()
            .map(|r| (r.to_string(), *r))
            .into_iter()
            .collect();
        for i in 0..32 {
            reg_expansion_table.insert(format!("x{}", i), IRegister::from(i));
        }
        // don't forget FP
        reg_expansion_table.insert("fp".to_string(), IRegister::FP);
        RiscVParser {
            inst_expansion_table,
            reg_expansion_table,
        }
    }

    /// Attempts to consume exactly N arguments from the iterator, possibly comma-separated.
    /// The first and last tokens cannot be commas. If repeated commas appear anywhere,
    /// an error is returned.
    /// The only tokens that may appear during this consumption are commas, names, and immediates.
    fn consume_commasep_args(
        &self,
        iter: &mut TokenIter,
        head_loc: Location,
        head_name: String,
        n: usize,
    ) -> Result<Vec<Token>, ParseError> {
        use TokenType::*;
        if n == 0 {
            if !iter.peek().is_none() {
                Err(ParseError::unexpected(
                    head_loc,
                    format!(
                        "{:?} (too many arguments for instruction {})",
                        iter.next().unwrap(),
                        head_name
                    ),
                ))
            } else {
                Ok(Vec::new())
            }
        } else {
            match iter.next() {
                Some(tok) => match tok.data {
                    Name(..) | Immediate(..) => {
                        // Allow single comma
                        if let Some(tok2) = iter.peek() {
                            if let Comma = tok2.data {
                                iter.next();
                            }
                        }
                        self.consume_commasep_args(iter, head_loc, head_name, n - 1)
                            .and_then(|mut args| {
                                args.insert(0, tok);
                                Ok(args)
                            })
                    }
                    _ => Err(ParseError::bad_arg(tok.location, format!("{:?}", tok.data))),
                },
                None => Err(ParseError::not_enough_args(
                    head_loc,
                    format!("for instruction {}", head_name),
                )),
            }
        }
    }

    fn try_parse_reg(&self, token: &Token) -> Result<IRegister, ParseError> {
        match &token.data {
            TokenType::Name(name) => self
                .reg_expansion_table
                .get(name)
                .and_then(|r| Some(*r))
                .ok_or(ParseError::bad_reg(token.location, name.to_string())),
            _ => Err(ParseError::unexpected(
                token.location,
                format!("Expected register name, found {:?}", token.data),
            )),
        }
    }

    fn try_expand_inst(
        &self,
        head_loc: Location,
        name: String,
        iter: &mut TokenIter,
    ) -> Result<Vec<ConcreteInst>, ParseError> {
        if let Some(parse_type) = self.inst_expansion_table.get(name.as_str()) {
            match parse_type {
                ParseType::R(inst_new) => {
                    // R-types are always "inst rd, rs1, rs2" with one or no commas in between
                    let args = self.consume_commasep_args(iter, head_loc, name, 3);
                    args.and_then(|lst| {
                        debug_assert!(lst.len() == 3);
                        let maybe_rd = self.try_parse_reg(&lst[0]);
                        let maybe_rs1 = self.try_parse_reg(&lst[1]);
                        let maybe_rs2 = self.try_parse_reg(&lst[2]);
                        maybe_rd.and_then(|rd| {
                            maybe_rs1.and_then(|rs1| {
                                maybe_rs2.and_then(|rs2| Ok(vec![inst_new(rd, rs1, rs2)]))
                            })
                        })
                    })
                }
                // Arith => ,
                // // Env => ,
                // Mem => ,
                // B => ,
                // Jal => ,
                // Jalr => ,
                // U => ,
                _ => Err(ParseError::unexpected(
                    head_loc,
                    "unimplemented".to_string(),
                )),
            }
        } else {
            Err(ParseError::new(
                head_loc,
                "No instruction found with name".to_string(),
                name,
            ))
        }
    }

    fn parse_line(&self, toks: TokenStream) -> Result<Vec<ConcreteInst>, ParseError> {
        // needed for lifetime reasons i guess
        let orig_iter = &mut toks.into_iter() as &mut dyn Iterator<Item = Token>;
        let mut iter = orig_iter.peekable();
        return if let Some(head_tok) = iter.next() {
            use TokenType::*;
            match head_tok.data {
                Name(name) => self.try_expand_inst(head_tok.location, name, &mut iter),
                LabelDef(_label_name) => Ok(Vec::new()), // TODO
                SectionDef(_section_name) => Ok(Vec::new()), // TODO
                Comment(..) => Ok(Vec::new()),           // deliberate no-op
                Comma => Err(ParseError::bad_head(head_tok.location, ",".to_string())),
                Immediate(n, style) => {
                    Err(ParseError::bad_head(head_tok.location, style.format(n)))
                }
                LParen => Err(ParseError::bad_head(head_tok.location, "(".to_string())),
                RParen => Err(ParseError::bad_head(head_tok.location, ")".to_string())),
            }
        } else {
            Ok(Vec::new())
        };
    }

    pub fn parse_program(&self, lines: LineTokenStream) -> Result<RiscVProgram, Vec<ParseError>> {
        let mut insts = Vec::<ConcreteInst>::new();
        let mut errs = Vec::<ParseError>::new();
        for line in lines {
            match self.parse_line(line) {
                Ok(new_insts) => insts.extend(new_insts),
                Err(new_err) => errs.push(new_err),
            }
        }
        if errs.is_empty() {
            Ok(RiscVProgram { insts })
        } else {
            Err(errs)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::isa::*;
    use crate::lexer;
    use crate::program_state::IRegister::*;

    #[test]
    fn test_r_type_parse() {
        let parser = RiscVParser::new();
        let (toks, lex_err) = lexer::lex_string("add x5, sp, fp".to_string());
        assert!(lex_err.is_empty());
        let result = parser.parse_program(toks).expect("Error while parsing");
        assert_eq!(result.insts.len(), 1);
        assert_eq!(result.insts[0], Add::new(IRegister::from(5), SP, FP));
    }
}
