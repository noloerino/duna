use crate::instruction::*;
use crate::isa;
use crate::lexer::*;
use crate::program_state::DataWord;
use crate::program_state::IRegister;
use crate::pseudo_inst::*;
use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Eq, PartialEq, Debug)]
struct ParseErrorData {
    msg: String,
    contents: String,
}

#[derive(Eq, PartialEq, Debug)]
pub struct ParseError {
    location: Location,
    data: ParseErrorData,
}

impl ParseError {
    pub fn new(location: Location, msg: String, contents: String) -> Self {
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

    fn not_enough_args(location: Location, inst_name: String) -> Self {
        ParseError::new(
            location,
            "Not enough arguments".to_string(),
            format!("while parsing instruction {}", inst_name),
        )
    }

    fn imm_too_big(location: Location, imm_str: String) -> Self {
        ParseError::new(location, "Immediate too large".to_string(), imm_str)
    }

    fn unexpected(location: Location, contents: String) -> Self {
        ParseError::new(location, "Unexpected token".to_string(), contents)
    }

    fn unclosed_paren(location: Location, contents: String) -> Self {
        ParseError::new(
            location,
            "Expected closing parentheses".to_string(),
            contents,
        )
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ParseError at {}: {}\t{}",
            self.location, self.data.msg, self.data.contents
        )
    }
}

#[derive(Clone)]
enum ParseType {
    R(&'static dyn Fn(IRegister, IRegister, IRegister) -> ConcreteInst),
    Arith(&'static dyn Fn(IRegister, IRegister, DataWord) -> ConcreteInst),
    Env,
    MemL(&'static dyn Fn(IRegister, IRegister, DataWord) -> ConcreteInst),
    MemS(&'static dyn Fn(IRegister, IRegister, DataWord) -> ConcreteInst),
    B,
    Jal,
    Jalr,
    U(&'static dyn Fn(IRegister, DataWord) -> ConcreteInst),
}

#[derive(Clone)]
enum PseudoParseType {
    RegImm(&'static dyn Fn(IRegister, DataWord) -> Vec<ConcreteInst>),
    RegReg(&'static dyn Fn(IRegister, IRegister) -> Vec<ConcreteInst>),
    NoArgs(&'static dyn Fn() -> Vec<ConcreteInst>),
}

struct ParserData {
    inst_expansion_table: HashMap<String, ParseType>,
    pseudo_expansion_table: HashMap<String, PseudoParseType>,
    reg_expansion_table: HashMap<String, IRegister>,
}

pub struct RiscVParser {
    parser_data: ParserData,
    lines: LineTokenStream,
}

type TokenIter<'a> = Peekable<IntoIter<Token>>;

impl RiscVParser {
    pub fn from_tokens(lines: LineTokenStream) -> Self {
        use isa::*;
        use ParseType::*;
        use PseudoParseType::*;
        let inst_expansion_table = [
            ("add", R(&Add::new)),
            ("addi", Arith(&Addi::new)),
            ("and", R(&And::new)),
            ("andi", Arith(&Andi::new)),
            ("auipc", U(&Auipc::new)),
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
            ("lb", MemL(&Lb::new)),
            ("lbu", MemL(&Lbu::new)),
            ("lh", MemL(&Lh::new)),
            ("lhu", MemL(&Lhu::new)),
            ("lui", U(&Lui::new)),
            ("lw", MemL(&Lw::new)),
            // ("or", R),
            // ("ori", Arith),
            ("sb", MemS(&Sb::new)),
            ("sh", MemS(&Sh::new)),
            // ("sll", R),
            // ("slli", Arith),
            // ("slt", R),
            // ("slti", Arith),
            // ("sltiu", Arith),
            // ("sltu", R),
            // ("sra", R),
            // ("srai", Arith),
            // ("srl", R),
            // ("srli", Arith),
            // ("sub", R),
            ("sw", MemS(&Sw::new)),
            // ("xor", R),
            // ("xori", Arith),
        ]
        .iter()
        .cloned()
        .map(|(s, t)| (s.to_string(), t))
        .collect();
        let pseudo_expansion_table = [
            ("li", RegImm(&Li::expand)),
            ("mv", RegReg(&Mv::expand)),
            ("nop", NoArgs(&Nop::expand)),
        ]
        .iter()
        .cloned()
        .map(|(s, t)| (s.to_string(), t))
        .collect();
        let mut reg_expansion_table: HashMap<String, IRegister> = IRegister::REG_ARRAY
            .iter()
            .map(|r| (r.to_string(), *r))
            .collect();
        for i in 0..32 {
            reg_expansion_table.insert(format!("x{}", i), IRegister::from(i));
        }
        // don't forget FP
        reg_expansion_table.insert("fp".to_string(), IRegister::FP);
        RiscVParser {
            parser_data: ParserData {
                inst_expansion_table,
                pseudo_expansion_table,
                reg_expansion_table,
            },
            lines,
        }
    }

    pub fn parse(self) -> (Vec<ConcreteInst>, Vec<ParseError>) {
        let mut insts = Vec::<ConcreteInst>::new();
        let mut errs = Vec::<ParseError>::new();
        for line in self.lines {
            match LineParser::new(&self.parser_data, line).parse() {
                Ok(new_insts) => insts.extend(new_insts),
                Err(new_err) => errs.push(new_err),
            }
        }
        (insts, errs)
    }
}

/// Returns the first error found in lst, or none if there are no such errors.
fn find_first_err<T, E>(lst: Vec<Result<T, E>>) -> Option<E> {
    lst.into_iter().find_map(|r| match r {
        Err(e) => Some(e),
        _ => None,
    })
}

/// Contains arguments for a memory operation (load or store).
/// The registers correspond to the order in which they appear: for stores, RS2 precedes RS1;
/// for loads, RD preceds RS1.
struct MemArgs {
    first_reg: IRegister,
    second_reg: IRegister,
    imm: DataWord,
}

struct LineParser<'a> {
    data: &'a ParserData,
    iter: TokenIter<'a>,
}

impl LineParser<'_> {
    /// Attempts to consume exactly N arguments from the iterator, possibly comma-separated.
    /// The first and last tokens cannot be commas. If repeated commas appear anywhere,
    /// an error is returned.
    /// The only tokens that may appear during this consumption are commas, names, and immediates.
    fn consume_commasep_args(
        &mut self,
        head_loc: Location,
        head_name: String,
        n: usize,
    ) -> Result<Vec<Token>, ParseError> {
        use TokenType::*;
        if n == 0 {
            if self.iter.peek().is_some() {
                Err(ParseError::unexpected(
                    head_loc,
                    format!(
                        "{:?} (too many arguments for instruction {})",
                        self.iter.next().unwrap(),
                        head_name
                    ),
                ))
            } else {
                Ok(Vec::new())
            }
        } else {
            match self.iter.next() {
                Some(tok) => match tok.data {
                    Name(..) | Immediate(..) => {
                        // Allow single comma, excpet when trailing
                        if n > 1 {
                            if let Some(tok2) = self.iter.peek() {
                                if let Comma = tok2.data {
                                    self.iter.next();
                                }
                            }
                        }
                        self.consume_commasep_args(head_loc, head_name, n - 1)
                            .and_then(|mut args| {
                                args.insert(0, tok);
                                Ok(args)
                            })
                    }
                    _ => Err(ParseError::bad_arg(tok.location, format!("{:?}", tok.data))),
                },
                None => Err(ParseError::not_enough_args(head_loc, head_name)),
            }
        }
    }

    /// Consumes tokens for arguments for a memory operation.
    /// These are either of the form "inst reg, imm, reg)" e.g. "lw x1 -4 x2"
    /// or "inst reg, (imm)reg" e.g "lw x1, 4(x2)" (commas optional in both cases)
    fn consume_mem_args(
        &mut self,
        head_loc: Location,
        head_name: String,
    ) -> Result<MemArgs, ParseError> {
        // first consumed token must be register name
        let first_tok = self.try_next_tok(head_loc, &head_name)?;
        let first_reg = self.try_parse_reg(&first_tok)?;
        // check for comma
        let maybe_comma = self.try_peek_tok(head_loc, &head_name)?;
        if let TokenType::Comma = maybe_comma.data {
            self.iter.next();
        }
        // must be immediate here
        let imm_tok = self.try_next_tok(head_loc, &head_name)?;
        let imm = self.try_parse_imm(12, &imm_tok)?;
        // check for lparen
        let maybe_lparen = self.try_peek_tok(head_loc, &head_name)?;
        let is_lparen = if let TokenType::LParen = maybe_lparen.data {
            self.iter.next();
            true
        } else {
            false
        };
        // must be a register here
        let reg2_tok = self.try_next_tok(head_loc, &head_name)?;
        let second_reg = self.try_parse_reg(&reg2_tok)?;
        if is_lparen {
            let maybe_rparen = self.try_next_tok(head_loc, &head_name)?;
            if let TokenType::RParen = maybe_rparen.data {
            } else {
                return Err(ParseError::unclosed_paren(
                    maybe_rparen.location,
                    format!("{:?}", maybe_rparen.data),
                ));
            }
        }
        Ok(MemArgs {
            first_reg,
            second_reg,
            imm,
        })
    }

    /// Attempts to advance the next token of the iterator, returning a ParseError if there are none.
    fn try_next_tok(&mut self, head_loc: Location, inst_name: &str) -> Result<Token, ParseError> {
        if let Some(tok) = self.iter.next() {
            Ok(tok)
        } else {
            Err(ParseError::not_enough_args(head_loc, inst_name.to_string()))
        }
    }

    /// Attempts to peek the next token of the iterator, returning a ParseError if there are none.
    fn try_peek_tok(&mut self, head_loc: Location, inst_name: &str) -> Result<&Token, ParseError> {
        if let Some(tok) = self.iter.peek() {
            Ok(tok)
        } else {
            Err(ParseError::not_enough_args(head_loc, inst_name.to_string()))
        }
    }

    fn try_parse_reg(&self, token: &Token) -> Result<IRegister, ParseError> {
        match &token.data {
            TokenType::Name(name) => self
                .data
                .reg_expansion_table
                .get(name)
                .cloned()
                .ok_or_else(|| ParseError::bad_reg(token.location, name.to_string())),
            _ => Err(ParseError::unexpected(
                token.location,
                format!("Expected register name, found {:?}", token.data),
            )),
        }
    }

    /// Parses an immediate that is required to be at most n bits.
    /// If the provided immediate is a negative, then the upper (32 - n + 1) bits must all be 1.
    fn try_parse_imm(&self, n: u8, token: &Token) -> Result<DataWord, ParseError> {
        match &token.data {
            // Check lower 12 bits
            // We give a pass to negative numbers with high bits set
            TokenType::Immediate(val, radix) => {
                let mask = (-1i32)
                    << (if *val < 0 {
                        // Allow the sign bit to be part of the mask
                        n - 1
                    } else {
                        n
                    });
                let mask_result = *val & mask;
                if mask_result != 0 && mask_result != mask {
                    Err(ParseError::imm_too_big(token.location, radix.format(*val)))
                } else {
                    Ok(DataWord::from(*val))
                }
            }
            _ => Err(ParseError::unexpected(
                token.location,
                format!("Expected immediate, found {:?}", token.data),
            )),
        }
    }

    fn try_expand_inst(
        &mut self,
        head_loc: Location,
        name: String,
    ) -> Result<Vec<ConcreteInst>, ParseError> {
        use ParseType::*;
        use PseudoParseType::*;
        if let Some(parse_type) = self.data.inst_expansion_table.get(name.as_str()) {
            Ok(vec![match parse_type {
                R(inst_new) => {
                    // R-types are always "inst rd, rs1, rs2" with one or no commas in between
                    let args = self.consume_commasep_args(head_loc, name, 3)?;
                    debug_assert!(args.len() == 3);
                    let regs: Vec<Result<IRegister, ParseError>> =
                        args.iter().map(|arg| self.try_parse_reg(arg)).collect();
                    match regs.as_slice() {
                        [Ok(rd), Ok(rs1), Ok(rs2)] => Ok(inst_new(*rd, *rs1, *rs2)),
                        _ => Err(find_first_err(regs).unwrap()),
                    }
                }
                Arith(inst_new) => {
                    let args = self.consume_commasep_args(head_loc, name, 3)?;
                    debug_assert!(args.len() == 3);
                    let rd = self.try_parse_reg(&args[0])?;
                    let rs1 = self.try_parse_reg(&args[1])?;
                    let imm = self.try_parse_imm(12, &args[2])?;
                    Ok(inst_new(rd, rs1, imm))
                }
                // // Env => ,
                MemL(inst_new) => {
                    let args = self.consume_mem_args(head_loc, name)?;
                    let rd = args.first_reg;
                    let rs1 = args.second_reg;
                    let imm = args.imm;
                    Ok(inst_new(rd, rs1, imm))
                }
                MemS(inst_new) => {
                    let args = self.consume_mem_args(head_loc, name)?;
                    let rs2 = args.first_reg;
                    let rs1 = args.second_reg;
                    let imm = args.imm;
                    Ok(inst_new(rs1, rs2, imm))
                }
                // B => ,
                // Jal => ,
                // Jalr => ,
                U(inst_new) => {
                    let args = self.consume_commasep_args(head_loc, name, 2)?;
                    let rd = self.try_parse_reg(&args[0])?;
                    let imm = self.try_parse_imm(20, &args[1])?;
                    Ok(inst_new(rd, imm))
                }
                _ => Err(ParseError::unexpected(
                    head_loc,
                    "unimplemented".to_string(),
                )),
            }?])
        } else if let Some(parse_type) = self.data.pseudo_expansion_table.get(name.as_str()) {
            match parse_type {
                RegImm(inst_expand) => {
                    let args = self.consume_commasep_args(head_loc, name, 2)?;
                    debug_assert!(args.len() == 2);
                    let rd = self.try_parse_reg(&args[0])?;
                    let imm = self.try_parse_imm(32, &args[1])?;
                    Ok(inst_expand(rd, imm))
                }
                NoArgs(inst_expand) => {
                    let args = self.consume_commasep_args(head_loc, name, 0)?;
                    debug_assert!(args.is_empty());
                    Ok(inst_expand())
                }
                RegReg(inst_expand) => {
                    let args = self.consume_commasep_args(head_loc, name, 2)?;
                    debug_assert!(args.len() == 2);
                    let rd = self.try_parse_reg(&args[0])?;
                    let rs = self.try_parse_reg(&args[1])?;
                    Ok(inst_expand(rd, rs))
                }

                // _ => Err(ParseError::unexpected(
                //     head_loc,
                //     "unimplemented".to_string(),
                // )),
            }
        } else {
            Err(ParseError::new(
                head_loc,
                "No instruction found with name".to_string(),
                name,
            ))
        }
    }

    fn new(data: &ParserData, tokens: TokenStream) -> LineParser {
        LineParser {
            data,
            iter: tokens.into_iter().peekable(),
        }
    }

    fn parse(mut self) -> Result<Vec<ConcreteInst>, ParseError> {
        // needed for lifetime reasons i guess
        if let Some(head_tok) = self.iter.next() {
            use TokenType::*;
            match head_tok.data {
                Name(name) => self.try_expand_inst(head_tok.location, name),
                LabelDef(_label_name) => Ok(Vec::new()), // TODO
                Directive(_section_name) => Ok(Vec::new()), // TODO
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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::isa::*;
    use crate::lexer;
    use crate::program_state::IRegister::*;

    /// Lexes a program. Asserts that the lex has no errors.
    fn lex(prog: &str) -> Vec<TokenStream> {
        let (toks, errs) = lexer::Lexer::from_string(prog.to_string()).lex();
        assert!(errs.is_empty());
        toks
    }

    /// Parses and lexes the provided string, assuming that there are no errors in either phase.
    /// Assumes that there were no lex errors.
    fn parse_and_lex(prog: &str) -> Vec<ConcreteInst> {
        let (insts, errs) = RiscVParser::from_tokens(lex(prog)).parse();
        assert!(errs.is_empty());
        insts
    }

    #[test]
    fn test_bad_commas() {
        let bad_insts = vec![
            "add x5, sp, fp,",
            "add ,x1, x2, x3",
            ",add x1 x2 x3",
            "add x1,,x2, x3",
        ];
        for inst in bad_insts {
            let (_, parse_err) = RiscVParser::from_tokens(lex(inst)).parse();
            assert!(!parse_err.is_empty());
        }
    }

    #[test]
    fn test_r_type_parse() {
        let insts = parse_and_lex("add x5, sp, fp");
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0], Add::new(IRegister::from(5), SP, FP));
    }

    #[test]
    fn test_i_arith_parse() {
        // lack of commas is deliberate
        let insts = parse_and_lex("addi sp sp -4");
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0], Addi::new(SP, SP, DataWord::from(-4)));
    }

    #[test]
    fn test_lui_parse() {
        let insts = parse_and_lex("lui a0, 0xD_EADC");
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0], Lui::new(A0, DataWord::from(0xD_EADC)));
    }

    #[test]
    fn test_imm_too_big() {
        // immediates for instructions like addi can only be 12 bits long
        let parser = RiscVParser::from_tokens(lex("addi sp sp 0xF000"));
        let (insts, parse_err) = parser.parse();
        assert!(!parse_err.is_empty());
        assert!(insts.is_empty());
    }

    #[test]
    fn test_pseudo_li() {
        let insts = parse_and_lex("li a0, 0xDEAD_BEEF");
        assert_eq!(insts, Li::expand(A0, DataWord::from(0xDEAD_BEEFu32)));
    }
}
