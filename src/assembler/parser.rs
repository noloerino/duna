use super::assembler::{Label, PartialInst};
use super::lexer::*;
use crate::instruction::*;
use crate::isa;
use crate::program_state::{DataWord, IRegister};
use crate::pseudo_inst::*;
use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use std::vec::IntoIter;

type ParsedInstStream = Vec<PartialInst>;
type ParseResult = Result<ParsedInstStream, ParseError>;

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
    pub fn new(location: Location, msg: &str, contents: &str) -> Self {
        ParseError {
            location,
            data: ParseErrorData {
                msg: msg.to_string(),
                contents: contents.to_string(),
            },
        }
    }

    fn bad_head(location: Location, contents: &str) -> Self {
        ParseError::new(
            location,
            "Expected label, section, or instruction",
            contents,
        )
    }

    fn bad_arg(location: Location, contents: &str) -> Self {
        ParseError::new(location, "Expected register name or immediate", contents)
    }

    fn bad_reg(location: Location, reg_name: &str) -> Self {
        ParseError::new(location, "Expected register name", reg_name)
    }

    fn not_enough_args(location: Location, inst_name: &str) -> Self {
        ParseError::new(
            location,
            "Not enough arguments",
            &format!("while parsing instruction {}", inst_name),
        )
    }

    fn imm_too_big(location: Location, imm_str: &str) -> Self {
        ParseError::new(location, "Immediate too large", imm_str)
    }

    fn unexpected(location: Location, contents: &str) -> Self {
        ParseError::new(location, "Unexpected token", contents)
    }

    fn unclosed_paren(location: Location, contents: &str) -> Self {
        ParseError::new(location, "Expected closing parentheses", contents)
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
/// Describes the arguments needed for a type of function.
/// Due to their unique parsing rules, Jal, Jalr, and Li are hardcoded.
enum ParseType {
    // Base ISA
    R(fn(IRegister, IRegister, IRegister) -> ConcreteInst),
    Arith(fn(IRegister, IRegister, DataWord) -> ConcreteInst),
    Env(fn() -> ConcreteInst),
    MemL(fn(IRegister, IRegister, DataWord) -> ConcreteInst),
    MemS(fn(IRegister, IRegister, DataWord) -> ConcreteInst),
    B(fn(IRegister, IRegister, DataWord) -> ConcreteInst),
    // Covers "jal ra, label", "jal label", "jal -4" etc.
    Jal,
    // Covers "jalr ra, 0(x1)", "jalr x1", etc.
    Jalr,
    U(fn(IRegister, DataWord) -> ConcreteInst),
    // Pseudo-instructions
    Li,
    RegReg(fn(IRegister, IRegister) -> ConcreteInst),
    NoArgs(fn() -> ConcreteInst),
    OneReg(fn(IRegister) -> ConcreteInst),
    // Covers "j label", "j -4", etc.
    LikeJ(fn(DataWord) -> ConcreteInst),
}

struct ParserData {
    inst_expansion_table: &'static HashMap<String, ParseType>,
    reg_expansion_table: &'static HashMap<String, IRegister>,
}

pub struct RiscVParser {
    parser_data: ParserData,
    lines: LineTokenStream,
}

type TokenIter<'a> = Peekable<IntoIter<Token>>;

lazy_static! {
    static ref INST_EXPANSION_TABLE: HashMap<String, ParseType> = {
        use isa::*;

        use ParseType::*;
        [
            ("add", R(Add::new)),
            ("addi", Arith(Addi::new)),
            ("and", R(And::new)),
            ("andi", Arith(Andi::new)),
            ("auipc", U(Auipc::new)),
            ("beq", B(Beq::new)),
            ("bge", B(Bge::new)),
            ("bgeu", B(Bgeu::new)),
            ("blt", B(Blt::new)),
            ("bltu", B(Bltu::new)),
            ("bne", B(Bne::new)),
            // ("ebreak", Env),
            ("ecall", Env(Ecall::new)),
            ("jal", ParseType::Jal),
            ("jalr", ParseType::Jalr),
            ("lb", MemL(Lb::new)),
            ("lbu", MemL(Lbu::new)),
            ("lh", MemL(Lh::new)),
            ("lhu", MemL(Lhu::new)),
            ("lui", U(Lui::new)),
            ("lw", MemL(Lw::new)),
            // ("or", R),
            // ("ori", Arith),
            ("sb", MemS(Sb::new)),
            ("sh", MemS(Sh::new)),
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
            ("sw", MemS(Sw::new)),
            // ("xor", R),
            // ("xori", Arith),
            ("li", ParseType::Li),
            ("mv", RegReg(Mv::expand)),
            ("nop", NoArgs(Nop::expand)),
            ("j", LikeJ(J::expand)),
            ("jr", OneReg(Jr::expand)),
            ("ret", NoArgs(Ret::expand)),
        ]
        .iter()
        .cloned()
        .map(|(s, t)| (s.to_string(), t))
        .collect()
    };
    static ref REG_EXPANSION_TABLE: HashMap<String, IRegister> = {
        let mut reg_expansion_table: HashMap<String, IRegister> = IRegister::REG_ARRAY
            .iter()
            .map(|r| (r.to_string(), *r))
            .collect();
        for i in 0..32 {
            reg_expansion_table.insert(format!("x{}", i), IRegister::from(i));
        }
        // don't forget FP
        reg_expansion_table.insert("fp".to_string(), IRegister::FP);
        reg_expansion_table
    };
}

impl RiscVParser {
    pub fn from_tokens(lines: LineTokenStream) -> Self {
        RiscVParser {
            parser_data: ParserData {
                inst_expansion_table: &INST_EXPANSION_TABLE,
                reg_expansion_table: &REG_EXPANSION_TABLE,
            },
            lines,
        }
    }

    pub fn parse(self) -> (ParsedInstStream, Vec<ParseError>) {
        let mut insts = Vec::<PartialInst>::new();
        let mut errs = Vec::<ParseError>::new();
        let mut last_label: Option<Label> = None;
        for line in self.lines {
            let (found_label, parse_result) =
                LineParser::new(&self.parser_data, line, &last_label).parse();
            match parse_result {
                Ok(mut new_insts) => {
                    // if insts is not empty, then that means the label was already used
                    last_label = if new_insts.is_empty() {
                        found_label
                    } else {
                        // stick label onto first inst
                        if let Some(new_label) = found_label {
                            let head_inst = new_insts.remove(0).with_label(new_label);
                            new_insts.insert(0, head_inst);
                        }
                        None
                    };
                    insts.extend(new_insts);
                }
                Err(new_err) => errs.push(new_err),
            }
        }
        (insts, errs)
    }
}

/// Contains arguments for a memory operation (load or store).
/// The registers correspond to the order in which they appear: for stores, RS2 precedes RS1;
/// for loads, RD preceds RS1.
struct MemArgs {
    first_reg: IRegister,
    second_reg: IRegister,
    imm: DataWord,
}

enum ImmOrLabel {
    Imm(DataWord),
    Label(Label),
}

/// Convenience method to stuff a PartialInst into a Vec<PartialInst>
fn ok_vec(inst: PartialInst) -> ParseResult {
    Ok(vec![inst])
}

/// Convenience method to stuff a ConcreteInst into Ok(vec![PartialInst(...)])
fn ok_wrap_concr(inst: ConcreteInst) -> ParseResult {
    ok_vec(PartialInst::new_complete(inst))
}

/// Convenience method to turn a Vec<ConcreteInst> into Ok(Vec<PartialInst>)
fn ok_wrap_expanded(inst: Vec<ConcreteInst>) -> ParseResult {
    Ok(inst.into_iter().map(PartialInst::new_complete).collect())
}

struct LineParser<'a> {
    data: &'a ParserData,
    iter: TokenIter<'a>,
    label: Option<Label>,
}

impl LineParser<'_> {
    /// Checks this line's iterator to ensure that there are no more tokens remaining, save
    /// for a possible comment.
    /// This is used for situations where a fixed number of arguments is expected, as we're
    /// free to consume the iterator since more tokens would be an error regardless.
    fn check_no_more_args(
        &mut self,
        head_loc: Location,
        head_name: &str,
    ) -> Result<(), ParseError> {
        let next = self.iter.next();
        if let Some(tok) = next {
            if let TokenType::Comment(_) = tok.data {
                Ok(())
            } else {
                Err(ParseError::unexpected(
                    head_loc,
                    &format!(
                        "{:?} (too many arguments for instruction {})",
                        tok, head_name
                    ),
                ))
            }
        } else {
            Ok(())
        }
    }

    /// Attempts to consume exactly N arguments from the iterator, possibly comma-separated.
    /// The first and last tokens cannot be commas. If repeated commas appear anywhere,
    /// an error is returned.
    /// The only tokens that may appear during this consumption are commas, names, and immediates.
    fn consume_commasep_args(
        &mut self,
        head_loc: Location,
        head_name: &str,
        n: usize,
    ) -> Result<Vec<Token>, ParseError> {
        use TokenType::*;
        let result = if n == 0 {
            self.check_no_more_args(head_loc, head_name)
                .and(Ok(Vec::new()))
        } else {
            match self.iter.next() {
                Some(tok) => match tok.data {
                    Name(..) | Immediate(..) => {
                        // Allow single comma, except when trailing
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
                    _ => Err(ParseError::bad_arg(
                        tok.location,
                        &format!("{:?}", tok.data),
                    )),
                },
                None => Err(ParseError::not_enough_args(head_loc, head_name)),
            }
        };
        // jank to allow assertion and borrow checker to both be happy
        match result {
            Ok(args) => {
                debug_assert!(args.len() == n);
                Ok(args)
            }
            _ => result,
        }
    }

    /// Attempts to consume possibly comma-separate arguments from the iterator.
    /// The first and last tokens cannot be commas. If repeated commas appear anywhere,
    /// an error is returned.
    /// This consumes until a comment token or the end of the iterator is reached.
    fn consume_unbounded_commasep_args(&mut self) -> Result<Vec<Token>, ParseError> {
        use TokenType::*;
        let mut toks = Vec::new();
        // track if we just visited a comma
        // initialize to true to prevent leading commas
        let mut was_comma = true;
        for tok in &mut self.iter {
            match tok.data {
                Name(..) | Immediate(..) => {
                    was_comma = false;
                    toks.push(tok)
                }
                Comma => {
                    if was_comma {
                        return Err(ParseError::bad_arg(
                            tok.location,
                            &format!("{:?}", tok.data),
                        ));
                    }
                    was_comma = true;
                }
                Comment(..) => return Ok(toks),
                _ => {
                    return Err(ParseError::bad_arg(
                        tok.location,
                        &format!("{:?}", tok.data),
                    ));
                }
            }
        }
        Ok(toks)
    }

    /// Consumes tokens for arguments for a memory operation.
    /// These are either of the form "inst reg, imm, reg)" e.g. "lw x1 -4 x2"
    /// or "inst reg, (imm)reg" e.g "lw x1, 4(x2)" (commas optional in both cases)
    fn consume_mem_args(
        &mut self,
        head_loc: Location,
        head_name: &str,
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
                    &format!("{:?}", maybe_rparen.data),
                ));
            }
        }
        // Any trailing token must be a comment
        self.check_no_more_args(head_loc, head_name)
            .and(Ok(MemArgs {
                first_reg,
                second_reg,
                imm,
            }))
    }

    /// Attempts to advance the next token of the iterator, returning a ParseError if there are none.
    fn try_next_tok(&mut self, head_loc: Location, inst_name: &str) -> Result<Token, ParseError> {
        if let Some(tok) = self.iter.next() {
            Ok(tok)
        } else {
            Err(ParseError::not_enough_args(head_loc, inst_name))
        }
    }

    /// Attempts to peek the next token of the iterator, returning a ParseError if there are none.
    fn try_peek_tok(&mut self, head_loc: Location, inst_name: &str) -> Result<&Token, ParseError> {
        if let Some(tok) = self.iter.peek() {
            Ok(tok)
        } else {
            Err(ParseError::not_enough_args(head_loc, inst_name))
        }
    }

    fn try_parse_reg(&self, token: &Token) -> Result<IRegister, ParseError> {
        match &token.data {
            TokenType::Name(name) => self
                .data
                .reg_expansion_table
                .get(name)
                .cloned()
                .ok_or_else(|| ParseError::bad_reg(token.location, name)),
            _ => Err(ParseError::unexpected(
                token.location,
                &format!("Expected register name, found {:?}", token.data),
            )),
        }
    }

    /// Parses an immediate that is required to be at most n bits.
    /// If the provided immediate is a negative, then the upper (32 - n + 1) bits must all be 1.
    fn try_parse_imm(&self, n: u8, token: &Token) -> Result<DataWord, ParseError> {
        match &token.data {
            // Check lower n bits
            // We give a pass to negative numbers with high bits set
            TokenType::Immediate(val, radix) => {
                let mask = if n == 32 {
                    // Prevent shift overflow
                    0
                } else {
                    (-1i32)
                        << (if *val < 0 {
                            // Allow the sign bit to be part of the mask
                            n - 1
                        } else {
                            n
                        })
                };
                let mask_result = *val & mask;
                if mask_result != 0 && mask_result != mask {
                    Err(ParseError::imm_too_big(token.location, &radix.format(*val)))
                } else {
                    Ok(DataWord::from(*val))
                }
            }
            _ => Err(ParseError::unexpected(
                token.location,
                &format!("Expected immediate, found {:?}", token.data),
            )),
        }
    }

    /// Attempts to expand a token into a label reference or an immediate of at most max_imm_len.
    fn try_parse_imm_or_label_ref(
        &self,
        max_imm_len: u8,
        token: &Token,
    ) -> Result<ImmOrLabel, ParseError> {
        if let TokenType::Name(name) = &token.data {
            // label case
            Ok(ImmOrLabel::Label(name.clone()))
        } else {
            // imm case
            Ok(ImmOrLabel::Imm(self.try_parse_imm(max_imm_len, token)?))
        }
    }

    /// Expands an instruction that is known to be in the expansion table.
    fn try_expand_found_inst(
        &mut self,
        head_loc: Location,
        name: &str,
        parse_type: &ParseType,
    ) -> Result<ParsedInstStream, ParseError> {
        use ParseType::*;
        match parse_type {
            R(inst_new) => {
                // R-types are always "inst rd, rs1, rs2" with one or no commas in between
                let args = self.consume_commasep_args(head_loc, name, 3)?;
                let rd = self.try_parse_reg(&args[0])?;
                let rs1 = self.try_parse_reg(&args[1])?;
                let rs2 = self.try_parse_reg(&args[2])?;
                ok_wrap_concr(inst_new(rd, rs1, rs2))
            }
            Arith(inst_new) => {
                let args = self.consume_commasep_args(head_loc, name, 3)?;
                let rd = self.try_parse_reg(&args[0])?;
                let rs1 = self.try_parse_reg(&args[1])?;
                let imm = self.try_parse_imm(12, &args[2])?;
                ok_wrap_concr(inst_new(rd, rs1, imm))
            }
            Env(inst_new) => {
                let _args = self.consume_commasep_args(head_loc, name, 0)?;
                ok_wrap_concr(inst_new())
            }
            MemL(inst_new) => {
                let args = self.consume_mem_args(head_loc, name)?;
                let rd = args.first_reg;
                let rs1 = args.second_reg;
                let imm = args.imm;
                ok_wrap_concr(inst_new(rd, rs1, imm))
            }
            MemS(inst_new) => {
                let args = self.consume_mem_args(head_loc, name)?;
                let rs2 = args.first_reg;
                let rs1 = args.second_reg;
                let imm = args.imm;
                ok_wrap_concr(inst_new(rs1, rs2, imm))
            }
            B(inst_new) => {
                let args = self.consume_commasep_args(head_loc, name, 3)?;
                let rs1 = self.try_parse_reg(&args[0])?;
                let rs2 = self.try_parse_reg(&args[1])?;
                // Becuse branches actually chop off the LSB, we can take up to 13b
                let last_arg = self.try_parse_imm_or_label_ref(13, &args[2])?;
                match last_arg {
                    ImmOrLabel::Imm(imm) => {
                        if u32::from(imm) & 1 > 0 {
                            Err(ParseError::new(
                                head_loc,
                                "Branch immediates must be multiples of two",
                                &imm.to_string(),
                            ))
                        } else {
                            // LSB chopping is handled by instruction
                            ok_wrap_concr(inst_new(rs1, rs2, imm))
                        }
                    }
                    ImmOrLabel::Label(tgt_label) => ok_vec(PartialInst::new_two_reg_needs_label(
                        *inst_new, rs1, rs2, tgt_label,
                    )),
                }
            }
            Jal => {
                let args = self.consume_unbounded_commasep_args()?;
                let argc = args.len();
                match argc {
                    1 => {
                        // "jal label"
                        let last_arg = self.try_parse_imm_or_label_ref(20, &args[0])?;
                        match last_arg {
                            ImmOrLabel::Imm(imm) => ok_wrap_concr(JalPseudo::expand(imm)),
                            ImmOrLabel::Label(tgt_label) => ok_vec(
                                PartialInst::new_no_reg_needs_label(JalPseudo::expand, tgt_label),
                            ),
                        }
                    }
                    2 => {
                        // "jal ra label"
                        let rd = self.try_parse_reg(&args[0])?;
                        // J-type has 20-bit immediate
                        let last_arg = self.try_parse_imm_or_label_ref(20, &args[1])?;
                        match last_arg {
                            ImmOrLabel::Imm(imm) => ok_wrap_concr(isa::Jal::new(rd, imm)),
                            ImmOrLabel::Label(tgt_label) => ok_vec(
                                PartialInst::new_one_reg_needs_label(isa::Jal::new, rd, tgt_label),
                            ),
                        }
                    }
                    _ => Err(ParseError::unexpected(
                        head_loc,
                        &format!("wrong number of arguments for instruction {}", name),
                    )),
                }
            }
            Jalr => {
                let args = self.consume_unbounded_commasep_args()?;
                let argc = args.len();
                match argc {
                    1 => {
                        // "jalr rs"
                        let rs = self.try_parse_reg(&args[0])?;
                        ok_wrap_concr(JalrPseudo::expand(rs))
                    }
                    3 => {
                        // "jalr rd, rs, imm"
                        let rd = self.try_parse_reg(&args[0])?;
                        let rs1 = self.try_parse_reg(&args[1])?;
                        let imm = self.try_parse_imm(12, &args[2])?;
                        ok_wrap_concr(isa::Jalr::new(rd, rs1, imm))
                    }
                    _ => Err(ParseError::unexpected(
                        head_loc,
                        &format!("wrong number of arguments for instruction {}", name),
                    )),
                }
            }
            U(inst_new) => {
                let args = self.consume_commasep_args(head_loc, name, 2)?;
                let rd = self.try_parse_reg(&args[0])?;
                let imm = self.try_parse_imm(20, &args[1])?;
                ok_wrap_concr(inst_new(rd, imm))
            }
            Li => {
                let args = self.consume_commasep_args(head_loc, name, 2)?;
                let rd = self.try_parse_reg(&args[0])?;
                let imm = self.try_parse_imm(32, &args[1])?;
                ok_wrap_expanded(crate::pseudo_inst::Li::expand(rd, imm))
            }
            NoArgs(inst_expand) => {
                let _args = self.consume_commasep_args(head_loc, name, 0)?;
                ok_wrap_concr(inst_expand())
            }
            RegReg(inst_expand) => {
                let args = self.consume_commasep_args(head_loc, name, 2)?;
                let rd = self.try_parse_reg(&args[0])?;
                let rs = self.try_parse_reg(&args[1])?;
                ok_wrap_concr(inst_expand(rd, rs))
            }
            LikeJ(inst_expand) => {
                let args = self.consume_commasep_args(head_loc, name, 1)?;
                // j expands to J-type, so 20-bit immediate
                let last_arg = self.try_parse_imm_or_label_ref(20, &args[0])?;
                match last_arg {
                    ImmOrLabel::Imm(imm) => ok_wrap_concr(inst_expand(imm)),
                    ImmOrLabel::Label(tgt_label) => {
                        ok_vec(PartialInst::new_no_reg_needs_label(*inst_expand, tgt_label))
                    }
                }
            }
            OneReg(inst_expand) => {
                let args = self.consume_commasep_args(head_loc, name, 1)?;
                let rs = self.try_parse_reg(&args[0])?;
                ok_wrap_concr(inst_expand(rs))
            }
        }
    }

    fn try_expand_inst(&mut self, head_loc: Location, name: &str) -> ParseResult {
        if let Some(parse_type) = self.data.inst_expansion_table.get(name) {
            self.try_expand_found_inst(head_loc, &name, parse_type)
        // TODO label should be moved onto the vec instead
        } else {
            Err(ParseError::new(
                head_loc,
                "No instruction found with name",
                name,
            ))
        }
    }

    /// Creates a LineParser, with a label possibly inherited from the previous line.
    fn new<'a>(
        data: &'a ParserData,
        tokens: TokenStream,
        maybe_label: &'a Option<Label>,
    ) -> LineParser<'a> {
        let mut iter = tokens.into_iter().peekable();
        let label_passed_in = maybe_label.is_some();
        // Check the first token for a label
        let this_label = match maybe_label {
            Some(l) => Some(l.clone()),
            None => {
                if let Some(first_tok) = iter.peek() {
                    if let TokenType::LabelDef(label_name) = &first_tok.data {
                        Some(label_name.to_string())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        };
        // should consume token
        // can't put this in the above if stmt due to lifetime rules
        if this_label.is_some() && !label_passed_in {
            iter.next();
        }
        LineParser {
            data,
            iter,
            label: this_label,
        }
    }

    fn parse(mut self) -> (Option<Label>, ParseResult) {
        (
            self.label.clone(),
            if let Some(head_tok) = self.iter.next() {
                use TokenType::*;
                match head_tok.data {
                    Name(name) => self.try_expand_inst(head_tok.location, &name),
                    // first label def is handled by contructor
                    // TODO handle multiple labels on same line
                    LabelDef(label_name) => Err(ParseError::new(
                        head_tok.location,
                        "Multiple labels on the same line is unimplemented",
                        &label_name,
                    )),
                    Directive(_section_name) => Ok(Vec::new()), // TODO
                    Comment(..) => Ok(Vec::new()),              // deliberate no-op
                    Comma => Err(ParseError::bad_head(head_tok.location, ",")),
                    Immediate(n, style) => {
                        Err(ParseError::bad_head(head_tok.location, &style.format(n)))
                    }
                    LParen => Err(ParseError::bad_head(head_tok.location, "(")),
                    RParen => Err(ParseError::bad_head(head_tok.location, ")")),
                }
            } else {
                Ok(Vec::new())
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::isa::*;
    use crate::program_state::IRegister::*;

    /// Lexes a program. Asserts that the lex has no errors.
    fn lex(prog: &str) -> Vec<TokenStream> {
        let (toks, errs) = Lexer::from_string(prog.to_string()).lex();
        assert!(errs.is_empty());
        toks
    }

    /// Parses and lexes the provided string, assuming that there are no errors in either phase.
    /// Assumes that there were no lex errors.
    fn parse_and_lex(prog: &str) -> Vec<PartialInst> {
        let (insts, errs) = RiscVParser::from_tokens(lex(prog)).parse();
        println!("{:?}", errs);
        assert!(errs.is_empty());
        insts
    }

    /// Parses and lexes a string assuming it contains instructions that don't need expanding.
    fn parse_and_lex_concr(prog: &str) -> Vec<ConcreteInst> {
        parse_and_lex(prog)
            .into_iter()
            .map(|inst| inst.try_into_concrete_inst())
            .collect()
    }

    #[test]
    /// Tests parsing of a label in the middle and a label at the end.
    fn test_label_defs() {
        let insts = parse_and_lex("add a0, sp, fp\nl1: addi sp, sp, -4\naddi sp, sp, 4\nl2:");
        let expected_concrete = [
            Add::new(A0, SP, FP),
            Addi::new(SP, SP, DataWord::from(-4)),
            Addi::new(SP, SP, DataWord::from(4)),
        ];
        assert_eq!(insts.len(), 3);
        assert_eq!(insts[0].label, None);
        assert_eq!(insts[1].label, Some("l1".to_string()));
        assert_eq!(insts[2].label, None);
        // TODO handle label at end
        // assert_eq!(insts[3].label, Some("l2".to_string()));
        for (partial_inst, exp_inst) in insts.into_iter().zip(expected_concrete.iter()) {
            assert_eq!(
                partial_inst.try_into_concrete_inst().to_machine_code(),
                exp_inst.to_machine_code()
            );
        }
    }

    #[test]
    /// Tests the parsing of labels as an argument.
    fn test_needed_labels() {
        let insts = parse_and_lex("bne x0, x0, l1\nl1: jal ra, end\nend: nop");
        assert_eq!(insts.len(), 3);
        assert_eq!(insts[0].get_needed_label(), Some(&"l1".to_string()));
        assert_eq!(insts[1].get_needed_label(), Some(&"end".to_string()));
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
        let insts = parse_and_lex_concr("add x5, sp, fp");
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0], Add::new(IRegister::from(5), SP, FP));
    }

    #[test]
    fn test_i_arith_parse() {
        // lack of commas is deliberate
        let insts = parse_and_lex_concr("addi sp sp -4");
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0], Addi::new(SP, SP, DataWord::from(-4)));
    }

    #[test]
    fn test_lui_parse() {
        let insts = parse_and_lex_concr("lui a0, 0xD_EADC");
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
        let insts = parse_and_lex_concr("li a0, 0xDEAD_BEEF");
        assert_eq!(insts, Li::expand(A0, DataWord::from(0xDEAD_BEEFu32)));
    }
}
