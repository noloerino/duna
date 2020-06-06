use super::assembler_impl::{ProgramSection, SectionStore};
use super::lexer::*;
use super::parse_error::{ErrLocation, ParseError, ParseErrorReport, ParseErrorReporter};
use super::partial_inst::PartialInst;
use crate::instruction::*;
use crate::isa;
use crate::program_state::*;
use crate::pseudo_inst::*;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::iter::Peekable;
use std::vec::IntoIter;

pub type Label = String;

type ParsedInstStream<T> = Vec<PartialInst<T>>;
type LineParseResult<T> = Result<ParsedInstStream<T>, ParseError>;
pub struct ParseResult<T: MachineDataWidth> {
    pub file_name: String,
    pub insts: ParsedInstStream<T>,
    pub sections: SectionStore,
    pub declared_globals: HashSet<String>,
    pub report: ParseErrorReport,
}

/// State about the program being parsed.
struct ParseState {
    /// The section in which parsed values should be placed.
    /// This should default to text.
    curr_section: ProgramSection,
    sections: SectionStore,
    /// These labels were given to a .global declaration, which means either the
    /// current file defined the symbol and is making it visible to the linker, or
    /// the current file will look for the symbol in another file.
    declared_globals: HashSet<String>,
}

impl ParseState {
    fn new() -> ParseState {
        ParseState {
            curr_section: ProgramSection::Text,
            sections: SectionStore::new(),
            declared_globals: HashSet::new(),
        }
    }
}

#[derive(Copy, Clone)]
/// Describes the arguments needed for a type of function.
/// Due to their unique parsing rules, Jal, Jalr, and Li are hardcoded.
enum ParseType<T: MachineDataWidth> {
    // Base ISA
    R(fn(IRegister, IRegister, IRegister) -> ConcreteInst<T>),
    Arith(fn(IRegister, IRegister, T::RegData) -> ConcreteInst<T>),
    Env(fn() -> ConcreteInst<T>),
    MemL(fn(IRegister, IRegister, T::RegData) -> ConcreteInst<T>),
    MemS(fn(IRegister, IRegister, T::RegData) -> ConcreteInst<T>),
    B(fn(IRegister, IRegister, T::RegData) -> ConcreteInst<T>),
    // Covers "jal ra, label", "jal label", "jal -4" etc.
    Jal,
    // Covers "jalr ra, 0(x1)", "jalr x1", etc.
    Jalr,
    U(fn(IRegister, T::RegData) -> ConcreteInst<T>),
    // Pseudo-instructions
    Li,
    RegReg(fn(IRegister, IRegister) -> ConcreteInst<T>),
    NoArgs(fn() -> ConcreteInst<T>),
    OneReg(fn(IRegister) -> ConcreteInst<T>),
    // Covers "j label", "j -4", etc.
    LikeJ(fn(T::RegData) -> ConcreteInst<T>),
}

struct ParserData<'a, T: MachineDataWidth> {
    inst_expansion_table: &'a HashMap<String, ParseType<T>>,
    reg_expansion_table: &'a HashMap<String, IRegister>,
}

pub struct RiscVParser<'a, T: MachineDataWidth> {
    parser_data: ParserData<'a, T>,
    file_name: String,
    lines: LineTokenStream,
    contents: Cow<'a, str>,
    reporter: ParseErrorReporter,
    state: ParseState,
}

type TokenIter = Peekable<IntoIter<Token>>;

lazy_static! {
    static ref RV32_INST_EXPANSION_TABLE: HashMap<String, ParseType<Width32b>> = {
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

impl RiscVParser<'_, Width32b> {
    pub fn parse_file(path: &str) -> ParseResult<Width32b> {
        RiscVParser::parse_lex_result(Lexer::lex_file(path))
    }

    pub fn parse_str(contents: &str) -> ParseResult<Width32b> {
        RiscVParser::parse_lex_result(Lexer::lex_str(contents))
    }

    pub fn parse_lex_result(lex_result: LexResult) -> ParseResult<Width32b> {
        RiscVParser {
            parser_data: ParserData {
                inst_expansion_table: &RV32_INST_EXPANSION_TABLE,
                reg_expansion_table: &REG_EXPANSION_TABLE,
            },
            file_name: lex_result.file_name,
            lines: lex_result.lines,
            reporter: lex_result.reporter,
            state: ParseState::new(),
            contents: lex_result.contents,
        }
        .parse()
    }

    fn parse(mut self) -> ParseResult<Width32b> {
        let mut insts = Vec::<PartialInst<Width32b>>::new();
        let mut last_label: Option<Label> = None;
        let parser_data = &self.parser_data;
        for (line, contents) in self.lines.into_iter().zip(self.contents.lines()) {
            // line is an iterator over tokens
            // contents is the raw string
            let (found_label, parse_result) = LineParser::new(
                parser_data,
                line,
                &last_label,
                &mut self.state,
                LineContents::new(&self.file_name, contents),
            )
            .parse();
            match parse_result {
                Ok(mut new_insts) => {
                    // if insts is not empty, then that means the label was already used
                    last_label = if new_insts.is_empty() {
                        found_label
                    } else {
                        // stick label onto first inst
                        if let Some(new_label) = found_label {
                            let head_inst = new_insts.remove(0).with_label(new_label.to_string());
                            new_insts.insert(0, head_inst);
                        }
                        None
                    };
                    insts.extend(new_insts);
                }
                Err(new_err) => self.reporter.add_error(new_err),
            }
        }
        ParseResult {
            file_name: self.file_name,
            insts,
            sections: self.state.sections,
            declared_globals: self.state.declared_globals,
            report: self.reporter.into_report(),
        }
    }
}

/// Contains arguments for a memory operation (load or store).
/// The registers correspond to the order in which they appear: for stores, RS2 precedes RS1;
/// for loads, RD preceds RS1.
struct MemArgs<T: RegSize> {
    first_reg: IRegister,
    second_reg: IRegister,
    imm: T,
}

enum ImmOrLabel<T: RegSize> {
    Imm(T),
    Label(Label),
}

/// Convenience method to stuff a PartialInst into a Vec<PartialInst>
fn ok_vec<T: MachineDataWidth>(inst: PartialInst<T>) -> LineParseResult<T> {
    Ok(vec![inst])
}

/// Convenience method to stuff a ConcreteInst into Ok(vec![PartialInst(...)])
fn ok_wrap_concr<T: MachineDataWidth>(inst: ConcreteInst<T>) -> LineParseResult<T> {
    ok_vec(PartialInst::new_complete(inst))
}

/// Convenience method to turn a Vec<ConcreteInst<T>> into Ok(Vec<PartialInst>)
fn ok_wrap_expanded<T: MachineDataWidth>(inst: Vec<ConcreteInst<T>>) -> LineParseResult<T> {
    Ok(inst.into_iter().map(PartialInst::new_complete).collect())
}

/// Parses an immediate that is required to be at most n bits.
/// If the provided immediate is a negative, then the upper (64 - n + 1) bits must all be 1.
/// An i64 is returned, which should then be converted into a RegData type by a parser.
fn try_parse_imm(line_contents: &LineContents, n: u8, token: Token) -> Result<i64, ParseError> {
    match token.data {
        // Check lower n bits
        // We give a pass to negative numbers with high bits set
        TokenType::Immediate(val, radix) => {
            let mask = if n == 64 {
                // Prevent shift overflow
                0
            } else {
                (-1) << (if val < 0 {
                    // Allow the sign bit to be part of the mask
                    n - 1
                } else {
                    n
                })
            };
            let mask_result = val & mask;
            if mask_result != 0 && mask_result != mask {
                Err(ParseError::imm_too_big(
                    ErrLocation::new(&token.location, line_contents),
                    n,
                    &radix.format(val),
                ))
            } else {
                Ok(val)
            }
        }
        _ => Err(ParseError::unexpected_type(
            ErrLocation::new(&token.location, line_contents),
            "immediate",
            token.data,
        )),
    }
}

/// Checks this line's iterator to ensure that there are no more tokens remaining, save
/// for a possible comment.
/// This is used for situations where a fixed number of arguments is expected, as we're
/// free to consume the iterator since more tokens would be an error regardless.
fn check_no_more_args(
    iter: &mut TokenIter,
    line_contents: &LineContents,
    name: &str,
    needed: u8,
) -> Result<(), ParseError> {
    let next = iter.next();
    if let Some(tok) = next {
        if let TokenType::Comment(_) = tok.data {
            Ok(())
        } else {
            Err(ParseError::too_many_args(
                ErrLocation::new(&tok.location, line_contents),
                name,
                needed,
            ))
        }
    } else {
        Ok(())
    }
}

/// Attempts to advance the next token of the iterator, returning a ParseError if there are none.
fn try_next_tok(
    iter: &mut TokenIter,
    line_contents: &LineContents,
    head_loc: &Location,
    name: &str,
    needed_args: u8,
    found_so_far: u8,
) -> Result<Token, ParseError> {
    if let Some(tok) = iter.next() {
        Ok(tok)
    } else {
        Err(ParseError::wrong_argc(
            ErrLocation::new(head_loc, line_contents),
            name,
            needed_args,
            found_so_far,
        ))
    }
}
/// Attempts to consume possibly comma-separate arguments from the iterator.
/// The first and last tokens cannot be commas. If repeated commas appear anywhere,
/// an error is returned.
/// This consumes until a comment token or the end of the iterator is reached.
fn consume_unbounded_commasep_args(
    iter: &mut TokenIter,
    line_contents: &LineContents,
) -> Result<Vec<Token>, ParseError> {
    use TokenType::*;
    let mut toks = Vec::new();
    // track if we just visited a comma
    // initialize to true to prevent leading commas
    let mut was_comma = true;
    for tok in iter {
        match tok.data {
            Name(..) | Immediate(..) | StringLiteral(..) => {
                was_comma = false;
                toks.push(tok)
            }
            Comma => {
                if was_comma {
                    return Err(ParseError::bad_arg(
                        ErrLocation::new(&tok.location, &line_contents),
                        &format!("{:?}", tok.data),
                    ));
                }
                was_comma = true;
            }
            Comment(..) => return Ok(toks),
            _ => {
                return Err(ParseError::bad_arg(
                    ErrLocation::new(&tok.location, &line_contents),
                    &format!("{:?}", tok.data),
                ));
            }
        }
    }
    Ok(toks)
}

/// Responsible for parsing a line with an instruction
struct InstParser<'a, T: MachineDataWidth> {
    data: &'a ParserData<'a, T>,
    iter: TokenIter,
    head_loc: &'a Location,
    inst_name: &'a str,
    line_contents: LineContents,
}

impl<'a, T: MachineDataWidth> InstParser<'a, T> {
    fn new(
        data: &'a ParserData<T>,
        iter: TokenIter,
        head_loc: &'a Location,
        inst_name: &'a str,
        line_contents: LineContents,
    ) -> InstParser<'a, T> {
        InstParser {
            data,
            iter,
            head_loc,
            inst_name,
            line_contents,
        }
    }

    /// Checks this line's iterator to ensure that there are no more tokens remaining, save
    /// for a possible comment.
    /// This is used for situations where a fixed number of arguments is expected, as we're
    /// free to consume the iterator since more tokens would be an error regardless.
    fn check_no_more_args(&mut self, needed: u8) -> Result<(), ParseError> {
        check_no_more_args(&mut self.iter, &self.line_contents, self.inst_name, needed)
    }

    /// Attempts to consume exactly N arguments from the iterator, possibly comma-separated.
    /// The first and last tokens cannot be commas. If repeated commas appear anywhere,
    /// an error is returned.
    /// The only tokens that may appear during this consumption are commas, names, and immediates.
    fn consume_commasep_args(&mut self, n: u8) -> Result<Vec<Token>, ParseError> {
        use TokenType::*;
        let mut found = Vec::<Token>::new();
        for left in (0..n).rev() {
            match self.iter.next() {
                Some(tok) => match tok.data {
                    Name(..) | Immediate(..) => {
                        // Allow single comma, except when trailing
                        if left > 0 {
                            if let Some(tok2) = self.iter.peek() {
                                if let Comma = tok2.data {
                                    self.iter.next();
                                }
                            }
                        }
                        found.push(tok);
                    }
                    _ => {
                        return Err(ParseError::bad_arg(
                            ErrLocation::new(&tok.location, &self.line_contents),
                            &format!("{:?}", tok.data),
                        ))
                    }
                },
                None => {
                    return Err(ParseError::wrong_argc(
                        ErrLocation::new(&self.head_loc, &self.line_contents),
                        self.inst_name,
                        n,
                        found.len() as u8,
                    ))
                }
            }
        }
        debug_assert!(found.len() == (n as usize));
        self.check_no_more_args(n).and(Ok(found))
    }

    /// Attempts to consume possibly comma-separate arguments from the iterator.
    /// The first and last tokens cannot be commas. If repeated commas appear anywhere,
    /// an error is returned.
    /// This consumes until a comment token or the end of the iterator is reached.
    fn consume_unbounded_commasep_args(&mut self) -> Result<Vec<Token>, ParseError> {
        consume_unbounded_commasep_args(&mut self.iter, &self.line_contents)
    }

    /// Consumes tokens for arguments for a memory operation.
    /// These are either of the form "inst reg, imm, reg)" e.g. "lw x1 -4 x2"
    /// or "inst reg, (imm)reg" e.g "lw x1, 4(x2)" (commas optional in both cases)
    fn consume_mem_args(&mut self) -> Result<MemArgs<T::RegData>, ParseError> {
        // first consumed token must be register name
        let first_tok = self.try_next_tok(3, 0)?;
        let first_reg = self.try_parse_reg(first_tok)?;
        // check for comma
        let maybe_comma = self.try_peek_tok(3, 1)?;
        if let TokenType::Comma = maybe_comma.data {
            self.iter.next();
        }
        // must be immediate here
        let imm_tok = self.try_next_tok(3, 1)?;
        let imm = self.try_parse_imm(12, imm_tok)?;
        // check for lparen
        let maybe_lparen = self.try_peek_tok(3, 2)?;
        let is_lparen = if let TokenType::LParen = maybe_lparen.data {
            self.iter.next();
            true
        } else {
            false
        };
        // must be a register here
        let reg2_tok = self.try_next_tok(3, 2)?;
        let second_reg = self.try_parse_reg(reg2_tok)?;
        if is_lparen {
            let maybe_rparen = self.try_next_tok(3, 2)?;
            if let TokenType::RParen = maybe_rparen.data {
            } else {
                return Err(ParseError::unclosed_paren(
                    ErrLocation::new(&maybe_rparen.location, &self.line_contents),
                    maybe_rparen.data,
                ));
            }
        }
        // Any trailing token must be a comment
        self.check_no_more_args(3).and(Ok(MemArgs {
            first_reg,
            second_reg,
            imm,
        }))
    }

    /// Attempts to advance the next token of the iterator, returning a ParseError if there are none.
    fn try_next_tok(&mut self, needed_args: u8, found_so_far: u8) -> Result<Token, ParseError> {
        try_next_tok(
            &mut self.iter,
            &self.line_contents,
            self.head_loc,
            self.inst_name,
            needed_args,
            found_so_far,
        )
    }

    /// Attempts to peek the next token of the iterator, returning a ParseError if there are none.
    fn try_peek_tok(&mut self, needed_args: u8, found_so_far: u8) -> Result<&Token, ParseError> {
        if let Some(tok) = self.iter.peek() {
            Ok(tok)
        } else {
            Err(ParseError::wrong_argc(
                ErrLocation::new(&self.head_loc, &self.line_contents),
                self.inst_name,
                needed_args,
                found_so_far,
            ))
        }
    }

    fn try_parse_reg(&self, token: Token) -> Result<IRegister, ParseError> {
        match &token.data {
            TokenType::Name(name) => {
                self.data
                    .reg_expansion_table
                    .get(name)
                    .cloned()
                    .ok_or_else(|| {
                        ParseError::unexpected_type(
                            ErrLocation::new(&token.location, &self.line_contents),
                            "register name",
                            token.data,
                        )
                    })
            }
            _ => Err(ParseError::unexpected_type(
                ErrLocation::new(&token.location, &self.line_contents),
                "register name",
                token.data,
            )),
        }
    }

    /// Parses an immediate that is required to be at most n bits.
    /// If the provided immediate is a negative, then the upper (64 - n + 1) bits must all be 1.
    fn try_parse_imm(&self, n: u8, token: Token) -> Result<T::RegData, ParseError> {
        try_parse_imm(&self.line_contents, n, token).map(|res_i64| res_i64.into())
    }

    /// Attempts to expand a token into a label reference or an immediate of at most max_imm_len.
    fn try_parse_imm_or_label_ref(
        &self,
        max_imm_len: u8,
        token: Token,
    ) -> Result<ImmOrLabel<T::RegData>, ParseError> {
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
        parse_type: &ParseType<T>,
    ) -> Result<ParsedInstStream<T>, ParseError> {
        use ParseType::*;
        match parse_type {
            R(inst_new) => {
                // R-types are always "inst rd, rs1, rs2" with one or no commas in between
                let mut args = self.consume_commasep_args(3)?;
                let rd = self.try_parse_reg(args.remove(0))?;
                let rs1 = self.try_parse_reg(args.remove(0))?;
                let rs2 = self.try_parse_reg(args.remove(0))?;
                ok_wrap_concr(inst_new(rd, rs1, rs2))
            }
            Arith(inst_new) => {
                let mut args = self.consume_commasep_args(3)?;
                let rd = self.try_parse_reg(args.remove(0))?;
                let rs1 = self.try_parse_reg(args.remove(0))?;
                let imm = self.try_parse_imm(12, args.remove(0))?;
                ok_wrap_concr(inst_new(rd, rs1, imm))
            }
            Env(inst_new) => {
                let _args = self.consume_commasep_args(0)?;
                ok_wrap_concr(inst_new())
            }
            MemL(inst_new) => {
                let args = self.consume_mem_args()?;
                let rd = args.first_reg;
                let rs1 = args.second_reg;
                let imm = args.imm;
                ok_wrap_concr(inst_new(rd, rs1, imm))
            }
            MemS(inst_new) => {
                let args = self.consume_mem_args()?;
                let rs2 = args.first_reg;
                let rs1 = args.second_reg;
                let imm = args.imm;
                ok_wrap_concr(inst_new(rs1, rs2, imm))
            }
            B(inst_new) => {
                let mut args = self.consume_commasep_args(3)?;
                let rs1 = self.try_parse_reg(args.remove(0))?;
                let rs2 = self.try_parse_reg(args.remove(0))?;
                // Becuse branches actually chop off the LSB, we can take up to 13b
                let last_arg = self.try_parse_imm_or_label_ref(13, args.remove(0))?;
                match last_arg {
                    ImmOrLabel::Imm(imm) => {
                        if u8::from(imm.get_byte(0)) & 1 > 0 {
                            Err(ParseError::generic(
                                ErrLocation::new(&self.head_loc, &self.line_contents),
                                &format!(
                                    "branch immediates must be multiples of two, got {}",
                                    imm.to_string()
                                ),
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
                let mut args = self.consume_unbounded_commasep_args()?;
                let argc = args.len();
                match argc {
                    1 => {
                        // "jal label"
                        let last_arg = self.try_parse_imm_or_label_ref(20, args.remove(0))?;
                        match last_arg {
                            ImmOrLabel::Imm(imm) => ok_wrap_concr(JalPseudo::expand(imm)),
                            ImmOrLabel::Label(tgt_label) => ok_vec(
                                PartialInst::new_no_reg_needs_label(JalPseudo::expand, tgt_label),
                            ),
                        }
                    }
                    2 => {
                        // "jal ra label"
                        let rd = self.try_parse_reg(args.remove(0))?;
                        // J-type has 20-bit immediate
                        let last_arg = self.try_parse_imm_or_label_ref(20, args.remove(0))?;
                        match last_arg {
                            ImmOrLabel::Imm(imm) => ok_wrap_concr(isa::Jal::new(rd, imm)),
                            ImmOrLabel::Label(tgt_label) => ok_vec(
                                PartialInst::new_one_reg_needs_label(isa::Jal::new, rd, tgt_label),
                            ),
                        }
                    }
                    _ => Err(ParseError::wrong_diff_argc(
                        ErrLocation::new(&self.head_loc, &self.line_contents),
                        self.inst_name,
                        1,
                        2,
                        argc as u8,
                    )),
                }
            }
            Jalr => {
                let mut args = self.consume_unbounded_commasep_args()?;
                let argc = args.len();
                match argc {
                    1 => {
                        // "jalr rs"
                        let rs = self.try_parse_reg(args.remove(0))?;
                        ok_wrap_concr(JalrPseudo::expand(rs))
                    }
                    3 => {
                        // "jalr rd, rs, imm"
                        let rd = self.try_parse_reg(args.remove(0))?;
                        let rs1 = self.try_parse_reg(args.remove(0))?;
                        let imm = self.try_parse_imm(12, args.remove(0))?;
                        ok_wrap_concr(isa::Jalr::new(rd, rs1, imm))
                    }
                    _ => Err(ParseError::wrong_diff_argc(
                        ErrLocation::new(&self.head_loc, &self.line_contents),
                        self.inst_name,
                        1,
                        3,
                        argc as u8,
                    )),
                }
            }
            U(inst_new) => {
                let mut args = self.consume_commasep_args(2)?;
                let rd = self.try_parse_reg(args.remove(0))?;
                let imm = self.try_parse_imm(20, args.remove(0))?;
                ok_wrap_concr(inst_new(rd, imm))
            }
            Li => {
                let mut args = self.consume_commasep_args(2)?;
                let rd = self.try_parse_reg(args.remove(0))?;
                let imm = self.try_parse_imm(32, args.remove(0))?;
                ok_wrap_expanded(crate::pseudo_inst::Li::expand(rd, imm))
            }
            NoArgs(inst_expand) => {
                let _args = self.consume_commasep_args(0)?;
                ok_wrap_concr(inst_expand())
            }
            RegReg(inst_expand) => {
                let mut args = self.consume_commasep_args(2)?;
                let rd = self.try_parse_reg(args.remove(0))?;
                let rs = self.try_parse_reg(args.remove(0))?;
                ok_wrap_concr(inst_expand(rd, rs))
            }
            LikeJ(inst_expand) => {
                let mut args = self.consume_commasep_args(1)?;
                // j expands to J-type, so 20-bit immediate
                let last_arg = self.try_parse_imm_or_label_ref(20, args.remove(0))?;
                match last_arg {
                    ImmOrLabel::Imm(imm) => ok_wrap_concr(inst_expand(imm)),
                    ImmOrLabel::Label(tgt_label) => {
                        ok_vec(PartialInst::new_no_reg_needs_label(*inst_expand, tgt_label))
                    }
                }
            }
            OneReg(inst_expand) => {
                let mut args = self.consume_commasep_args(1)?;
                let rs = self.try_parse_reg(args.remove(0))?;
                ok_wrap_concr(inst_expand(rs))
            }
        }
    }

    fn try_expand_inst(&mut self) -> LineParseResult<T> {
        if let Some(parse_type) = self.data.inst_expansion_table.get(self.inst_name) {
            self.try_expand_found_inst(parse_type)
        } else {
            Err(ParseError::bad_inst_name(
                ErrLocation::new(&self.head_loc, &self.line_contents),
                self.inst_name,
            ))
        }
    }
}

/// Responsible for parsing a line with a directive.
struct DirectiveParser<'a> {
    iter: TokenIter,
    state: &'a mut ParseState,
    head_loc: &'a Location,
    head_directive: &'a str,
    line_contents: LineContents,
}

type DirectiveParseResult = Result<(), ParseError>;

impl<'a> DirectiveParser<'a> {
    fn new(
        iter: TokenIter,
        state: &'a mut ParseState,
        head_loc: &'a Location,
        head_directive: &'a str,
        line_contents: LineContents,
    ) -> DirectiveParser<'a> {
        DirectiveParser {
            iter,
            state,
            head_loc,
            head_directive,
            line_contents,
        }
    }

    fn parse(self) -> DirectiveParseResult {
        match self.head_directive {
            // sections
            "section" => self.parse_section(),
            "text" => {
                self.state.curr_section = ProgramSection::Text;
                self.ok(0)
            }
            "data" => {
                self.state.curr_section = ProgramSection::Data;
                self.ok(0)
            }
            "rodata" => {
                self.state.curr_section = ProgramSection::Rodata;
                self.ok(0)
            }
            // literal insertions
            "byte" => self.parse_data(DataWidth::Byte),
            "2byte" | "half" | "short" => self.parse_data(DataWidth::Half),
            "4byte" | "word" | "long" => self.parse_data(DataWidth::Word),
            "8byte" | "dword" | "quad" => self.parse_data(DataWidth::DoubleWord),
            "zero" => self.parse_zero(),
            "ascii" => self.parse_string(false),
            "asciz" | "string" => self.parse_string(true),
            // symbol declarations
            "global" | "globl" => self.parse_global_label(),
            // TODO: equ, set, equiv (refactor symbol table to have enum value)
            _ => Err(ParseError::unsupported_directive(
                ErrLocation::new(&self.head_loc, &self.line_contents),
                self.head_directive,
            )),
        }
    }

    /// Emits zero or more integer literal declarations through .byte, .word, etc.
    fn parse_data(mut self, kind: DataWidth) -> DirectiveParseResult {
        use ProgramSection::*;
        match self.state.curr_section {
            Text => Err(ParseError::unimplemented(
                ErrLocation::new(&self.head_loc, &self.line_contents),
                "cannot insert literals in .text section (only instructions allowed)",
            )),
            section => {
                let toks = self.consume_unbounded_commasep_args()?;
                for tok in toks {
                    use DataWidth::*;
                    match kind {
                        Byte => {
                            let val: u8 = self.try_parse_imm(8, tok)? as u8;
                            self.state.sections.add_byte(section, val)
                        }
                        Half => {
                            let val: u16 = self.try_parse_imm(16, tok)? as u16;
                            self.state.sections.add_half(section, val)
                        }
                        Word => {
                            let val: u32 = self.try_parse_imm(32, tok)? as u32;
                            self.state.sections.add_word(section, val)
                        }
                        DoubleWord => {
                            let val: u64 = self.try_parse_imm(32, tok)? as u64;
                            self.state.sections.add_doubleword(section, val)
                        }
                    }
                }
                // should never fail since we've consumed the whole iterator
                self.ok(0)
            }
        }
    }

    fn parse_section(mut self) -> DirectiveParseResult {
        let next_tok = self.try_next_tok(1, 0)?;
        if let TokenType::Directive(s) = &next_tok.data {
            match s.as_str() {
                "text" => {
                    self.state.curr_section = ProgramSection::Text;
                    return self.ok(1);
                }
                "data" => {
                    self.state.curr_section = ProgramSection::Data;
                    return self.ok(1);
                }
                "rodata" => {
                    self.state.curr_section = ProgramSection::Rodata;
                    return self.ok(1);
                }
                "bss" => {
                    return Err(ParseError::unimplemented(
                        ErrLocation::new(&next_tok.location, &self.line_contents),
                        "bss section",
                    ))
                }
                _ => {}
            }
        }
        Err(ParseError::unexpected_type(
            ErrLocation::new(&next_tok.location, &self.line_contents),
            "one of [.text, .bss, .data, .rodata]",
            next_tok.data,
        ))
    }

    /// Emits zero or more string literals.
    ///
    /// See https://sourceware.org/binutils/docs/as/Ascii.html#Ascii
    fn parse_string(mut self, null_terminated: bool) -> DirectiveParseResult {
        if let ProgramSection::Text = self.state.curr_section {
            return Err(ParseError::unimplemented(
                ErrLocation::new(&self.head_loc, &self.line_contents),
                "cannot insert literals in .text section (only instructions allowed)",
            ));
        }
        let toks = self.consume_unbounded_commasep_args()?;
        for tok in toks {
            if let TokenType::StringLiteral(s) = &tok.data {
                for c in s.chars() {
                    self.state
                        .sections
                        .add_byte(self.state.curr_section, c as u8)
                }
                if null_terminated {
                    self.state.sections.add_byte(self.state.curr_section, 0);
                }
            } else {
                return Err(ParseError::unexpected_type(
                    ErrLocation::new(&tok.location, &self.line_contents),
                    "string literal",
                    tok.data,
                ));
            }
        }
        self.ok(0)
    }

    fn parse_zero(mut self) -> DirectiveParseResult {
        if let ProgramSection::Text = self.state.curr_section {
            return Err(ParseError::unimplemented(
                ErrLocation::new(&self.head_loc, &self.line_contents),
                "cannot insert literals in .text section (only instructions allowed)",
            ));
        }
        let next_tok = self.try_next_tok(1, 0)?;
        if let TokenType::Immediate(n, ..) = next_tok.data {
            // insert n bytes of zeroes
            if n < 0 {
                Err(ParseError::unexpected_type(
                    ErrLocation::new(&next_tok.location, &self.line_contents),
                    "positive integer literal",
                    next_tok.data,
                ))
            } else {
                for _ in 0..n {
                    self.state.sections.add_byte(self.state.curr_section, 0)
                }
                self.ok(1)
            }
        } else {
            Err(ParseError::unexpected_type(
                ErrLocation::new(&next_tok.location, &self.line_contents),
                "integer literal",
                next_tok.data,
            ))
        }
    }

    /// Indicates that a symbol is declared globally.
    fn parse_global_label(mut self) -> DirectiveParseResult {
        let next_tok = self.try_next_tok(1, 0)?;
        if let TokenType::Name(name) = next_tok.data {
            // redeclared labels are ok in this context
            self.state.declared_globals.insert(name);
            self.ok(1)
        } else {
            Err(ParseError::unexpected_type(
                ErrLocation::new(&next_tok.location, &self.line_contents),
                "integer literal",
                next_tok.data,
            ))
        }
    }

    fn try_next_tok(&mut self, needed_args: u8, found_so_far: u8) -> Result<Token, ParseError> {
        try_next_tok(
            &mut self.iter,
            &self.line_contents,
            self.head_loc,
            self.head_directive,
            needed_args,
            found_so_far,
        )
    }

    fn try_parse_imm(&self, n: u8, token: Token) -> Result<i64, ParseError> {
        try_parse_imm(&self.line_contents, n, token)
    }

    fn consume_unbounded_commasep_args(&mut self) -> Result<Vec<Token>, ParseError> {
        consume_unbounded_commasep_args(&mut self.iter, &self.line_contents)
    }

    /// Checks that the iterator has run out of tokens; returns ok if so.
    /// needed_argc is the number of arguments that were needed in total, not the number that
    /// still need to be consumed (which is always 0, since we're checking if we ran out of args).
    fn ok(mut self, needed_argc: u8) -> DirectiveParseResult {
        check_no_more_args(
            &mut self.iter,
            &self.line_contents,
            self.head_directive,
            needed_argc,
        )?;
        Ok(())
    }
}

/// Responsible for parsing a line.
struct LineParser<'a, T: MachineDataWidth> {
    data: &'a ParserData<'a, T>,
    iter: TokenIter,
    label: Option<Label>,
    state: &'a mut ParseState,
    line_contents: LineContents,
}

impl<'a, T: MachineDataWidth> LineParser<'a, T> {
    /// Creates a LineParser, with a label possibly inherited from the previous line.
    fn new(
        data: &'a ParserData<T>,
        tokens: TokenStream,
        maybe_label: &'a Option<Label>,
        state: &'a mut ParseState,
        line_contents: LineContents,
    ) -> LineParser<'a, T> {
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
            state,
            line_contents,
        }
    }

    fn parse(mut self) -> (Option<Label>, LineParseResult<T>) {
        (
            self.label,
            if let Some(head_tok) = self.iter.next() {
                let errloc = ErrLocation::new(&head_tok.location, &self.line_contents);
                use TokenType::*;
                match head_tok.data {
                    Name(name) => {
                        if self.state.curr_section == ProgramSection::Text {
                            InstParser::new(
                                self.data,
                                self.iter,
                                &head_tok.location,
                                &name,
                                self.line_contents,
                            )
                            .try_expand_inst()
                        } else {
                            Err(ParseError::unsupported_directive(
                                errloc,
                                &format!(
                                    "instructions can only be in the .text section (current section is {})",
                                    self.state.curr_section
                                ),
                            ))
                        }
                    }
                    // first label def is handled by contructor
                    // TODO handle multiple labels on same line
                    LabelDef(label_name) => Err(ParseError::generic(
                        errloc,
                        &format!(
                            "Multiple labels on the same line is unimplemented (found label {})",
                            label_name
                        ),
                    )),
                    Directive(section_name) => DirectiveParser::new(
                        self.iter,
                        &mut self.state,
                        &head_tok.location,
                        &section_name,
                        self.line_contents,
                    )
                    .parse()
                    .and(Ok(Vec::new())),
                    Comment(..) => Ok(Vec::new()), // deliberate no-op
                    Comma => Err(ParseError::bad_head(errloc, ",")),
                    Immediate(n, style) => Err(ParseError::bad_head(errloc, &style.format(n))),
                    StringLiteral(s) => Err(ParseError::bad_head(errloc, &s)),
                    LParen => Err(ParseError::bad_head(errloc, "(")),
                    RParen => Err(ParseError::bad_head(errloc, ")")),
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
    use crate::instruction::ConcreteInst;
    use crate::isa::*;
    use crate::program_state::IRegister::*;
    use crate::program_state::{DataWord, Width32b};

    /// Lexes a program. Asserts that the lex has no errors.
    fn lex(prog: &str) -> LexResult {
        let result = Lexer::lex_str(prog);
        assert_eq!(result.reporter.get_errs(), &[]);
        result
    }

    /// Parses and lexes the provided string, assuming that there are no errors in either phase.
    /// Assumes that there were no lex errors.
    fn parse_and_lex(prog: &str) -> Vec<PartialInst<Width32b>> {
        let ParseResult { insts, report, .. } = RiscVParser::parse_lex_result(lex(prog));
        assert!(report.is_empty(), format!("{:?}", report.report()));
        insts
    }

    /// Parses and lexes a string assuming it contains instructions that don't need expanding.
    fn parse_and_lex_concr(prog: &str) -> Vec<ConcreteInst<Width32b>> {
        parse_and_lex(prog)
            .into_iter()
            .map(|inst| inst.try_into_concrete_inst())
            .collect()
    }

    #[test]
    /// Tests the loading of immediates in the .data section.
    fn test_data_directives() {
        let prog = ".section .data\n.byte 0x12\n.word 0xdeadbeef";
        let ParseResult {
            report,
            sections,
            insts,
            ..
        } = RiscVParser::parse_str(prog);
        assert!(report.is_empty(), insts.is_empty());
        assert_eq!(sections.data, vec![0x12, 0xef, 0xbe, 0xad, 0xde]);
    }

    #[test]
    fn test_data_directives_bad() {
        let programs = [
            ".data 1",                     // requires .byte or similar
            ".section .data\n.byte 0x123", // immediate too large
        ];
        for prog in &programs {
            let ParseResult { report, .. } = RiscVParser::parse_str(prog);
            assert!(!report.is_empty());
        }
    }

    #[test]
    /// Tests parsing of a label in the middle and a label at the end.
    fn test_label_defs() {
        let insts = parse_and_lex("add a0, sp, fp\nl1: addi sp, sp, -4\naddi sp, sp, 4\nl2:");
        let expected_concrete: [ConcreteInst<Width32b>; 3] = [
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
            let ParseResult { report, .. } = RiscVParser::parse_str(inst);
            assert!(!report.is_empty());
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
        let ParseResult { insts, report, .. } = RiscVParser::parse_str("addi sp sp 0xF000");
        assert!(!report.is_empty());
        assert!(insts.is_empty());
    }

    #[test]
    fn test_pseudo_li() {
        let insts = parse_and_lex_concr("li a0, 0xDEAD_BEEF");
        assert_eq!(insts, Li::expand(A0, DataWord::from(0xDEAD_BEEFu32)));
    }
}
