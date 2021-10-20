use super::{arch::*, instruction::*, isa, isa::*, registers::RiscVRegister};
use crate::{
    assembler::{lexer::*, parser::*, *},
    data_structures::*,
};
use std::{collections::HashMap, marker::PhantomData};

/// Describes the arguments needed for a type of function.
/// Due to their unique parsing rules, Jal, Jalr, and La are hardcoded.
#[derive(Copy, Clone)]
pub enum ParseType<S: AtLeast32b> {
    // Base ISA
    R(fn(RiscVRegister, RiscVRegister, RiscVRegister) -> RiscVInst<S>),
    Arith(fn(RiscVRegister, RiscVRegister, RegValue<S>) -> RiscVInst<S>),
    // "ecall" and "ebreak"
    Env(fn() -> RiscVInst<S>),
    MemL(fn(RiscVRegister, RiscVRegister, RegValue<S>) -> RiscVInst<S>),
    MemS(fn(RiscVRegister, RiscVRegister, RegValue<S>) -> RiscVInst<S>),
    B(fn(RiscVRegister, RiscVRegister, RegValue<S>) -> RiscVInst<S>),
    // Covers "jal ra, label", "jal label", "jal -4" etc.
    Jal,
    // Covers "jalr ra, x1, 0", "jalr x1", etc. ("jalr ra, 0(x1)" not yet supported)
    Jalr,
    U(fn(RiscVRegister, RegValue<S>) -> RiscVInst<S>),
    // Pseudo-instructions
    // La is special because it produces two instructions and takes a label
    La,
    // Li is split to allow for distinction between 32 and 64-bit variants
    Li(fn(RiscVRegister, RegValue<S>) -> Vec<RiscVInst<S>>),
    RegReg(fn(RiscVRegister, RiscVRegister) -> RiscVInst<S>),
    NoArgs(fn() -> RiscVInst<S>),
    OneReg(fn(RiscVRegister) -> RiscVInst<S>),
    // Covers "j label", "j -4", etc.
    LikeJ(fn(RegValue<S>) -> RiscVInst<S>),
}

lazy_static! {
    static ref RV32_INST_EXPANSION_TABLE: HashMap<String, ParseType<W32b>> = {
        use super::isa::*;
        use ParseType::*;
        [
            // === Base ===
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
            // ("fence", ???),
            ("ecall", Env(Ecall::new)),
            ("jal", ParseType::Jal),
            ("jalr", ParseType::Jalr),
            ("lb", MemL(Lb::new)),
            ("lbu", MemL(Lbu::new)),
            ("lh", MemL(Lh::new)),
            ("lhu", MemL(Lhu::new)),
            ("lui", U(Lui::new)),
            ("lw", MemL(Lw::new)),
            ("or", R(Or::new)),
            ("ori", Arith(Ori::new)),
            ("sb", MemS(Sb::new)),
            ("sh", MemS(Sh::new)),
            ("sll", R(Sll::new)),
            ("slli", Arith(Slli::new)),
            ("slt", R(Slt::new)),
            ("slti", Arith(Slti::new)),
            ("sltiu", Arith(Sltiu::new)),
            ("sltu", R(Sltu::new)),
            ("sra", R(Sra::new)),
            ("srai", Arith(Srai::new)),
            ("srl", R(Srl::new)),
            ("srli", Arith(Srli::new)),
            ("sub", R(Sub::new)),
            ("sw", MemS(Sw::new)),
            ("xor", R(Xor::new)),
            ("xori", Arith(Xori::new)),
            // === Pseudo ===
            ("la", ParseType::La),
            ("li", Li(Li32::expand)),
            ("mv", RegReg(Mv::expand)),
            ("neg", RegReg(Neg::expand)),
            ("nop", NoArgs(Nop::expand)),
            ("not", RegReg(Not::expand)),
            ("j", LikeJ(J::expand)),
            ("jr", OneReg(Jr::expand)),
            ("ret", NoArgs(Ret::expand)),
            // === M extension ===
            ("mul", R(Mul::new)),
            // ("mulh", R(Mulh::new)),
            // ("mulhu", R(Mulhu::new)),
            // ("mulhsu", R(Mulsu::new)),
            ("div", R(Div::new)),
            ("divu", R(Divu::new)),
            ("rem", R(Rem::new)),
            ("remu", R(Remu::new)),
            // === Zicsr ===
            ("csrrw", Arith(Csrrw::new)),
            ("csrrs", Arith(Csrrs::new)),
            ("csrrc", Arith(Csrrc::new)),
            // ("csrrwi", Arith(Csrrwi::new)),
            // ("csrrsi", Arith(Csrrsi::new)),
            // ("csrrci", Arith(Csrrci::new)),
            // === Zicsr pseudo
            // ("rdinstret", ???),
            // ("rdinstreth", ???),
            // ("rdcycle", ???),
            // ("rdcycleh", ???),
            // ("rdtime", ???),
            // ("rdtimeh", ???),
            // ("csrr", ???),
            // ("csrw", ???),
            // ("csrs", ???),
            // ("csrc", ???),
            // ("csrwi", ???),
            // ("csrsi", ???),
            // ("csrci", ???),
        ]
        .iter()
        .cloned()
        .map(|(s, t)| (s.to_string(), t))
        .collect()
    };

    static ref RV64_INST_EXPANSION_TABLE: HashMap<String, ParseType<W64b >> = {
        use super::isa::*;
        use ParseType::*;
        [
            // === Base ===
            ("add", R(Add::new)),
            ("addw", R(Addw::new)),
            ("addi", Arith(Addi::new)),
            ("addiw", Arith(Addiw::new)),
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
            // ("fence", ???),
            ("ecall", Env(Ecall::new)),
            ("jal", ParseType::Jal),
            ("jalr", ParseType::Jalr),
            ("lb", MemL(Lb::new)),
            ("lbu", MemL(Lbu::new)),
            ("ld", MemL(Ld::new)),
            ("lh", MemL(Lh::new)),
            ("lhu", MemL(Lhu::new)),
            ("lui", U(Lui::new)),
            ("lw", MemL(Lw::new)),
            ("lwu", MemL(Lwu::new)),
            ("or", R(Or::new)),
            ("ori", Arith(Ori::new)),
            ("sb", MemS(Sb::new)),
            ("sd", MemS(Sd::new)),
            ("sh", MemS(Sh::new)),
            ("sll", R(Sll::new)),
            ("slli", Arith(Slli::new)),
            ("slliw", Arith(Slliw::new)),
            ("sllw", R(Sllw::new)),
            ("slt", R(Slt::new)),
            ("slti", Arith(Slti::new)),
            ("sltiu", Arith(Sltiu::new)),
            ("sltu", R(Sltu::new)),
            ("sra", R(Sra::new)),
            ("srai", Arith(Srai::new)),
            ("sraiw", Arith(Sraiw::new)),
            ("sraw", R(Sraw::new)),
            ("srl", R(Srl::new)),
            ("srli", Arith(Srli::new)),
            ("srliw", Arith(Srliw::new)),
            ("srlw", R(Srlw::new)),
            ("sub", R(Sub::new)),
            ("subw", R(Subw::new)),
            ("sw", MemS(Sw::new)),
            ("xor", R(Xor::new)),
            ("xori", Arith(Xori::new)),
            // === Pseudo ===
            ("la", ParseType::La),
            ("li", Li(Li64::expand)),
            ("mv", RegReg(Mv::expand)),
            ("neg", RegReg(Neg::expand)),
            ("nop", NoArgs(Nop::expand)),
            ("not", RegReg(Not::expand)),
            ("j", LikeJ(J::expand)),
            ("jr", OneReg(Jr::expand)),
            ("ret", NoArgs(Ret::expand)),
            ("sext.w", RegReg(SextW::expand)),
            // === M extension ===
            ("mul", R(Mul::new)),
            ("mulw", R(Mulw::new)),
            // ("mulh", R(Mulh::new)),
            // ("mulhu", R(Mulhu::new)),
            // ("mulhsu", R(Mulsu::new)),
            ("div", R(Div::new)),
            ("divw", R(Divw::new)),
            ("divu", R(Divu::new)),
            ("divuw", R(Divuw::new)),
            ("rem", R(Rem::new)),
            ("remw", R(Remw::new)),
            ("remu", R(Remu::new)),
            ("remuw", R(Remuw::new)),
            // === Zicsr ===
            ("csrrw", Arith(Csrrw::new)),
            ("csrrs", Arith(Csrrs::new)),
            ("csrrc", Arith(Csrrc::new)),
            // ("csrrwi", Arith(Csrrwi::new)),
            // ("csrrsi", Arith(Csrrsi::new)),
            // ("csrrci", Arith(Csrrci::new)),
            // === Zicsr pseudo
            // ("rdinstret", ???),
            // ("rdinstreth", ???),
            // ("rdcycle", ???),
            // ("rdcycleh", ???),
            // ("rdtime", ???),
            // ("rdtimeh", ???),
            // ("csrr", ???),
            // ("csrw", ???),
            // ("csrs", ???),
            // ("csrc", ???),
            // ("csrwi", ???),
            // ("csrsi", ???),
            // ("csrci", ???),
        ]
        .iter()
        .cloned()
        .map(|(s, t)| (s.to_string(), t))
        .collect()
    };

    static ref REG_EXPANSION_TABLE: HashMap<String, RiscVRegister> = {
        let mut reg_expansion_table: HashMap<String, RiscVRegister> = RiscVRegister::REG_ARRAY
            .iter()
            .map(|r| (r.to_string(), *r))
            .collect();
        for i in 0..32 {
            reg_expansion_table.insert(format!("x{}", i), RiscVRegister::from(i));
        }
        // don't forget Fp
        reg_expansion_table.insert("fp".to_string(), RiscVRegister::Fp);
        reg_expansion_table
    };
}

/// Contains arguments for a memory operation (load or store).
/// The registers correspond to the order in which they appear: for stores, RS2 precedes RS1;
/// for loads, RD preceds RS1.
struct MemArgs<S: AtLeast32b> {
    first_reg: RiscVRegister,
    second_reg: RiscVRegister,
    imm: RegValue<S>,
}

enum ImmOrLabelRef<S: AtLeast32b> {
    Imm(RegValue<S>),
    LabelRef(LabelRef),
}

pub struct RiscVInstParser<S: AtLeast32b> {
    _phantom: PhantomData<S>,
}

type RvInstParseState<'a, S> = InstParseState<'a, RiscV<S>, S, ParseType<S>>;

impl InstParser<RiscV<W32b>, W32b> for RiscVInstParser<W32b> {
    type ParseType = ParseType<W32b>;

    fn inst_expansion_table() -> &'static HashMap<String, Self::ParseType> {
        &RV32_INST_EXPANSION_TABLE
    }

    fn reg_expansion_table() -> &'static HashMap<String, RiscVRegister> {
        &REG_EXPANSION_TABLE
    }

    fn try_expand_found_inst(
        state: RvInstParseState<'_, W32b>,
        parse_type: &ParseType<W32b>,
    ) -> InstParseResult<RiscV<W32b>, W32b> {
        Self::try_expand_found_inst(state, parse_type)
    }
}

impl InstParser<RiscV<W64b>, W64b> for RiscVInstParser<W64b> {
    type ParseType = ParseType<W64b>;

    fn inst_expansion_table() -> &'static HashMap<String, Self::ParseType> {
        &RV64_INST_EXPANSION_TABLE
    }

    fn reg_expansion_table() -> &'static HashMap<String, RiscVRegister> {
        &REG_EXPANSION_TABLE
    }

    fn try_expand_found_inst(
        state: RvInstParseState<'_, W64b>,
        parse_type: &ParseType<W64b>,
    ) -> InstParseResult<RiscV<W64b>, W64b> {
        Self::try_expand_found_inst(state, parse_type)
    }
}

impl<S: AtLeast32b> RiscVInstParser<S> {
    /// Consumes tokens for arguments for a memory operation.
    /// These are either of the form "inst reg, imm, reg)" e.g. "lw x1 -4 x2"
    /// or "inst reg, (imm)reg" e.g "lw x1, 4(x2)" (commas optional in both cases)
    fn consume_mem_args(state: &mut RvInstParseState<'_, S>) -> Result<MemArgs<S>, ParseError> {
        // first consumed token must be register name
        let first_tok = state.try_next_tok(3, 0)?;
        let first_reg = state.try_parse_reg(first_tok)?;
        // check for comma
        let maybe_comma = state.try_peek_tok(3, 1)?;
        if let TokenType::Comma = maybe_comma.data {
            state.iter.next();
        }
        // must be immediate here
        let imm_tok = state.try_next_tok(3, 1)?;
        let imm = state.try_parse_imm(12, imm_tok)?;
        // check for lparen
        let maybe_lparen = state.try_peek_tok(3, 2)?;
        let is_lparen = if let TokenType::LParen = maybe_lparen.data {
            state.iter.next();
            true
        } else {
            false
        };
        // must be a register here
        let reg2_tok = state.try_next_tok(3, 2)?;
        let second_reg = state.try_parse_reg(reg2_tok)?;
        if is_lparen {
            let maybe_rparen = state.try_next_tok(3, 2)?;
            if let TokenType::RParen = maybe_rparen.data {
            } else {
                return Err(ParseError::unclosed_paren(
                    ErrMetadata::new(&maybe_rparen.location),
                    maybe_rparen.data,
                ));
            }
        }
        // Any trailing token must be a comment
        state.check_no_more_args(3).and(Ok(MemArgs {
            first_reg,
            second_reg,
            imm,
        }))
    }

    /// Attempts to expand a token into a label reference or an immediate of at most max_imm_len.
    fn try_parse_imm_or_label_ref(
        state: &RvInstParseState<'_, S>,
        max_imm_len: u8,
        token: Token,
    ) -> Result<ImmOrLabelRef<S>, ParseError> {
        Ok(match &token.data {
            TokenType::Name(name) => {
                // label case
                ImmOrLabelRef::LabelRef(LabelRef::new(name.clone(), token.location))
            }
            TokenType::Directive(name) => {
                // if an item is lexed starting with a period in this position,
                // it's actually a name, so we have to special case this
                // and add the leading period
                let mut with_period: String = ".".to_owned();
                with_period.push_str(name);
                ImmOrLabelRef::LabelRef(LabelRef::new(with_period, token.location))
            }
            _ => {
                // imm case
                ImmOrLabelRef::Imm(state.try_parse_imm(max_imm_len, token)?)
            }
        })
    }

    /// Expands an instruction that is known to be in the expansion table.
    fn try_expand_found_inst(
        mut owned_state: RvInstParseState<'_, S>,
        parse_type: &ParseType<S>,
    ) -> InstParseResult<RiscV<S>, S> {
        use ParseType::*;
        let state = &mut owned_state;
        match parse_type {
            R(inst_new) => {
                // R-types are always "inst rd, rs1, rs2" with one or no commas in between
                let mut args = state.consume_commasep_args(3)?;
                let rd = state.try_parse_reg(args.remove(0))?;
                let rs1 = state.try_parse_reg(args.remove(0))?;
                let rs2 = state.try_parse_reg(args.remove(0))?;
                ok_wrap_concr(inst_new(rd, rs1, rs2))
            }
            Arith(inst_new) => {
                let mut args = state.consume_commasep_args(3)?;
                let rd = state.try_parse_reg(args.remove(0))?;
                let rs1 = state.try_parse_reg(args.remove(0))?;
                let imm = state.try_parse_imm(12, args.remove(0))?;
                ok_wrap_concr(inst_new(rd, rs1, imm))
            }
            Env(inst_new) => {
                let _args = state.consume_commasep_args(0)?;
                ok_wrap_concr(inst_new())
            }
            MemL(inst_new) => {
                let args = Self::consume_mem_args(state)?;
                let rd = args.first_reg;
                let rs1 = args.second_reg;
                let imm = args.imm;
                ok_wrap_concr(inst_new(rd, rs1, imm))
            }
            MemS(inst_new) => {
                let args = Self::consume_mem_args(state)?;
                let rs2 = args.first_reg;
                let rs1 = args.second_reg;
                let imm = args.imm;
                ok_wrap_concr(inst_new(rs1, rs2, imm))
            }
            B(inst_new) => {
                let mut args = state.consume_commasep_args(3)?;
                let rs1 = state.try_parse_reg(args.remove(0))?;
                let rs2 = state.try_parse_reg(args.remove(0))?;
                // Becuse branches actually chop off the LSB, we can take up to 13b
                let last_arg = Self::try_parse_imm_or_label_ref(state, 13, args.remove(0))?;
                match last_arg {
                    ImmOrLabelRef::Imm(imm) => {
                        if u8::from(imm.get_byte(0)) & 1 > 0 {
                            Err(ParseError::generic(
                                ErrMetadata::new(state.head_loc),
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
                    ImmOrLabelRef::LabelRef(tgt_label) => ok_vec(
                        PartialInst::new_two_reg_needs_label(*inst_new, rs1, rs2, tgt_label),
                    ),
                }
            }
            Jal => {
                let mut args = state.consume_unbounded_commasep_args()?;
                let argc = args.len();
                match argc {
                    1 => {
                        // "jal label"
                        let last_arg = Self::try_parse_imm_or_label_ref(state, 20, args.remove(0))?;
                        match last_arg {
                            ImmOrLabelRef::Imm(imm) => ok_wrap_concr(JalPseudo::expand(imm)),
                            ImmOrLabelRef::LabelRef(tgt_label) => ok_vec(
                                PartialInst::new_no_reg_needs_label(JalPseudo::expand, tgt_label),
                            ),
                        }
                    }
                    2 => {
                        // "jal ra label"
                        let rd = state.try_parse_reg(args.remove(0))?;
                        // J-type has 20-bit immediate
                        let last_arg = Self::try_parse_imm_or_label_ref(state, 20, args.remove(0))?;
                        match last_arg {
                            ImmOrLabelRef::Imm(imm) => ok_wrap_concr(isa::Jal::new(rd, imm)),
                            ImmOrLabelRef::LabelRef(tgt_label) => ok_vec(
                                PartialInst::new_one_reg_needs_label(isa::Jal::new, rd, tgt_label),
                            ),
                        }
                    }
                    _ => Err(ParseError::wrong_diff_argc(
                        ErrMetadata::new(state.head_loc),
                        state.inst_name,
                        1,
                        2,
                        argc as u8,
                    )),
                }
            }
            Jalr => {
                let mut args = state.consume_unbounded_commasep_args()?;
                let argc = args.len();
                match argc {
                    1 => {
                        // "jalr rs"
                        let rs = state.try_parse_reg(args.remove(0))?;
                        ok_wrap_concr(JalrPseudo::expand(rs))
                    }
                    3 => {
                        // "jalr rd, rs, imm"
                        let rd = state.try_parse_reg(args.remove(0))?;
                        let rs1 = state.try_parse_reg(args.remove(0))?;
                        let imm = state.try_parse_imm(12, args.remove(0))?;
                        ok_wrap_concr(isa::Jalr::new(rd, rs1, imm))
                    }
                    _ => Err(ParseError::wrong_diff_argc(
                        ErrMetadata::new(state.head_loc),
                        state.inst_name,
                        1,
                        3,
                        argc as u8,
                    )),
                }
            }
            U(inst_new) => {
                let mut args = state.consume_commasep_args(2)?;
                let rd = state.try_parse_reg(args.remove(0))?;
                let imm = state.try_parse_imm(20, args.remove(0))?;
                ok_wrap_concr(inst_new(rd, imm))
            }
            La => {
                let mut args = state.consume_commasep_args(2)?;
                let rd = state.try_parse_reg(args.remove(0))?;
                let last_arg = Self::try_parse_imm_or_label_ref(state, 32, args.remove(0))?;
                match last_arg {
                    ImmOrLabelRef::Imm(imm) => Ok(vec![
                        PartialInst::new_complete(isa::La::expand_upper(rd, imm)),
                        PartialInst::new_complete(isa::La::expand_lower(
                            rd,
                            // Since the AUIPC is emitted immediately before the ADDI, the offset
                            // of the ADDI must be adjusted by the size of one instruction to
                            // because the pseudo instruction expansion will automatically add 4
                            imm + RegValue::<S>::from(-4i64),
                        )),
                    ]),
                    ImmOrLabelRef::LabelRef(tgt_label) => Ok(vec![
                        PartialInst::new_one_reg_needs_label(
                            isa::La::expand_upper,
                            rd,
                            tgt_label.clone(),
                        ),
                        PartialInst::new_one_reg_needs_label(isa::La::expand_lower, rd, tgt_label),
                    ]),
                }
            }
            Li(inst_expand) => {
                let mut args = state.consume_commasep_args(2)?;
                let rd = state.try_parse_reg(args.remove(0))?;
                let imm = state.try_parse_imm(32, args.remove(0))?;
                ok_wrap_expanded(inst_expand(rd, imm))
            }
            NoArgs(inst_expand) => {
                let _args = state.consume_commasep_args(0)?;
                ok_wrap_concr(inst_expand())
            }
            RegReg(inst_expand) => {
                let mut args = state.consume_commasep_args(2)?;
                let rd = state.try_parse_reg(args.remove(0))?;
                let rs = state.try_parse_reg(args.remove(0))?;
                ok_wrap_concr(inst_expand(rd, rs))
            }
            LikeJ(inst_expand) => {
                let mut args = state.consume_commasep_args(1)?;
                // j expands to J-type, so 20-bit immediate
                let last_arg = Self::try_parse_imm_or_label_ref(state, 20, args.remove(0))?;
                match last_arg {
                    ImmOrLabelRef::Imm(imm) => ok_wrap_concr(inst_expand(imm)),
                    ImmOrLabelRef::LabelRef(tgt_label) => {
                        ok_vec(PartialInst::new_no_reg_needs_label(*inst_expand, tgt_label))
                    }
                }
            }
            OneReg(inst_expand) => {
                let mut args = state.consume_commasep_args(1)?;
                let rs = state.try_parse_reg(args.remove(0))?;
                ok_wrap_concr(inst_expand(rs))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        super::{isa::*, registers::RiscVRegister::*},
        *,
    };
    use crate::{
        assembler::parser::tests::*, data_structures::DataLword, instruction::ConcreteInst,
    };

    #[test]
    /// Tests the loading of immediates in the .data section.
    fn test_data_directives() {
        let prog = ".section .data\n.word 0xdeadbeef\n.byte 0x12";
        let ParseResult {
            reporter,
            sections,
            insts,
            ..
        } = Parser::<Rv32>::parse_str(0, prog);
        assert!(reporter.is_empty());
        assert!(insts.is_empty());
        assert_eq!(sections.data(), vec![0xef, 0xbe, 0xad, 0xde, 0x12]);
    }

    #[test]
    fn test_data_directives_bad() {
        let programs = [
            ".data 1",                     // requires .byte or similar
            ".section .data\n.byte 0x123", // immediate too large
        ];
        for prog in &programs {
            let ParseResult { reporter, .. } = Parser::<Rv32>::parse_str(0, prog);
            assert!(!reporter.is_empty());
        }
    }

    #[test]
    /// Tests parsing of a label in the middle and a label at the end.
    fn test_label_defs() {
        let insts =
            parse_and_lex::<Rv32>("add a0, sp, fp\nl1: addi sp, sp, -4\naddi sp, sp, 4\nl2:");
        let expected_concrete: [RiscVInst<W32b>; 3] = [
            Add::new(A0, Sp, S0),
            Addi::new(Sp, Sp, DataLword::from(-4)),
            Addi::new(Sp, Sp, DataLword::from(4)),
        ];
        assert_eq!(insts.len(), 3);
        assert_eq!(insts[0].label, None);
        assert_eq!(
            insts[1].label,
            Some(LabelDef::new(
                "l1".to_string(),
                Location {
                    file_id: 0,
                    lineno: 1,
                    offs: 0
                }
            ))
        );
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
        let insts = parse_and_lex::<Rv32>("bne x0, x0, l1\nl1: jal ra, end\nend: nop");
        assert_eq!(insts.len(), 3);
        assert_eq!(
            insts[0].get_needed_label().unwrap().target,
            "l1".to_string()
        );
        assert_eq!(
            insts[1].get_needed_label().unwrap().target,
            "end".to_string()
        );
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
            let ParseResult { reporter, .. } = Parser::<Rv32>::parse_str(0, inst);
            assert!(!reporter.is_empty());
        }
    }

    #[test]
    fn test_r_type_parse() {
        let insts = parse_and_lex_concr::<Rv32>("add x5, sp, fp");
        assert_eq!(insts.len(), 1);
        assert_eq!(
            insts[0],
            Add::new(RiscVRegister::from(5), Sp, RiscVRegister::Fp)
        );
    }

    #[test]
    fn test_i_arith_parse() {
        // lack of commas is deliberate
        let insts = parse_and_lex_concr::<Rv32>("addi sp sp -4");
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0], Addi::new(Sp, Sp, DataLword::from(-4)));
    }

    #[test]
    fn test_lui_parse() {
        let insts = parse_and_lex_concr::<Rv32>("lui a0, 0xD_EADC");
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0], Lui::new(A0, DataLword::from(0xD_EADC)));
    }

    #[test]
    fn test_imm_too_big() {
        // immediates for instructions like addi can only be 12 bits long
        let ParseResult {
            insts, reporter, ..
        } = Parser::<Rv32>::parse_str(0, "addi sp sp 0xF000");
        assert!(!reporter.is_empty());
        assert!(insts.is_empty());
    }

    #[test]
    fn test_pseudo_li() {
        let insts = parse_and_lex_concr::<Rv32>("li a0, 0xDEAD_BEEF");
        assert_eq!(insts, Li32::expand(A0, DataLword::from(0xDEAD_BEEFu32)));
    }
}
