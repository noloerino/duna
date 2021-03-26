use super::{arch::Mips, instruction::*, registers::MipsRegister};
use crate::{assembler::parser::*, data_structures::*};
use std::{collections::HashMap, marker::PhantomData};

pub struct MipsInstParser<S: DataWidth> {
    // file_id: FileId,
    // lines: LineTokenStream,
    // reporter: ParseErrorReporter,
    // state: ParseState,
    _phantom: PhantomData<S>,
}

#[derive(Copy, Clone)]
pub enum ParseType<S: AtLeast32b> {
    R(fn(MipsRegister, MipsRegister, MipsRegister) -> MipsInst<S>),
}

lazy_static! {
    static ref MIPS32_INST_EXPANSION_TABLE: HashMap<String, ParseType<W32b>> = {
        use super::isa::*;
        use ParseType::*;
        [
            // === Base ===
            ("add", R(Add::new)),
        ]
        .iter()
        .cloned()
        .map(|(s, t)| (s.to_string(), t))
        .collect()
    };

    static ref REG_EXPANSION_TABLE: HashMap<String, MipsRegister> = {
        let mut reg_expansion_table: HashMap<String, MipsRegister> = MipsRegister::REG_ARRAY
            .iter()
            .map(|r| (r.to_string(), *r))
            .collect();
        for i in 0..32 {
            reg_expansion_table.insert(format!("r{}", i), MipsRegister::from(i));
        }
        // don't forget FP
        reg_expansion_table.insert("fp".to_string(), MipsRegister::FP);
        reg_expansion_table
    };
}

type MipsInstParseState<'a, S> = InstParseState<'a, Mips<S>, S, ParseType<S>>;

impl InstParser<Mips<W32b>, W32b> for MipsInstParser<W32b> {
    type ParseType = ParseType<W32b>;

    fn inst_expansion_table() -> &'static HashMap<String, Self::ParseType> {
        &MIPS32_INST_EXPANSION_TABLE
    }

    fn reg_expansion_table() -> &'static HashMap<String, MipsRegister> {
        &REG_EXPANSION_TABLE
    }

    fn try_expand_found_inst(
        state: MipsInstParseState<'_, W32b>,
        parse_type: &ParseType<W32b>,
    ) -> InstParseResult<Mips<W32b>, W32b> {
        Self::try_expand_found_inst(state, parse_type)
    }
}

impl<S: AtLeast32b> MipsInstParser<S> {
    fn try_expand_found_inst(
        mut owned_state: MipsInstParseState<'_, S>,
        parse_type: &ParseType<S>,
    ) -> InstParseResult<Mips<S>, S> {
        use ParseType::*;
        let state = &mut owned_state;
        match parse_type {
            R(inst_new) => {
                // R-types are always "inst rd, rs1, rs2" with commas in between
                // TODO consume_commasep currently doesn't require commas in between
                let mut args = state.consume_commasep_args(3)?;
                let rd = state.try_parse_reg(args.remove(0))?;
                let rs1 = state.try_parse_reg(args.remove(0))?;
                let rs2 = state.try_parse_reg(args.remove(0))?;
                ok_wrap_concr(inst_new(rd, rs1, rs2))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        super::{isa::*, registers::MipsRegister::*},
        *,
    };
    use crate::{architectures::mips::arch::Mips32, assembler::parser::tests::*};

    #[test]
    fn test_r_type_parse() {
        let insts = parse_and_lex_concr::<Mips32>("add r5, sp, fp");
        assert_eq!(insts.len(), 1);
        assert_eq!(
            insts[0],
            Add::new(MipsRegister::from(5), SP, MipsRegister::FP)
        );
    }
}
