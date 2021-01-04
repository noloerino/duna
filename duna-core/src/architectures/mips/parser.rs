use super::{arch::Mips, registers::MipsRegister};
use crate::{assembler::parser::*, data_structures::*};
use std::{collections::HashMap, marker::PhantomData};

pub struct MipsInstParser<S: DataWidth> {
    // file_id: FileId,
    // lines: LineTokenStream,
    // reporter: ParseErrorReporter,
    // state: ParseState,
    _phantom: PhantomData<S>,
}

pub enum ParseType {}

type MipsParseState<'a, S> = InstParseState<'a, Mips<S>, S, ParseType>;

impl<S: AtLeast32b> InstParser<Mips<S>, S> for MipsInstParser<S> {
    type ParseType = ParseType;

    fn inst_expansion_table() -> &'static HashMap<String, Self::ParseType> {
        todo!()
    }

    fn reg_expansion_table() -> &'static HashMap<String, MipsRegister> {
        todo!()
    }

    fn try_expand_found_inst<'a>(
        _state: MipsParseState<'a, S>,
        _parse_type: &ParseType,
    ) -> InstParseResult<Mips<S>, S> {
        todo!()
    }
}
