use super::arch::Mips;
use crate::{
    assembler::{lexer::*, parser::*},
    data_structures::*,
};
use std::marker::PhantomData;

pub struct MipsParser<S: DataWidth> {
    // file_id: FileId,
    // lines: LineTokenStream,
    // reporter: ParseErrorReporter,
    // state: ParseState,
    _phantom: PhantomData<S>,
}

impl<S: AtLeast32b> Parser<Mips<S>, S> for MipsParser<S> {
    fn parse_lex_result(_: LexResult<'_>) -> ParseResult<Mips<S>, S> {
        todo!()
    }
}
