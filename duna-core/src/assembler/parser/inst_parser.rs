use super::*;
use crate::data_structures::*;

pub struct InstParseState<'a, F: ArchFamily<S>, S: DataWidth, ParseType: 'static> {
    pub data: &'a ParserData<F::Register, ParseType>,
    pub iter: TokenIter,
    pub head_loc: &'a Location,
    pub inst_name: &'a str,
}

impl<'a, F, S, ParseType> InstParseState<'a, F, S, ParseType>
where
    F: ArchFamily<S>,
    S: DataWidth,
    ParseType: 'static,
{
    pub fn new(
        data: &'a ParserData<F::Register, ParseType>,
        iter: TokenIter,
        head_loc: &'a Location,
        inst_name: &'a str,
    ) -> Self {
        Self {
            data,
            iter,
            head_loc,
            inst_name,
        }
    }

    pub fn check_no_more_args(&mut self, needed: u8) -> Result<(), ParseError> {
        self.iter.check_no_more_args(self.inst_name, needed)
    }

    pub fn try_next_tok(&mut self, needed_args: u8, found_so_far: u8) -> Result<Token, ParseError> {
        self.iter
            .try_next_tok(self.head_loc, self.inst_name, needed_args, found_so_far)
    }

    pub fn consume_unbounded_commasep_args(&mut self) -> Result<Vec<Token>, ParseError> {
        self.iter.consume_unbounded_commasep_args()
    }

    /// Attempts to consume exactly N arguments from the iterator, possibly comma-separated.
    /// The first and last tokens cannot be commas. If repeated commas appear anywhere,
    /// an error is returned.
    /// The only tokens that may appear during this consumption are commas, names, and immediates.
    pub fn consume_commasep_args(&mut self, n: u8) -> Result<Vec<Token>, ParseError> {
        use TokenType::*;
        let mut found = Vec::<Token>::new();
        for left in (0..n).rev() {
            match self.iter.next() {
                Some(tok) => match tok.data {
                    // It might make more semantic sense to lex directives as names instead
                    // but we need to stll be able to treat them as labels
                    Name(..) | Immediate(..) | Directive(..) => {
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
                            ErrMetadata::new(&tok.location),
                            &format!("{:?}", tok.data),
                        ))
                    }
                },
                None => {
                    return Err(ParseError::wrong_argc(
                        ErrMetadata::new(&self.head_loc),
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

    /// Attempts to peek the next token of the iterator, returning a ParseError if there are none.
    pub fn try_peek_tok(
        &mut self,
        needed_args: u8,
        found_so_far: u8,
    ) -> Result<&Token, ParseError> {
        if let Some(tok) = self.iter.peek() {
            Ok(tok)
        } else {
            Err(ParseError::wrong_argc(
                ErrMetadata::new(&self.head_loc),
                self.inst_name,
                needed_args,
                found_so_far,
            ))
        }
    }

    pub fn try_parse_reg(&self, token: Token) -> Result<F::Register, ParseError> {
        match &token.data {
            TokenType::Name(name) => {
                self.data
                    .reg_expansion_table
                    .get(name)
                    .cloned()
                    .ok_or_else(|| {
                        ParseError::unexpected_type(
                            ErrMetadata::new(&token.location),
                            "register name",
                            token.data,
                        )
                    })
            }
            _ => Err(ParseError::unexpected_type(
                ErrMetadata::new(&token.location),
                "register name",
                token.data,
            )),
        }
    }

    /// Parses an immediate that is required to be at most n bits.
    /// If the provided immediate is a negative, then the upper (64 - n + 1) bits must all be 1.
    pub fn try_parse_imm(&self, n: u8, token: Token) -> Result<RegValue<S>, ParseError> {
        try_parse_imm(n, token).map(|res_i64| res_i64.into())
    }
}

pub trait InstParser<F: ArchFamily<S>, S: DataWidth> {
    type ParseType: 'static;

    fn inst_expansion_table() -> &'static HashMap<String, Self::ParseType>;
    fn reg_expansion_table() -> &'static HashMap<String, F::Register>;

    fn try_expand_found_inst(
        state: InstParseState<'_, F, S, Self::ParseType>,
        parse_type: &Self::ParseType,
    ) -> InstParseResult<F, S>;

    fn try_expand_inst(state: InstParseState<'_, F, S, Self::ParseType>) -> InstParseResult<F, S> {
        if let Some(parse_type) = state.data.inst_expansion_table.get(state.inst_name) {
            Self::try_expand_found_inst(state, parse_type)
        } else {
            Err(ParseError::bad_inst_name(
                ErrMetadata::new(&state.head_loc),
                state.inst_name,
            ))
        }
    }
}
