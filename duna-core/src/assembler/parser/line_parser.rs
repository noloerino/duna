use super::{DirectiveParser, InstParser, *};
use crate::{arch::*, assembler::ErrMetadata};

/// Responsible for parsing a line.
pub struct LineParser<'a, A: Architecture> {
    data: &'a _ParserData<A>,
    iter: TokenIter,
    label: Option<LabelDef>,
    state: &'a mut ParseState,
}

impl<'a, A> LineParser<'a, A>
where
    A: Architecture,
{
    /// Creates a LineParser, with a label possibly inherited from the previous line.
    pub fn new(
        data: &'a _ParserData<A>,
        tokens: TokenStream,
        maybe_label: &'a Option<LabelDef>,
        state: &'a mut ParseState,
    ) -> Self {
        let mut iter = tokens.into_iter().peekable();
        let label_passed_in = maybe_label.is_some();
        // Check the first token for a label
        let this_label = match maybe_label {
            Some(l) => Some(l.clone()),
            None => {
                if let Some(first_tok) = iter.peek() {
                    if let TokenType::LabelDef(label_name) = &first_tok.data {
                        Some(LabelDef::new(label_name.to_string(), first_tok.location))
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
            iter: TokenIter(iter),
            label: this_label,
            state,
        }
    }

    pub fn parse(mut self) -> (Option<LabelDef>, LineParseResult<A::Family, A::DataWidth>) {
        (
            self.label,
            if let Some(head_tok) = self.iter.next() {
                let errloc = ErrMetadata::new(&head_tok.location);
                use TokenType::*;
                match head_tok.data {
                    Name(name) => {
                        if self.state.curr_section == ProgramSection::Text {
                            A::InstParser::try_expand_inst(InstParseState::new(
                                self.data,
                                self.iter,
                                &head_tok.location,
                                &name,
                            ))
                            .map(OkParseResult::Insts)
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
                    LabelDef(label_name) => Err(ParseError::unimplemented(
                        errloc,
                        &format!(
                            "(found label {}) multiple labels on the same line",
                            label_name
                        ),
                    )),
                    Directive(section_name) => DirectiveParser::new(
                        self.iter,
                        &mut self.state,
                        &head_tok.location,
                        &section_name,
                    )
                    .parse()
                    .map(|option| {
                        if let Some(literals) = option {
                            OkParseResult::Literals(literals)
                        } else {
                            OkParseResult::None
                        }
                    }),
                    Comment(..) => Ok(OkParseResult::None), // deliberate no-op
                    Comma => Err(ParseError::bad_head(errloc, ",")),
                    Immediate(n, style) => Err(ParseError::bad_head(errloc, &style.format(n))),
                    StringLiteral(s) => Err(ParseError::bad_head(errloc, &s)),
                    LParen => Err(ParseError::bad_head(errloc, "(")),
                    RParen => Err(ParseError::bad_head(errloc, ")")),
                }
            } else {
                Ok(OkParseResult::None)
            },
        )
    }
}
