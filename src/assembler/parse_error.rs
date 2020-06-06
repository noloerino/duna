use super::lexer::{ImmRenderType, LineContents, Location, TokenType};
use std::fmt;

pub struct ParseErrorReport {
    /// Assume the errors are sorted by location
    pub errs: Vec<ParseError>,
}

impl ParseErrorReport {
    /// Prints errors.
    pub fn report(&self) {
        println!("{:?}", self)
    }

    pub fn is_empty(&self) -> bool {
        self.errs.is_empty()
    }

    pub fn get_errs(&self) -> &[ParseError] {
        self.errs.as_slice()
    }
}

impl fmt::Debug for ParseErrorReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for err in &self.errs {
            writeln!(f, "{}", err)?;
            let Location {
                file_name,
                lineno,
                offs,
            } = &err.errloc.location;
            // TODO fix spacing if lineno is more than one digit
            writeln!(f, " --> {}:{}", file_name, err.errloc.location)?;
            writeln!(f, "  |")?;
            writeln!(f, "{} | {}", lineno, err.errloc.line_contents.content)?;
            write!(f, "  |")?;
            // throw informational caret in
            // +1 because there's one space between the pipe and the string in the line above
            for _ in 0..=(*offs) {
                write!(f, " ")?;
            }
            writeln!(f, "^\n")?;
        }
        if self.errs.is_empty() {
            Ok(())
        } else {
            write!(
                f,
                "error: aborting due to {} previous errors",
                self.errs.len()
            )
        }
    }
}

/// Reports parse-time errors
#[derive(Debug)]
pub struct ParseErrorReporter {
    pub errs: Vec<ParseError>,
}

impl ParseErrorReporter {
    pub fn new() -> ParseErrorReporter {
        ParseErrorReporter { errs: Vec::new() }
    }

    pub fn add_error(&mut self, err: ParseError) {
        self.errs.push(err);
    }

    pub fn is_empty(&self) -> bool {
        self.errs.is_empty()
    }

    #[cfg(test)]
    pub fn get_errs(&self) -> &[ParseError] {
        self.errs.as_slice()
    }

    /// Consumes the other reporter.
    pub fn merge(&mut self, mut other: ParseErrorReporter) {
        self.errs.append(&mut other.errs);
    }

    pub fn into_report(self) -> ParseErrorReport {
        ParseErrorReport { errs: self.errs }
    }
}

#[derive(Eq, PartialEq, Debug)]
/// Encodes different kinds of parsing and lexing errors.
enum ParseErrorType {
    /// A catch-all for any kind of error I was too lazy to add a type for.
    Generic(String),
    /// Indicates that a feature is currently unimplemented.
    Unimplemented(String),
    /// A bad character was encountered while lexing an integer literal.
    BadIntLiteral(ImmRenderType, String),
    /// An unrecognized escape character was provided.
    BadEscape(char),
    /// A string literal didn't have a closing quote.
    UnclosedStringLiteral,
    /// The line should have started with a different kind of token.
    BadFirstToken(String),
    /// An instruction name was expected, and the given string was not one.
    ExpectedInstName(String),
    /// A register or immediate was expected, and something else was given instead.
    ExpectedRegOrImm(String),
    /// The wrong number of arguments was provided.
    WrongArgc {
        inst_name: String,
        needed: u8,
        got: u8,
    },
    /// Two possible argcs were allowed, neither of which was provided.
    WrongDiffArgc {
        inst_name: String,
        allowed_1: u8,
        allowed_2: u8,
        got: u8,
    },
    /// Too many arguments were provided.
    /// Use this instead of WrongArgc in situations where the entire argument stream is not consumed,
    /// so we wouldn't know how many arguments were actually passed.
    TooManyArgs { inst_name: String, needed: u8 },
    /// The wrong number of arguments was provided (more generic version of NotEnoughArgs and TooManyArgs)
    /// The provided immediate has too many bits.
    ImmTooBig { max_bit_len: u8, imm_str: String },
    /// A token of type identified by exp_name was expected, but a different TokenType was provided.
    UnexpectedType { exp_name: String, got: TokenType },
    /// A parentheses was left unclosed, and the provided token was found instead.
    UnclosedParen(TokenType),
    /// An unsupported directive was found.
    UnsupportedDirective(String),
    /// A label was referenced without declaration in its file, meaning label has no local definition
    /// and no .global declaration.
    UndeclaredLabel(String),
    /// A label was defined in multiple locations.
    RedefinedLabel(String),
    /// A referenced label was not defined by any file.
    UndefinedLabel(String),
}

impl fmt::Display for ParseErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ImmRenderType::*;
        use ParseErrorType::*;
        match self {
            Generic(msg) => write!(f, "{}", msg),
            Unimplemented(msg) => write!(f, "{} is unimplemented", msg),
            BadIntLiteral(render_type, n) => write!(
                f,
                "encountered unexpected character while parsing {} integer literal {}",
                match render_type {
                    Bin => "binary",
                    Hex => "hexademical",
                    Dec => "decimal",
                },
                *n
            ),
            BadEscape(c) => write!(f, "encountered illegal escape sequence: \\{}", c),
            UnclosedStringLiteral => write!(f, "found unclosed string literal"),
            BadFirstToken(got) => write!(
                f,
                "expected label, section, or instruction, instead got {}",
                got
            ),
            ExpectedInstName(got) => write!(f, "expected instruction name, got {}", got),
            ExpectedRegOrImm(got) => write!(
                f,
                "expected register name or immediate, instead got {}",
                got
            ),
            WrongArgc {
                inst_name,
                needed,
                got,
            } => write!(
                f,
                "instruction {} needed {} arguments, got {}",
                inst_name, needed, got
            ),
            WrongDiffArgc {
                inst_name,
                allowed_1,
                allowed_2,
                got,
            } => write!(
                f,
                "instruction {} needed either {} or {} arguments, got {}",
                inst_name, allowed_1, allowed_2, got
            ),
            TooManyArgs { inst_name, needed } => write!(
                f,
                "instruction {} got too many arguments (needed {})",
                inst_name, needed
            ),
            ImmTooBig {
                max_bit_len,
                imm_str,
            } => write!(
                f,
                "provided immediate {} exceeded maximum length of {} bits",
                imm_str, max_bit_len
            ),
            UnexpectedType { exp_name, got } => write!(f, "expected {}, got {}", exp_name, got),
            UnclosedParen(got) => write!(f, "expected closing parentheses, got {}", got),
            UnsupportedDirective(got) => write!(f, "unsupported assembler directive {}", got),
            // TODO hint at .globl
            UndeclaredLabel(label) => write!(
                f,
                "label {} was neither defined locally nor declared global",
                label
            ),
            // TODO hint at previous definition
            RedefinedLabel(label) => write!(f, "multiple definitions found for label {}", label),
            UndefinedLabel(label) => write!(f, "label {} was declared but never defined", label),
        }
    }
}

#[derive(Eq, PartialEq, Hash, Debug)]
pub struct ErrMetadata {
    location: Location,
    line_contents: LineContents,
}

impl ErrMetadata {
    pub fn new(location: &Location, line_contents: &LineContents) -> ErrMetadata {
        ErrMetadata {
            location: location.clone(),
            line_contents: line_contents.clone(),
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct ParseError {
    errloc: ErrMetadata,
    tpe: ParseErrorType,
}

impl ParseError {
    fn new(location: ErrMetadata, tpe: ParseErrorType) -> Self {
        ParseError {
            errloc: location,
            tpe,
        }
    }

    pub fn generic(location: ErrMetadata, msg: &str) -> Self {
        ParseError::new(location, ParseErrorType::Generic(msg.to_string()))
    }

    pub fn unimplemented(location: ErrMetadata, msg: &str) -> Self {
        ParseError::new(location, ParseErrorType::Unimplemented(msg.to_string()))
    }
}

// functions for errors encountered by lexer
impl ParseError {
    pub fn bad_int_literal(location: ErrMetadata, render_type: ImmRenderType, n: String) -> Self {
        ParseError::new(location, ParseErrorType::BadIntLiteral(render_type, n))
    }

    pub fn bad_escape(location: ErrMetadata, escaped: char) -> Self {
        ParseError::new(location, ParseErrorType::BadEscape(escaped))
    }

    pub fn unclosed_string_literal(location: ErrMetadata) -> Self {
        ParseError::new(location, ParseErrorType::UnclosedStringLiteral)
    }
}

// functions for errors encountered by parser
impl ParseError {
    pub fn bad_head(location: ErrMetadata, got: &str) -> Self {
        ParseError::new(location, ParseErrorType::BadFirstToken(got.to_string()))
    }

    pub fn bad_inst_name(location: ErrMetadata, got: &str) -> Self {
        ParseError::new(location, ParseErrorType::ExpectedInstName(got.to_string()))
    }

    pub fn bad_arg(location: ErrMetadata, got: &str) -> Self {
        ParseError::new(location, ParseErrorType::ExpectedRegOrImm(got.to_string()))
    }

    pub fn wrong_argc(location: ErrMetadata, inst_name: &str, needed: u8, got: u8) -> Self {
        ParseError::new(
            location,
            ParseErrorType::WrongArgc {
                inst_name: inst_name.to_string(),
                needed,
                got,
            },
        )
    }

    pub fn wrong_diff_argc(
        location: ErrMetadata,
        inst_name: &str,
        allowed_1: u8,
        allowed_2: u8,
        got: u8,
    ) -> Self {
        ParseError::new(
            location,
            ParseErrorType::WrongDiffArgc {
                inst_name: inst_name.to_string(),
                allowed_1,
                allowed_2,
                got,
            },
        )
    }

    pub fn too_many_args(location: ErrMetadata, inst_name: &str, needed: u8) -> Self {
        ParseError::new(
            location,
            ParseErrorType::TooManyArgs {
                inst_name: inst_name.to_string(),
                needed,
            },
        )
    }

    pub fn imm_too_big(location: ErrMetadata, max_bit_len: u8, imm_str: &str) -> Self {
        ParseError::new(
            location,
            ParseErrorType::ImmTooBig {
                max_bit_len,
                imm_str: imm_str.to_string(),
            },
        )
    }

    pub fn unexpected_type(location: ErrMetadata, exp_name: &str, got: TokenType) -> Self {
        ParseError::new(
            location,
            ParseErrorType::UnexpectedType {
                exp_name: exp_name.to_string(),
                got,
            },
        )
    }

    pub fn unclosed_paren(location: ErrMetadata, got: TokenType) -> Self {
        ParseError::new(location, ParseErrorType::UnclosedParen(got))
    }

    pub fn unsupported_directive(location: ErrMetadata, got: &str) -> Self {
        ParseError::new(
            location,
            ParseErrorType::UnsupportedDirective(got.to_string()),
        )
    }
}

// functions for errors encountered by assembler/linker
impl ParseError {
    pub fn undeclared_label(location: ErrMetadata, label: &str) -> Self {
        ParseError::new(location, ParseErrorType::UndeclaredLabel(label.to_string()))
    }

    pub fn redefined_label(location: ErrMetadata, label: &str) -> Self {
        ParseError::new(location, ParseErrorType::RedefinedLabel(label.to_string()))
    }

    pub fn undefined_label(location: ErrMetadata, label: &str) -> Self {
        ParseError::new(location, ParseErrorType::UndefinedLabel(label.to_string()))
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error: {}", self.tpe)
    }
}

#[cfg(test)]
mod tests {
    use super::super::parser::RiscVParser;

    #[test]
    /// Tests that an error produced by the lexer makes it so the affected line is not passed
    /// to the parser.
    fn test_lex_short_circuit() {
        let reporter = RiscVParser::parse_str("test", "addi x1 0xggg1, x2").reporter;
        assert_eq!(reporter.errs.len(), 1);
    }
}
