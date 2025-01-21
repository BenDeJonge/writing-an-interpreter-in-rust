use std::ops::{Deref, DerefMut};

use crate::lexing::{ast::format_helper, token::Token};

#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    /// `UnexpectedToken(expected: Token, received: Token)`
    UnexpectedToken(Token, Token),
    /// `MissingIdent(instead: Token)`
    MissingIdent(Token),
    /// `UnknownExpressionToken(token: Token)`
    InvalidExpressionToken(Token),
    IntegerTooLarge(String),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken(expected, received) => {
                write!(
                    f,
                    "UnexpectedToken: got `{received}` instead of `{expected}`.",
                )
            }
            Self::MissingIdent(instead) => write!(f, "MissingIdent: got `{instead}` instead."),
            Self::InvalidExpressionToken(token) => write!(
                f,
                "InvalidExpressionToken: cannot construct expression for `{token}`."
            ),
            Self::IntegerTooLarge(number_str) => write!(
                f,
                "IntegerTooLarge: integers are limited between [{}, {}] received {}",
                // FIXME: integers are read number first and then the optional
                // minus is applied. This causes isize::MIN to overflow.
                isize::MIN + 1,
                isize::MAX,
                number_str
            ),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseErrors(pub Vec<ParseError>);

impl Deref for ParseErrors {
    type Target = Vec<ParseError>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ParseErrors {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::fmt::Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const SEP: &str = "\n\t- ";
        write!(f, "ParseErrors:{}{}", SEP, format_helper(self.iter(), SEP))
    }
}
