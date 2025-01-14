use crate::token::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token, Token),
    MissingToken(Token),
    MissingIdent(Token),
    InvalidExpression(Token),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "could not parse")
    }
}

pub type ParseErrors = Vec<ParseError>;
