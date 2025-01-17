use crate::lexing::{ast::Identifier, token::Token};

use super::object::{IntoEval, Object};

#[derive(Debug, PartialEq)]
pub enum EvaluationError {
    /// `InvalidPrefixOperator(operator: Token, right: Object)`
    InvalidPrefixOperator(Token, Object),
    /// `InvalidInfixOperator(operator: Token, left: Object, right: Object)`
    InvalidInfixOperator(Token, Object, Object),
    /// `UnknownIdentifier(id: Identifier)`
    UnknowIdentifier(Identifier),
    /// `IncorrectArgumentCount(expected: usize, received: usize)`
    IncorrectArgumentCount(usize, usize),
    /// `UnsupportedArgument(i: usize, arg: Object)`
    UnsupportedArgument(usize, Object),
}

impl std::fmt::Display for EvaluationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidPrefixOperator(operator, object) => {
                write!(
                    f,
                    "InvalidPrefixOperation: {}{}",
                    operator,
                    object.get_type()
                )
            }
            Self::InvalidInfixOperator(operator, left, right) => {
                write!(
                    f,
                    "InvalidOperation: {} {} {}",
                    left.get_type(),
                    operator,
                    right.get_type()
                )
            }
            Self::UnknowIdentifier(id) => write!(f, "UnknownIdentifier: {id}"),
            Self::IncorrectArgumentCount(expected, received) => write!(
                f,
                "IncorrectArgumentCount: expected {expected} received {received}"
            ),
            Self::UnsupportedArgument(i, arg) => {
                write!(
                    f,
                    "UnsupportedArgument: received {} for argument {i}",
                    arg.get_type()
                )
            }
        }
    }
}

impl IntoEval for EvaluationError {
    fn into_eval(self) -> super::evaluator::Evaluation
    where
        Self: Sized,
    {
        Err(self)
    }
}
