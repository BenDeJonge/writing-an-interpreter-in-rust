use crate::lexing::{ast::Identifier, token::Token};

use super::object::{IntoEval, Object};

#[derive(Debug, PartialEq)]
pub enum EvaluationError {
    /// `TypeMismatch(left: Object, right: Object)`
    TypeMismatch(Object, Object),
    /// `InvalidPrefixOperator(operator: Token, right: Object)`
    InvalidPrefixOperator(Token, Object),
    /// `InvalidInfixOperator(operator: Token, left: Object, right: Object)`
    InvalidInfixOperator(Token, Object, Object),
    /// `UnknownIdentifier(id: Identifier)`
    UnknowIdentifier(Identifier),
    /// `IncorrectArgumentCount(expected: usize, received: usize)`
    IncorrectArgumentCount(usize, usize),
}

impl std::fmt::Display for EvaluationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeMismatch(left, right) => {
                write!(f, "TypeMismatch: {}, {}", left.get_type(), right.get_type())
            }
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
