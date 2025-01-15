use crate::lexing::{ast::Identifier, token::Token};

use super::object::{IntoEval, Object};

#[derive(Debug, PartialEq)]
pub enum EvaluationError {
    TypeMismatch(Object, Object),
    InvalidPrefixOperator(Token, Object),
    InvalidInfixOperator(Token, Object, Object),
    UnknowIdentifier(Identifier),
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
