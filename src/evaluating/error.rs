use crate::{
    evaluating::object::{TYPE_ARRAY, TYPE_INTEGER},
    lexing::{ast::Identifier, token::Token},
};

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
    /// `IncorrectIndexType(array: Object, index: Object)`
    IncorrectIndexType(Object, Object),
    /// `IndexOutOfBounds(array: Object, index: Object)`
    IndexOutOfBounds(Object, Object),
    InvalidKeyValuePair(Object),
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
            Self::IncorrectIndexType(array, index) => {
                write!(
                    f,
                    "IncorrectIndexType: expected ({}, {}) received ({}, {})",
                    TYPE_ARRAY,
                    TYPE_INTEGER,
                    array.get_type(),
                    index.get_type(),
                )
            }
            Self::IndexOutOfBounds(array, index) => match (array, index) {
                (Object::Array(a), Object::Integer(i)) => write!(
                    f,
                    "IndexOutOfBounds: array of length {} cannot be indexed at {}",
                    a.len(),
                    i
                ),
                _ => unreachable!(),
            },
            Self::InvalidKeyValuePair(array) => match array {
                Object::Array(a) => write!(
                    f,
                    "InvalidKeyValuePair: expected array of length 2 received {}",
                    a.len()
                ),
                _ => unreachable!(),
            },
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
