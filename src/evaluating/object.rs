use crate::lexing::ast::{
    format_block_statement, format_function_arguments, BlockStatement, FunctionArguments,
};

use super::evaluator::Evaluation;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(isize),
    Bool(bool),
    Null,
    ReturnValue(Box<Object>),
    /// `Function(arguments: FunctionArguments, body: BlockStatement)`
    Function(FunctionArguments, BlockStatement),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{i}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Null => write!(f, "null"),
            Self::ReturnValue(object) => write!(f, "{object}"),
            Self::Function(arguments, body) => {
                write!(
                    f,
                    "fn({}) {{ {} }}",
                    format_function_arguments(arguments),
                    format_block_statement(body)
                )
            }
        }
    }
}
impl Object {
    pub fn get_type(&self) -> &str {
        match self {
            Self::Bool(_) => "BOOLEAN",
            Self::Integer(_) => "INTEGER",
            Self::Null => "NULLTYPE",
            Self::ReturnValue(object) => object.get_type(),
            Self::Function(_, _) => "FUNCTION",
        }
    }
}

/// A conversion method to go from native Rust types (`isize`, `bool`, `None`)
/// to an `Evaluation`.
pub trait IntoEval {
    fn into_eval(self) -> Evaluation
    where
        Self: Sized;
}

impl IntoEval for isize {
    fn into_eval(self) -> Evaluation
    where
        Self: Sized,
    {
        Ok(Object::Integer(self))
    }
}

impl IntoEval for bool {
    fn into_eval(self) -> Evaluation
    where
        Self: Sized,
    {
        Ok(Object::Bool(self))
    }
}

impl<T> IntoEval for Option<T>
where
    T: IntoEval,
{
    fn into_eval(self) -> Evaluation
    where
        Self: Sized,
    {
        match self {
            Some(t) => t.into_eval(),
            None => Ok(Object::Null),
        }
    }
}

impl IntoEval for Object {
    fn into_eval(self) -> Evaluation
    where
        Self: Sized,
    {
        Ok(self)
    }
}

// There exist only 2 boolean values and a single null value so we can reference
// these instead of constructing them on the fly.
pub const OBJECT_TRUE: Object = Object::Bool(true);
pub const OBJECT_FALSE: Object = Object::Bool(false);
pub const OBJECT_NULL: Object = Object::Null;

pub fn to_boolean_object(b: bool) -> Object {
    if b {
        OBJECT_TRUE
    } else {
        OBJECT_FALSE
    }
}
