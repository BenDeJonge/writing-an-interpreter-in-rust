use crate::lexing::ast::{
    format_block_statement, format_function_arguments, format_helper, BlockStatement,
    FunctionArguments,
};

use super::{builtin::BuiltIn, environment::Env, evaluator::Evaluation};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(isize),
    Bool(bool),
    String(String),
    Null,
    // `Vec<Object>` already adds the required indirection, just like `Box`.
    Array(Vec<Object>),
    ReturnValue(Box<Object>),
    /// `Function(arguments: FunctionArguments, body: BlockStatement, env: Env)`
    Function(FunctionArguments, BlockStatement, Env),
    BuiltIn(BuiltIn),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Primitive data types.
            Self::Integer(i) => write!(f, "{i}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, "\"{s}\""),
            Self::Null => write!(f, "null"),
            // Compound data types.
            Self::Array(vector) => write!(f, "[{}]", format_helper(vector, ", ")),
            // Function-related data types.
            Self::ReturnValue(object) => write!(f, "{object}"),
            Self::Function(arguments, body, _) => {
                write!(
                    f,
                    "fn({}) {{ {} }}",
                    format_function_arguments(arguments),
                    format_block_statement(body)
                )
            }
            Self::BuiltIn(builtin) => write!(f, "{builtin}"),
        }
    }
}

impl From<isize> for Object {
    fn from(value: isize) -> Self {
        Object::Integer(value)
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        if value {
            OBJECT_TRUE
        } else {
            OBJECT_FALSE
        }
    }
}

impl From<&str> for Object {
    fn from(value: &str) -> Self {
        Object::String(value.to_string())
    }
}

impl<T: Into<Object>> From<Vec<T>> for Object {
    fn from(value: Vec<T>) -> Self {
        Object::Array(Vec::from_iter(value.into_iter().map(|v| v.into())))
    }
}

impl<T: Into<Object>> From<Option<T>> for Object {
    fn from(value: Option<T>) -> Self {
        value.map(|v| v.into()).unwrap_or(OBJECT_NULL)
    }
}

pub const TYPE_BOOL: &str = "BOOLEAN";
pub const TYPE_INTEGER: &str = "INTEGER";
pub const TYPE_STRING: &str = "STRING";
pub const TYPE_NULL: &str = "NULL";
pub const TYPE_ARRAY: &str = "ARRAY";
pub const TYPE_FUNCTION: &str = "FUNCTION";
pub const TYPE_BUILTIN: &str = "BUILTIN";

impl Object {
    pub fn get_type(&self) -> &str {
        match self {
            // Primitive data types.
            Self::Bool(_) => TYPE_BOOL,
            Self::Integer(_) => TYPE_INTEGER,
            Self::String(_) => TYPE_STRING,
            Self::Null => TYPE_NULL,
            // Compound data types.
            Self::Array(_) => TYPE_ARRAY,
            // Function-related data types.
            Self::ReturnValue(object) => object.get_type(),
            Self::Function(_, _, _) => TYPE_FUNCTION,
            Self::BuiltIn(_) => TYPE_BUILTIN,
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

impl<T: Into<Object>> IntoEval for T {
    fn into_eval(self) -> Evaluation
    where
        Self: Sized,
    {
        Ok(self.into())
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
