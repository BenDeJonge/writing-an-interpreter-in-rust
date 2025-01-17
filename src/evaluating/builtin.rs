use crate::lexing::ast::Identifier;

use super::{error::EvaluationError, evaluator::Evaluation, object::Object};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum BuiltIn {
    Len,
}

const BUILTIN_NAME_LEN: &str = "len";

const BUILTIN_LEN: Object = Object::BuiltIn(BuiltIn::Len);

impl BuiltIn {
    pub fn lookup(ident: &Identifier) -> Option<Object> {
        match ident.0.as_str() {
            BUILTIN_NAME_LEN => Some(BUILTIN_LEN),
            // TODO: support other built-in functions.
            _ => None,
        }
    }

    pub fn apply(&self, args: &[Object]) -> Evaluation {
        match self {
            Self::Len => self.builtin_len(args),
        }
    }

    fn check_argument_count(
        &self,
        expected: usize,
        args: &[Object],
    ) -> Result<(), EvaluationError> {
        if expected != args.len() {
            return Err(EvaluationError::IncorrectArgumentCount(
                expected,
                args.len(),
            ));
        }
        Ok(())
    }

    fn builtin_len(&self, args: &[Object]) -> Evaluation {
        self.check_argument_count(1, args)?;
        match &args[0] {
            Object::String(s) => Ok(Object::Integer(s.len() as isize)),
            _ => Err(EvaluationError::UnsupportedArgument(0, args[0].clone())),
        }
    }
}

impl std::fmt::Display for BuiltIn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Len => write!(f, "{BUILTIN_NAME_LEN}"),
        }
    }
}
