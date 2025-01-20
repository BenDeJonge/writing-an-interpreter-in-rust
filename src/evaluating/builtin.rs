use crate::lexing::ast::Identifier;

use super::{error::EvaluationError, evaluator::Evaluation, object::Object};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum BuiltIn {
    Len,
    First,
    Last,
}

pub struct BuiltInParams {
    pub builtin: Object,
    pub name: &'static str,
    pub n_args: usize,
}

impl BuiltInParams {
    pub const fn new(builtin: BuiltIn) -> Self {
        match builtin {
            BuiltIn::Len => BuiltInParams {
                builtin: Object::BuiltIn(BuiltIn::Len),
                name: BUILTIN_NAME_LEN,
                n_args: 1,
            },
            BuiltIn::First => BuiltInParams {
                builtin: Object::BuiltIn(BuiltIn::First),
                name: BUILTIN_NAME_FIRST,
                n_args: 1,
            },
            BuiltIn::Last => BuiltInParams {
                builtin: Object::BuiltIn(BuiltIn::Last),
                name: BUILTIN_NAME_LAST,
                n_args: 1,
            },
        }
    }
}

const BUILTIN_NAME_LEN: &str = "len";
const BUILTIN_NAME_FIRST: &str = "first";
const BUILTIN_NAME_LAST: &str = "last";

const BUILTIN_LEN: BuiltInParams = BuiltInParams::new(BuiltIn::Len);
const BUILTIN_FIRST: BuiltInParams = BuiltInParams::new(BuiltIn::First);
const BUILTIN_LAST: BuiltInParams = BuiltInParams::new(BuiltIn::Last);

impl BuiltIn {
    pub fn get_params(&self) -> BuiltInParams {
        match self {
            Self::Len => BUILTIN_LEN,
            Self::First => BUILTIN_FIRST,
            Self::Last => BUILTIN_LAST,
        }
    }

    pub fn lookup(ident: &Identifier) -> Option<Object> {
        match ident.0.as_str() {
            BUILTIN_NAME_LEN => Some(BUILTIN_LEN.builtin),
            BUILTIN_NAME_FIRST => Some(BUILTIN_FIRST.builtin),
            BUILTIN_NAME_LAST => Some(BUILTIN_LAST.builtin),
            // TODO: support other built-in functions.
            _ => None,
        }
    }

    pub fn apply(&self, args: &[Object]) -> Evaluation {
        self.check_argument_count(self.get_params().n_args, args)?;
        match self {
            Self::Len => self.builtin_len(args),
            Self::First => self.builtin_first(args),
            Self::Last => self.builtin_last(args),
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
        match &args[0] {
            Object::String(s) => Ok(Object::Integer(s.len() as isize)),
            Object::Array(a) => Ok(Object::Integer(a.len() as isize)),
            _ => Err(EvaluationError::UnsupportedArgument(0, args[0].clone())),
        }
    }

    fn builtin_first(&self, args: &[Object]) -> Evaluation {
        match &args[0] {
            Object::String(s) => s
                .chars()
                .next()
                .map(|c| Ok(Object::String(c.to_string())))
                .unwrap_or(Err(EvaluationError::IndexOutOfBounds(
                    args[0].clone(),
                    0.into(),
                ))),
            Object::Array(a) => {
                a.first()
                    .map(|o| Ok(o.clone()))
                    .unwrap_or(Err(EvaluationError::IndexOutOfBounds(
                        args[0].clone(),
                        0.into(),
                    )))
            }
            _ => Err(EvaluationError::UnsupportedArgument(0, args[0].clone())),
        }
    }

    fn builtin_last(&self, args: &[Object]) -> Evaluation {
        match &args[0] {
            Object::String(s) => s
                .chars()
                .last()
                .map(|c| Ok(Object::String(c.to_string())))
                .unwrap_or(Err(EvaluationError::IndexOutOfBounds(
                    args[0].clone(),
                    0.into(),
                ))),
            Object::Array(a) => {
                a.last()
                    .map(|o| Ok(o.clone()))
                    .unwrap_or(Err(EvaluationError::IndexOutOfBounds(
                        args[0].clone(),
                        0.into(),
                    )))
            }
            _ => Err(EvaluationError::UnsupportedArgument(0, args[0].clone())),
        }
    }
}

impl std::fmt::Display for BuiltIn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_params().name)
    }
}
