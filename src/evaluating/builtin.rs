use crate::lexing::ast::Identifier;

use super::{
    error::EvaluationError,
    evaluator::Evaluation,
    object::{Object, OBJECT_NULL},
};

#[derive(Debug, PartialEq, Clone, Eq, Hash, PartialOrd, Ord)]
pub enum BuiltIn {
    Len,
    First,
    Last,
    Rest,
    Push,
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
            BuiltIn::Rest => BuiltInParams {
                builtin: Object::BuiltIn(BuiltIn::Rest),
                name: BUILTIN_NAME_LAST,
                n_args: 1,
            },
            BuiltIn::Push => BuiltInParams {
                builtin: Object::BuiltIn(BuiltIn::Push),
                name: BUILTIN_NAME_PUSH,
                n_args: 2,
            },
        }
    }
}

const BUILTIN_NAME_LEN: &str = "len";
const BUILTIN_NAME_FIRST: &str = "first";
const BUILTIN_NAME_LAST: &str = "last";
const BUILTIN_NAME_REST: &str = "rest";
const BUILTIN_NAME_PUSH: &str = "push";

const BUILTIN_LEN: BuiltInParams = BuiltInParams::new(BuiltIn::Len);
const BUILTIN_FIRST: BuiltInParams = BuiltInParams::new(BuiltIn::First);
const BUILTIN_LAST: BuiltInParams = BuiltInParams::new(BuiltIn::Last);
const BUILTIN_REST: BuiltInParams = BuiltInParams::new(BuiltIn::Rest);
const BUILTIN_PUSH: BuiltInParams = BuiltInParams::new(BuiltIn::Push);

impl BuiltIn {
    pub fn get_params(&self) -> BuiltInParams {
        match self {
            Self::Len => BUILTIN_LEN,
            Self::First => BUILTIN_FIRST,
            Self::Last => BUILTIN_LAST,
            Self::Rest => BUILTIN_REST,
            Self::Push => BUILTIN_PUSH,
        }
    }

    pub fn lookup(ident: &Identifier) -> Option<Object> {
        match ident.0.as_str() {
            BUILTIN_NAME_LEN => Some(BUILTIN_LEN.builtin),
            BUILTIN_NAME_FIRST => Some(BUILTIN_FIRST.builtin),
            BUILTIN_NAME_LAST => Some(BUILTIN_LAST.builtin),
            BUILTIN_NAME_REST => Some(BUILTIN_REST.builtin),
            BUILTIN_NAME_PUSH => Some(BUILTIN_PUSH.builtin),
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
            Self::Rest => self.builtin_rest(args),
            Self::Push => self.builtin_push(args),
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

    // -------------------------------------------------------------------------
    // I M P L E M E N T A T I O N S
    // -------------------------------------------------------------------------
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

    fn builtin_rest(&self, args: &[Object]) -> Evaluation {
        match &args[0] {
            Object::String(s) => {
                if s.is_empty() {
                    Ok(OBJECT_NULL)
                } else if s.len() == 1 {
                    Ok(Object::String(String::default()))
                } else {
                    Ok(Object::String(s[1..].to_owned()))
                }
            }
            Object::Array(a) => {
                if a.is_empty() {
                    Ok(OBJECT_NULL)
                } else if a.len() == 1 {
                    Ok(Object::Array(Vec::default()))
                } else {
                    Ok(Object::Array(a[1..].to_owned()))
                }
            }
            _ => Err(EvaluationError::UnsupportedArgument(0, args[0].clone())),
        }
    }

    fn builtin_push(&self, args: &[Object]) -> Evaluation {
        match &args[0..2] {
            [Object::String(base), Object::String(ext)] => {
                let mut s = base.clone();
                s.push_str(ext);
                Ok(Object::String(s))
            }
            [Object::Array(base), ext] => {
                let mut a = base.clone();
                a.push(ext.clone());
                Ok(Object::Array(a))
            }
            [Object::String(_), ext] => Err(EvaluationError::UnsupportedArgument(1, ext.clone())),
            [obj, _] => Err(EvaluationError::UnsupportedArgument(0, obj.clone())),
            _ => unreachable!("covered in check_argument_count()"),
        }
    }
}

impl std::fmt::Display for BuiltIn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_params().name)
    }
}
