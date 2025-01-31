use std::collections::BTreeMap;

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
    Puts,
}

pub struct BuiltInParams {
    pub builtin: Object,
    pub name: &'static str,
    pub n_args: Option<usize>,
}

impl BuiltInParams {
    pub const fn new(builtin: BuiltIn) -> Self {
        match builtin {
            BuiltIn::Len => BuiltInParams {
                builtin: Object::BuiltIn(BuiltIn::Len),
                name: BUILTIN_NAME_LEN,
                n_args: Some(1),
            },
            BuiltIn::First => BuiltInParams {
                builtin: Object::BuiltIn(BuiltIn::First),
                name: BUILTIN_NAME_FIRST,
                n_args: Some(1),
            },
            BuiltIn::Last => BuiltInParams {
                builtin: Object::BuiltIn(BuiltIn::Last),
                name: BUILTIN_NAME_LAST,
                n_args: Some(1),
            },
            BuiltIn::Rest => BuiltInParams {
                builtin: Object::BuiltIn(BuiltIn::Rest),
                name: BUILTIN_NAME_LAST,
                n_args: Some(1),
            },
            BuiltIn::Push => BuiltInParams {
                builtin: Object::BuiltIn(BuiltIn::Push),
                name: BUILTIN_NAME_PUSH,
                n_args: Some(2),
            },
            BuiltIn::Puts => BuiltInParams {
                builtin: Object::BuiltIn(BuiltIn::Puts),
                name: BUILTIN_NAME_PUTS,
                n_args: None,
            },
        }
    }
}

const BUILTIN_NAME_LEN: &str = "len";
const BUILTIN_NAME_FIRST: &str = "first";
const BUILTIN_NAME_LAST: &str = "last";
const BUILTIN_NAME_REST: &str = "rest";
const BUILTIN_NAME_PUSH: &str = "push";
const BUILTIN_NAME_PUTS: &str = "puts";

const BUILTIN_LEN: BuiltInParams = BuiltInParams::new(BuiltIn::Len);
const BUILTIN_FIRST: BuiltInParams = BuiltInParams::new(BuiltIn::First);
const BUILTIN_LAST: BuiltInParams = BuiltInParams::new(BuiltIn::Last);
const BUILTIN_REST: BuiltInParams = BuiltInParams::new(BuiltIn::Rest);
const BUILTIN_PUSH: BuiltInParams = BuiltInParams::new(BuiltIn::Push);
const BUILTIN_PUTS: BuiltInParams = BuiltInParams::new(BuiltIn::Puts);

impl BuiltIn {
    pub fn get_params(&self) -> BuiltInParams {
        match self {
            Self::Len => BUILTIN_LEN,
            Self::First => BUILTIN_FIRST,
            Self::Last => BUILTIN_LAST,
            Self::Rest => BUILTIN_REST,
            Self::Push => BUILTIN_PUSH,
            Self::Puts => BUILTIN_PUTS,
        }
    }

    pub fn lookup(ident: &Identifier) -> Option<Object> {
        match ident.0.as_str() {
            BUILTIN_NAME_LEN => Some(BUILTIN_LEN.builtin),
            BUILTIN_NAME_FIRST => Some(BUILTIN_FIRST.builtin),
            BUILTIN_NAME_LAST => Some(BUILTIN_LAST.builtin),
            BUILTIN_NAME_REST => Some(BUILTIN_REST.builtin),
            BUILTIN_NAME_PUSH => Some(BUILTIN_PUSH.builtin),
            BUILTIN_NAME_PUTS => Some(BUILTIN_PUTS.builtin),
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
            Self::Puts => self.builtin_puts(args),
        }
    }

    fn check_argument_count(
        &self,
        expected: Option<usize>,
        args: &[Object],
    ) -> Result<(), EvaluationError> {
        if let Some(n_args) = expected {
            if n_args != args.len() {
                return Err(EvaluationError::IncorrectArgumentCount(n_args, args.len()));
            }
            Ok(())
        } else {
            Ok(())
        }
    }

    // -------------------------------------------------------------------------
    // I M P L E M E N T A T I O N S
    // -------------------------------------------------------------------------
    fn builtin_len(&self, args: &[Object]) -> Evaluation {
        match &args[0] {
            Object::String(s) => Ok(Object::Integer(s.len() as isize)),
            Object::Array(a) => Ok(Object::Integer(a.len() as isize)),
            Object::Hash(m) => Ok(Object::Integer(m.len() as isize)),
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
            // Returns the last key in lexicographic order.
            Object::Hash(m) => Ok(m
                .iter()
                .next()
                .unwrap_or((&OBJECT_NULL, &OBJECT_NULL))
                .0
                .clone()),
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
            // Returns the first key in lexicographic order.
            Object::Hash(m) => Ok(m
                .iter()
                .last()
                .unwrap_or((&OBJECT_NULL, &OBJECT_NULL))
                .0
                .clone()),
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
            Object::Hash(btm) => {
                if btm.is_empty() {
                    Ok(OBJECT_NULL)
                } else if btm.len() == 1 {
                    Ok(Object::Hash(BTreeMap::default()))
                } else {
                    let mut btm_c = btm.clone();
                    btm_c.pop_first();
                    Ok(Object::Hash(btm_c))
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
            [Object::String(_), ext] => Err(EvaluationError::UnsupportedArgument(1, ext.clone())),
            [Object::Array(base), ext] => {
                let mut a = base.clone();
                a.push(ext.clone());
                Ok(Object::Array(a))
            }
            [Object::Hash(btm), Object::Array(kv)] => {
                if kv.len() != 2 {
                    Err(EvaluationError::InvalidKeyValuePair(Object::Array(
                        kv.to_vec(),
                    )))
                } else {
                    let mut btm_c = btm.clone();
                    btm_c.insert(kv[0].clone(), kv[1].clone());
                    Ok(Object::Hash(btm_c))
                }
            }
            [Object::Hash(_), ext] => Err(EvaluationError::UnsupportedArgument(1, ext.clone())),
            [obj, _] => Err(EvaluationError::UnsupportedArgument(0, obj.clone())),
            _ => unreachable!("covered in check_argument_count()"),
        }
    }

    fn builtin_puts(&self, args: &[Object]) -> Evaluation {
        for arg in args {
            println!("{arg}")
        }
        Ok(OBJECT_NULL)
    }
}

impl std::fmt::Display for BuiltIn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_params().name)
    }
}
