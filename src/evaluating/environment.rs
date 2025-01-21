use std::collections::BTreeMap;

use crate::lexing::ast::Identifier;

use super::object::Object;

/// The underlying `Environment` object will need to be passed around to various
/// evaluation function calls, that all need to access and perhaps mutate the
/// contents.
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Environment {
    inner: BTreeMap<Identifier, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(inner: BTreeMap<Identifier, Object>, outer: Option<Environment>) -> Self {
        Self {
            inner,
            outer: outer.map(Box::new),
        }
    }

    /// Enclose an environment as the outer scope into a new blank scope.
    pub fn enclose(outer: &Environment) -> Environment {
        Environment {
            inner: Default::default(),
            outer: Some(Box::new(outer.clone())),
        }
    }

    /// Search through the inner scope for an `Identifier`. If it is not found,
    /// recursively try searching the outer scopes.
    pub fn get(&self, k: &Identifier) -> Option<Object> {
        self.inner.get(k).cloned().or_else(|| {
            if let Some(outer) = &self.outer {
                outer.get(k)
            } else {
                None
            }
        })
    }

    /// Add an `Identifier` and its accompanying `Object` to the inner scope.
    pub fn insert(&mut self, k: Identifier, v: Object) -> Option<Object> {
        self.inner.insert(k, v)
    }
}
