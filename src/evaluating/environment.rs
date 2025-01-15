use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::lexing::ast::Identifier;

use super::object::Object;

/// The underlying `Environment` object will need to be passed around to various
/// evaluation function calls, that all need to access and perhaps mutate the
/// contents.
/// - [`Rc`](https://doc.rust-lang.org/book/ch15-04-rc.html) (Refence Counting)
///   enables multiple ownership by keeping track of the number of references.
///   The reference count is incremented (decremented) whenever the `Rc` is
///   cloned (goes out of scope). Note that cloning is a shallow operation.
/// - [`RefCell`](https://doc.rust-lang.org/book/ch15-05-interior-mutability.html)
///   enables mutation of interior data even when there are immutable references
///   around. This is achieved through `unsafe` code in the backend.
pub type Env = Rc<RefCell<Environment>>;

impl From<Environment> for Env {
    fn from(value: Environment) -> Self {
        Rc::new(RefCell::new(value))
    }
}

#[derive(Debug, Default)]
pub struct Environment {
    inner: HashMap<Identifier, Object>,
    outer: Option<Env>,
}

impl Environment {
    /// Enclose an environment as the outer scope into a new blank scope.
    pub fn enclose(outer: &Env) -> Environment {
        Environment {
            inner: Default::default(),
            outer: Some(outer.clone()),
        }
    }

    /// Search through the inner scope for an `Identifier`. If it is not found,
    /// recursively try searching the outer scopes.
    pub fn get(&self, k: &Identifier) -> Option<Object> {
        self.inner.get(k).cloned().or_else(|| {
            if let Some(outer) = &self.outer {
                outer.borrow().get(k)
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
