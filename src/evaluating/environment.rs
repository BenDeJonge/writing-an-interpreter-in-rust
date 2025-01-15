use std::{
    cell::RefCell,
    collections::HashMap,
    ops::{Deref, DerefMut},
    rc::Rc,
};

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

#[derive(Default)]
pub struct Environment(HashMap<Identifier, Object>);

impl Deref for Environment {
    type Target = HashMap<Identifier, Object>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Environment {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
