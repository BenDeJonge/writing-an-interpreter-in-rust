#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(isize),
    Bool(bool),
    Null,
    ReturnValue(Box<Object>),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{i}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Null => write!(f, "null"),
            Self::ReturnValue(object) => write!(f, "{object}"),
        }
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
