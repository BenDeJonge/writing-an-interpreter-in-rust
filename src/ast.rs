// A tuple struct, to give something a name without naming all fields.
#[derive(Debug, PartialEq)]
pub struct Ident(pub String);

#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(Ident),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement(Ident, Expression),
    ReturnStatement(Expression),
}

pub type Program = Vec<Statement>;
