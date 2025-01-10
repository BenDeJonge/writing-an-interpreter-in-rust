use std::fmt::Display;

use crate::token::Token;

// An additional layer of inderection to write a singular `Display` trait that
// wraps all possible outputs of a `Parser`.
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Program(program) => write!(f, "{}", format_program(program)),
            Self::Statement(statement) => write!(f, "{statement}"),
            Self::Expression(expression) => write!(f, "{expression}"),
        }
    }
}

// A tuple struct, to give something a name without naming all fields.
#[derive(Debug, PartialEq)]
pub struct Ident(pub String);

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(isize),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{i}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(String),
    Literal(Literal),
    // A self-referential (recursive) type has infinite size. By putting the
    // second `Expression` in a `Box`, it becomes allocated on the heap instead.
    // This breaks the cycle and allows such types.
    Prefix(Token, Box<Expression>),
    // Infix...
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Ident(s) => write!(f, "{s}"),
            Expression::Literal(l) => write!(f, "{l}"),
            Expression::Prefix(operation, expression) => write!(f, "({operation}{expression})"),
        }
    }
}

/// Relative precedence of expressions.
/// The actual values do not matter but their ordering does.
pub enum ExpressionPrecedence {
    Lowest = 0,
    Equal = 1,       // ==
    LessGreater = 2, // > or <
    Sum = 3,         // +
    Product = 4,     // *
    Prefix = 5,      // -X or !X
    Call = 6,        // my_function(X)
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Ident, Expression),
    Return(Expression),
    Expression(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(id, expr) => write!(f, "let {id} = {expr};"),
            Statement::Return(expr) => write!(f, "return {expr}"),
            Statement::Expression(expr) => write!(f, "{expr}"),
        }
    }
}

pub type Program = Vec<Statement>;

fn format_program(program: &Program) -> String {
    program
        .iter()
        .map(|statement| statement.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}
