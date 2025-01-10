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
    Bool(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{i}"),
            Literal::Bool(b) => write!(f, "{b}"),
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
    // Polish notation: a + b => + a b
    // https://en.wikipedia.org/wiki/Polish_notation
    Infix(Token, Box<Expression>, Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Ident(s) => write!(f, "{s}"),
            Expression::Literal(l) => write!(f, "{l}"),
            Expression::Prefix(operation, expression) => write!(f, "({operation}{expression})"),
            Expression::Infix(operation, left, right) => write!(f, "({left} {operation} {right})"),
        }
    }
}

/// Relative precedence of expressions.
/// The actual values do not matter but their ordering does.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest = 0,         // ...
    EqualNotEqual = 1,  // ==
    LessGreater = 2,    // > or <
    AddSubtract = 3,    // +
    MultiplyDivide = 4, // *
    Prefix = 5,         // -X or !X
    Call = 6,           // my_function(X)
}

impl From<&Token> for Precedence {
    fn from(value: &Token) -> Self {
        match value {
            Token::Equal | Token::NotEqual => Self::EqualNotEqual,
            Token::LessThan | Token::GreaterThan => Self::LessGreater,
            Token::Plus | Token::Minus => Self::AddSubtract,
            Token::Asterisk | Token::Slash => Self::MultiplyDivide,
            // Technically also `Token::Minus` but that is an unreachable branch
            // as it is already used in `Self::AddSubtract`.
            Token::Bang => Self::Prefix,
            Token::LParen => Self::Call,
            _ => Self::Lowest,
        }
    }
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
        .join("")
}
