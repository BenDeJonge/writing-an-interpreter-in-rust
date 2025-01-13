use std::fmt::Display;

use crate::token::Token;

// -----------------------------------------------------------------------------
// N O D E
// -----------------------------------------------------------------------------
/// An additional layer of inderection to write a singular `Display` trait that
/// wraps all possible outputs of a `Parser`.
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Program(program) => write!(f, "{}", format_statements(program)),
            Self::Statement(statement) => write!(f, "{statement}"),
            Self::Expression(expression) => write!(f, "{expression}"),
        }
    }
}

// -----------------------------------------------------------------------------
// I D E N T I F I E R
// -----------------------------------------------------------------------------
/// An `Identifier` is a non-reserved (non-keyword) token. In practice, this
/// means either a `Literal` or a variable name. E.g., in `let x = 5;` the `x`
/// is an `Identifier`.
///
/// It is represented in code by a tuple struct of a single `String` (meant to
/// give something a name without naming all fields).
#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// -----------------------------------------------------------------------------
// L I T E R A L
// -----------------------------------------------------------------------------
/// A `Literal` is a non-reserved (non-keyword) token that represents a literal
/// value. In practice, this means either an integer, a bool or a string.
#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(isize),
    Bool(bool),
    // TODO: String(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{i}"),
            Literal::Bool(b) => write!(f, "{b}"),
        }
    }
}

// -----------------------------------------------------------------------------
// S T A T E M E N T
// -----------------------------------------------------------------------------
/// A `Statement` is a combination of tokens that does not produces a value.
/// - `let x = 5` is a statement.
/// - `return 5;` is a statement.
#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Expression(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(id, expr) => write!(f, "let {id} = {expr};"),
            Statement::Return(expr) => write!(f, "return {expr};"),
            Statement::Expression(expr) => write!(f, "{expr}"),
        }
    }
}

// -----------------------------------------------------------------------------
// E X P R E S S I O N
// -----------------------------------------------------------------------------
/// An `Expression` is a special kind of `Statement` that  produces values.
/// - `5` is an expression.
/// - `add(5, 5)` is an expression.
#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(String),
    Literal(Literal),
    // A self-referential (recursive) type has infinite size. By putting the
    // second `Expression` in a `Box`, it becomes allocated on the heap instead.
    // This breaks the cycle and allows such types.
    /// A prefix operation:
    /// `Prefix(operation: Token, expression: Box<Expression>)`.
    Prefix(Token, Box<Expression>),
    /// An infix expression in Polish notation: a + b => + a b
    /// (https://en.wikipedia.org/wiki/Polish_notation):
    /// `Infix(
    ///     operation: Token,
    ///     left: Box<Expression>,
    ///     right: Box<Expression>
    /// )`.
    Infix(Token, Box<Expression>, Box<Expression>),
    /// A conditional if-else statement:
    /// `Conditional(
    ///     condition: Box<Expression>,
    ///     consequence: BlockStatement,
    ///     alternative: Option<BlockStatement>
    /// )`.
    Conditional(Box<Expression>, BlockStatement, Option<BlockStatement>),
    /// A function literal:
    /// `FunctionLiteral(
    ///     arguments: Vec<Ident>,
    ///     body: BlockStatement
    /// )`.
    FunctionLiteral(Vec<Identifier>, BlockStatement),
    /// A call expression of a built-in or user defined function.
    /// `FunctionCall(
    ///     name: Box<Expression>,
    ///     arguments: Vec<Expression>
    /// )`.
    FunctionCall(Box<Expression>, Vec<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Ident(s) => write!(f, "{s}"),
            Expression::Literal(l) => write!(f, "{l}"),
            Expression::Prefix(operation, expression) => write!(f, "({operation}{expression})"),
            Expression::Infix(operation, left, right) => write!(f, "({left} {operation} {right})"),
            Expression::Conditional(condition, consequence, alternative) => {
                // TODO: decide on formatting here.
                if let Some(alt) = alternative {
                    write!(
                        f,
                        "(if ({}) ({}) else ({})",
                        condition,
                        format_statements(consequence),
                        format_statements(alt)
                    )
                } else {
                    write!(
                        f,
                        "(if ({}) ({})",
                        condition,
                        format_statements(consequence),
                    )
                }
            }
            Expression::FunctionLiteral(arguments, body) => {
                write!(
                    f,
                    "fn({}) {{ {} }}",
                    format_arguments(arguments),
                    format_statements(body)
                )
            }
            // TODO: decide formatting
            Expression::FunctionCall(name, arguments) => {
                write!(f, "{}({})", name, format_arguments(arguments))
            }
        }
    }
}

/// Relative precedence of expressions i.e., the required order of parsing.
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

// -----------------------------------------------------------------------------
// S T R I N G   F O R M A T T I N G
// -----------------------------------------------------------------------------
fn format_helper<T: ToString>(vector: &[T], sep: &str) -> String {
    vector
        .iter()
        .map(|el| el.to_string())
        .collect::<Vec<String>>()
        .join(sep)
}

pub type Program = Vec<Statement>;
pub type BlockStatement = Vec<Statement>;
/// Convert a `BlockStatement` or a `Program` into a `String`.
fn format_statements(statements: &[Statement]) -> String {
    format_helper(statements, "")
}

pub type FunctionArguments = Vec<Identifier>;
fn format_arguments<T: ToString>(arguments: &[T]) -> String {
    format_helper(arguments, ", ")
}
