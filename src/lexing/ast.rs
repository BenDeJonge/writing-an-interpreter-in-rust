use std::{
    collections::BTreeMap,
    fmt::Display,
    hash::Hash,
    ops::{Deref, DerefMut},
};

use super::token::Token;

// -----------------------------------------------------------------------------
// N O D E
// -----------------------------------------------------------------------------
/// An additional layer of inderection to write a singular `Display` trait that
/// wraps all possible outputs of a `Parser`.
#[derive(Debug)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Program(program) => write!(f, "{}", format_block_statement(program)),
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
#[derive(Debug, PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub struct Identifier(pub String);

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for Identifier {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// -----------------------------------------------------------------------------
// L I T E R A L
// -----------------------------------------------------------------------------
/// A `Literal` is a non-reserved (non-keyword) token that represents a literal
/// value. In practice, this means either an integer, a bool, a string or an array.
#[derive(Debug, Eq, PartialEq, Clone, PartialOrd, Ord, Hash)]
pub enum Literal {
    Integer(isize),
    Bool(bool),
    String(String),
    Array(Vec<Expression>),
    // A Binary Tree Map is a sorted map. As every key has to implement Ord by
    // definition, a `BTreeMap` has a deterministic order while a `HashMap` does
    // not. As a result, equal keys will have equal hashes, as required by the
    // `Hash` trait.
    // https://doc.rust-lang.org/std/hash/trait.Hash.html#hash-and-eq
    Hash(BTreeMap<Expression, Expression>),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{i}"),
            Literal::Bool(b) => write!(f, "{b}"),
            Literal::String(s) => write!(f, "\"{s}\""),
            Literal::Array(v) => write!(f, "[{}]", format_function_arguments(v)),
            Literal::Hash(b) => write!(f, "{{{}}}", format_btreemap(b)),
        }
    }
}

// -----------------------------------------------------------------------------
// S T A T E M E N T
// -----------------------------------------------------------------------------
/// A `Statement` is a combination of tokens that does not produces a value.
/// - `let x = 5` is a statement.
/// - `return 5;` is a statement.
#[derive(Debug, Eq, PartialEq, Clone, Hash, PartialOrd, Ord)]
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
#[derive(Debug, Eq, PartialEq, Clone, PartialOrd, Ord, Hash)]
pub enum Expression {
    Ident(Identifier),
    Literal(Literal),
    // A self-referential (recursive) type has infinite size. By putting the
    // second `Expression` in a `Box`, it becomes allocated on the heap instead.
    // This breaks the cycle and allows such types.
    /// `Prefix(operation: Token, expression: Box<Expression>)`
    Prefix(Token, Box<Expression>),
    /// An infix expression in Polish notation: a + b => + a b
    /// (https://en.wikipedia.org/wiki/Polish_notation):
    /// `Infix(operation: Token, left: Box<Expression>, right: Box<Expression>)`
    Infix(Token, Box<Expression>, Box<Expression>),
    /// A conditional if-else statement:
    /// `if (condition) {  consequence } else { alternative }`
    ///
    /// `
    /// Conditional(
    ///     condition: Box<Expression>,
    ///     consequence: BlockStatement,
    ///     alternative: Option<BlockStatement>
    /// )
    /// `
    Conditional(Box<Expression>, BlockStatement, Option<BlockStatement>),
    /// `FunctionLiteral(arguments: Vec<Identifier>, body: BlockStatement)`
    FunctionLiteral(Vec<Identifier>, BlockStatement),
    /// A call expression of a built-in or user defined function.
    /// `FunctionCall(name: Box<Expression>, arguments: Vec<Expression>)`
    FunctionCall(Box<Expression>, Vec<Expression>),
    /// `Index(container: Box<Expression>, index: Box<Expression>)`
    Index(Box<Expression>, Box<Expression>),
    /// `HashPair(key: Box<Expression>, value: Box<Expression>)`
    HashPair(Box<Expression>, Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Ident(s) => write!(f, "{s}"),
            Expression::Literal(l) => write!(f, "{l}"),
            Expression::Prefix(operation, expression) => write!(f, "({operation}{expression})"),
            Expression::Infix(operation, left, right) => write!(f, "({left} {operation} {right})"),
            Expression::Conditional(condition, consequence, alternative) => {
                if let Some(alt) = alternative {
                    write!(
                        f,
                        "(if {{ {} }} {{ {} }} else {{ {} }}",
                        condition,
                        format_block_statement(consequence),
                        format_block_statement(alt)
                    )
                } else {
                    write!(
                        f,
                        "(if {{ {} }} {{ {} }}",
                        condition,
                        format_block_statement(consequence),
                    )
                }
            }
            Expression::FunctionLiteral(arguments, body) => {
                write!(
                    f,
                    "fn({}) {{ {} }}",
                    format_function_arguments(arguments),
                    format_block_statement(body)
                )
            }
            Expression::FunctionCall(name, arguments) => {
                write!(f, "{}({})", name, format_function_arguments(arguments))
            }
            Expression::Index(container, index) => write!(f, "({container}[{index}])"),
            Expression::HashPair(key, value) => write!(f, "({key}: {value})"),
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
    Index = 7,          // my_array[i]
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
            Token::LBracket => Self::Index,
            _ => Self::Lowest,
        }
    }
}

// -----------------------------------------------------------------------------
// S T R I N G   F O R M A T T I N G
// -----------------------------------------------------------------------------
pub fn format_helper<T: ToString>(vector: impl Iterator<Item = T>, sep: &str) -> String {
    vector
        .map(|el| el.to_string())
        .collect::<Vec<String>>()
        .join(sep)
}

#[derive(Debug, Default)]
pub struct Program(Vec<Statement>);

impl Deref for Program {
    type Target = Vec<Statement>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Program {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub type BlockStatement = Vec<Statement>;
pub type FunctionArguments = Vec<Identifier>;

/// Join with a separator `""`.
pub fn format_block_statement<T: ToString + Display>(block: &[T]) -> String {
    format_helper(block.iter(), "")
}

/// Join with a separator `", "`.
pub fn format_function_arguments<T: ToString + Display>(arguments: &[T]) -> String {
    format_helper(arguments.iter(), ", ")
}

pub fn format_btreemap<K: ToString + Display, V: ToString + Display>(
    map: &BTreeMap<K, V>,
) -> String {
    format_helper(map.iter().map(|(k, v)| format!("{k}: {v}")), ", ")
}
