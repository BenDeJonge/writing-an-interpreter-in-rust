use std::fmt::Display;

pub const TOKEN_ASSIGN: char = '=';
pub const TOKEN_PLUS: char = '+';
pub const TOKEN_MINUS: char = '-';
pub const TOKEN_SLASH: char = '/';
pub const TOKEN_BANG: char = '!';
pub const TOKEN_ASTERISK: char = '*';
pub const TOKEN_LT: char = '<';
pub const TOKEN_GT: char = '>';
pub const TOKEN_COMMA: char = ',';
pub const TOKEN_SEMICOLON: char = ';';
pub const TOKEN_LPAREN: char = '(';
pub const TOKEN_RPAREN: char = ')';
pub const TOKEN_LBRACE: char = '{';
pub const TOKEN_RBRACE: char = '}';
pub const TOKEN_DOUBLE_QUOTE: char = '"';
pub const _TOKENS_CHAR: [char; 15] = [
    TOKEN_ASSIGN,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_SLASH,
    TOKEN_BANG,
    TOKEN_ASTERISK,
    TOKEN_LT,
    TOKEN_GT,
    TOKEN_COMMA,
    TOKEN_SEMICOLON,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_DOUBLE_QUOTE,
];

pub const TOKEN_EQUAL: &str = "==";
pub const TOKEN_NOTEQUAL: &str = "!=";
pub const TOKEN_FUNCTION: &str = "fn";
pub const TOKEN_RETURN: &str = "return";
pub const TOKEN_LET: &str = "let";
pub const TOKEN_IF: &str = "if";
pub const TOKEN_ELSE: &str = "else";
pub const TOKEN_TRUE: &str = "true";
pub const TOKEN_FALSE: &str = "false";
pub const _TOKENS_STR: &[&str; 9] = &[
    TOKEN_EQUAL,
    TOKEN_NOTEQUAL,
    TOKEN_FUNCTION,
    TOKEN_RETURN,
    TOKEN_LET,
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_TRUE,
    TOKEN_FALSE,
];

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Token {
    Eof,
    // Identifiers and literals
    Illegal(String),
    Ident(String), // add, foobar, x, y ...
    Int(isize),    // 1234567890
    Bool(bool),    // true, false
    String(String),
    // Operators
    Assign,      // =
    Plus,        // +
    Minus,       // -
    Slash,       // /
    Bang,        // !
    Asterisk,    // *
    LessThan,    // <
    GreaterThan, // >
    Equal,       // ==
    NotEqual,    // !=
    // Delimiters
    Comma,     // ,
    Semicolon, // ;
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    // Keywords
    Function, // fn
    Return,   // return
    Let,      // let
    If,       // if
    Else,     // else
}

impl Token {
    /// Parse a literal into either:
    /// - a user-defined IDENT (`TokenType::Ident`)
    /// - a language KEYWORD
    ///     - `TokenType::Function`
    ///     - `TokenType::Return`
    ///     - `TokenType::Let`
    ///     - `TokenType::If`
    ///     - `TokenType::Else`
    ///     - `TokenType::True`
    ///     - `TokenType::False`
    pub fn lookup_ident(ident: &str) -> Token {
        match ident {
            TOKEN_FUNCTION => Token::Function,
            TOKEN_RETURN => Token::Return,
            TOKEN_LET => Token::Let,
            TOKEN_IF => Token::If,
            TOKEN_ELSE => Token::Else,
            TOKEN_TRUE => Token::Bool(true),
            TOKEN_FALSE => Token::Bool(false),
            _ => {
                // A boolean
                if ident == "true" {
                    Token::Bool(true)
                } else if ident == "false" {
                    Token::Bool(false)
                }
                // A parseable isize.
                else if let Ok(int) = ident.parse::<isize>() {
                    Token::Int(int)
                }
                // Anything else.
                else {
                    Token::Ident(ident.to_owned())
                }
            }
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(id) => write!(f, "{id}"),
            Token::Int(i) => write!(f, "{i}"),
            Token::Bool(b) => write!(f, "{b}"),
            Token::Assign => write!(f, "{TOKEN_ASSIGN}"),
            Token::Plus => write!(f, "{TOKEN_PLUS}"),
            Token::Minus => write!(f, "{TOKEN_MINUS}"),
            Token::Bang => write!(f, "{TOKEN_BANG}"),
            Token::Asterisk => write!(f, "{TOKEN_ASTERISK}"),
            Token::Slash => write!(f, "{TOKEN_SLASH}"),
            Token::LessThan => write!(f, "{TOKEN_LT}"),
            Token::GreaterThan => write!(f, "{TOKEN_GT}"),
            Token::Equal => write!(f, "{TOKEN_EQUAL}"),
            Token::NotEqual => write!(f, "{TOKEN_NOTEQUAL}"),
            Token::Comma => write!(f, "{TOKEN_COMMA}"),
            Token::Semicolon => write!(f, "{TOKEN_SEMICOLON}"),
            Token::LParen => write!(f, "{TOKEN_LPAREN}"),
            Token::RParen => write!(f, "{TOKEN_RPAREN}"),
            Token::LBrace => write!(f, "{TOKEN_LBRACE}"),
            Token::RBrace => write!(f, "{TOKEN_RBRACE}"),
            Token::Function => write!(f, "{TOKEN_FUNCTION}"),
            Token::Let => write!(f, "{TOKEN_LET}"),
            Token::Return => write!(f, "{TOKEN_RETURN}"),
            Token::If => write!(f, "{TOKEN_IF}"),
            Token::Else => write!(f, "{TOKEN_ELSE}"),
            _ => write!(f, "{:?}", self),
        }
    }
}
