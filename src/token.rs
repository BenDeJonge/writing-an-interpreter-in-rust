use std::path::Display;

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
pub const TOKEN_EQUAL: &str = "==";
pub const TOKEN_NOTEQUAL: &str = "!=";
pub const TOKEN_FUNCTION: &str = "fn";
pub const TOKEN_RETURN: &str = "return";
pub const TOKEN_LET: &str = "let";
pub const TOKEN_IF: &str = "if";
pub const TOKEN_ELSE: &str = "else";
pub const TOKEN_TRUE: &str = "true";
pub const TOKEN_FALSE: &str = "false";
pub const TOKENS_CHAR: [char; 14] = [
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
];
pub const TOKENS_STR: &[&str; 9] = &[
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

#[derive(Debug, PartialEq)]
pub struct Token {
    pub type_: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(type_: TokenType, literal: impl ToString) -> Self {
        Token {
            type_,
            literal: literal.to_string(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    EOF,
    // Identifiers and literals
    Ident, // add, foobar, x, y ...
    Int,   // 1234567890
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
    True,     // true
    False,    // false
}

impl TokenType {
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
    pub fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            TOKEN_FUNCTION => TokenType::Function,
            TOKEN_RETURN => TokenType::Return,
            TOKEN_LET => TokenType::Let,
            TOKEN_IF => TokenType::If,
            TOKEN_ELSE => TokenType::Else,
            TOKEN_TRUE => TokenType::True,
            TOKEN_FALSE => TokenType::False,
            _ => TokenType::Ident,
        }
    }
}

// #[derive(Debug, PartialEq, Eq)]
// struct ParseTokenTypeError;

// impl FromStr for TokenType {
//     type Err = ParseTokenTypeError;

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         match s {
//             "" => Ok(TokenType::EOF),
//             // Operators
//             "=" => Ok(TokenType::Assign),
//             "+" => Ok(TokenType::Plus),
//             // Delimiters
//             "," => Ok(TokenType::Comma),
//             ";" => Ok(TokenType::Semicolon),
//             "(" => Ok(TokenType::LParen),
//             ")" => Ok(TokenType::RParen),
//             "{" => Ok(TokenType::LBrace),
//             "}" => Ok(TokenType::RBrace),
//             // Keywords
//             "fn" => Ok(TokenType::Function),
//             "let" => Ok(TokenType::Let),
//             _ => {
//                 // Identifiers and literals
//                 if s.parse::<usize>().is_ok() {
//                     return Ok(TokenType::Int);
//                 } else {
//                     todo!()
//                 }
//             }
//         }
//     }
// }
