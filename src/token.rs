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
pub const _TOKENS_CHAR: [char; 14] = [
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

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Eof,
    // Identifiers and literals
    Illegal(String),
    Ident(String), // add, foobar, x, y ...
    Int(isize),    // 1234567890
    Bool(bool),    // true, false
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
            TOKEN_TRUE => Token::True,
            TOKEN_FALSE => Token::False,
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
