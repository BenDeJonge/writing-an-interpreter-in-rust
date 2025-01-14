use crate::lexing::token::{
    Token, TOKEN_ASSIGN, TOKEN_ASTERISK, TOKEN_BANG, TOKEN_COMMA, TOKEN_GT, TOKEN_LBRACE,
    TOKEN_LPAREN, TOKEN_LT, TOKEN_MINUS, TOKEN_PLUS, TOKEN_RBRACE, TOKEN_RPAREN, TOKEN_SEMICOLON,
    TOKEN_SLASH,
};

#[derive(Debug)]
pub struct Lexer {
    input: Vec<char>,
    /// Current position in the input (points to current char)
    position: usize,
    /// Current reading position in the input (after current char)
    next_position: usize,
    /// Current char under examination
    ch: Option<char>,
}

impl Lexer {
    // -------------------------------------------------------------------------
    // P U B L I C   M E T H O D S
    // -------------------------------------------------------------------------
    /// Create a new `Lexer` for a UTF-8 `String`.
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.chars().collect(),
            // Setting both at the start to prepare for the read_char() call.
            position: 0,
            next_position: 0,
            ch: None,
        };
        lexer.read_char();
        lexer
    }

    /// Get the next `Token` from the input.
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let t = match self.ch {
            // Read past the end.
            None => Token::Eof,
            // Still reading characters.
            Some(c) => match c {
                // Token can be = or ==.
                TOKEN_ASSIGN => {
                    if self.peek_equals(&TOKEN_ASSIGN.to_string()) {
                        Token::Equal
                    } else {
                        Token::Assign
                    }
                }
                // Token can be ! or !=.
                TOKEN_BANG => {
                    if self.peek_equals(&TOKEN_ASSIGN.to_string()) {
                        Token::NotEqual
                    } else {
                        Token::Bang
                    }
                }
                // Operators
                TOKEN_PLUS => Token::Plus,
                TOKEN_MINUS => Token::Minus,
                TOKEN_SLASH => Token::Slash,
                TOKEN_ASTERISK => Token::Asterisk,
                TOKEN_LT => Token::LessThan,
                TOKEN_GT => Token::GreaterThan,
                // Delimiters
                TOKEN_COMMA => Token::Comma,
                TOKEN_SEMICOLON => Token::Semicolon,
                TOKEN_LPAREN => Token::LParen,
                TOKEN_RPAREN => Token::RParen,
                TOKEN_LBRACE => Token::LBrace,
                TOKEN_RBRACE => Token::RBrace,
                // Keywords, identifiers and literals.
                c if Self::is_letter(c) => {
                    let ident = self.read_identifier();
                    Token::lookup_ident(&ident)
                }
                // Integers
                c if Self::is_digit(c) => {
                    let number_str = self.read_number();
                    if let Ok(number) = number_str.parse::<isize>() {
                        Token::Int(number)
                    } else {
                        Token::Illegal(number_str)
                    }
                }
                // Any other character is illegal.
                _ => Token::Illegal(c.to_string()),
            },
        };
        self.read_char();
        t
    }
    // -------------------------------------------------------------------------
    // P R I V A T E   M E T H O D S
    // -------------------------------------------------------------------------
    fn read_char(&mut self) {
        // TODO: this only supports UTF-8, not Unicode.
        // When reading past the EOF, self.ch = None.
        self.ch = self.peek(1);
        if self.ch.is_some() {
            // We have succesfully read, advance both position pointers.
            self.position = self.next_position;
            self.next_position += 1;
        }
    }

    fn peek(&self, n: usize) -> Option<char> {
        self.input.get(self.next_position + n - 1).copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            if ch.is_ascii_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    /// Continue reading as long as some condition applies to `self.ch`.
    fn read_while(&mut self, condition: &dyn Fn(Option<char>) -> bool) -> String {
        let position = self.position;
        while condition(self.peek(1)) {
            self.read_char();
        }
        self.input[position..=self.position]
            .iter()
            .collect::<String>()
    }

    fn is_letter(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn read_identifier(&mut self) -> String {
        self.read_while(&|ch: Option<char>| ch.is_some() && Self::is_letter(ch.unwrap()))
    }

    fn is_digit(c: char) -> bool {
        c.is_numeric()
    }

    fn read_number(&mut self) -> String {
        self.read_while(&|ch: Option<char>| ch.is_some() && Self::is_digit(ch.unwrap()))
    }

    fn peek_equals(&mut self, s: &str) -> bool {
        let mut matches = true;
        // All peeks match.
        for (i, wanted) in s.chars().enumerate() {
            if let Some(ch) = self.peek(1 + i) {
                if ch != wanted {
                    matches = false;
                    break;
                }
            }
        }
        if matches {
            for _ in 0..s.len() {
                self.read_char();
            }
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::lexing::token::Token;

    #[test]
    fn test_next_token_simple() {
        let input = "=+(){},;";
        let expected = [
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];
        let mut lexer = Lexer::new(input);
        for token_type in expected {
            assert_eq!(token_type, lexer.next_token())
        }
    }
    #[test]
    fn test_next_token() {
        let input = "let five = 5;
        let ten = 10;
        
        let add = fn(x, y) {
          x + y
        };
        
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        
        10 == 10;
        10 != 9;";
        let expected = [
            // let five = 5;
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            // let ten = 10;
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            // let add = fn(x, y) {x + y};
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::RBrace,
            Token::Semicolon,
            // let result = add(five, ten);
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            // !-/*5;
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            // 5 < 10 > 5;";
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::GreaterThan,
            Token::Int(5),
            Token::Semicolon,
            // if (5 < 10) {
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            //     return true;
            Token::Return,
            Token::Bool(true),
            Token::Semicolon,
            // } else {
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            //     return false;
            Token::Return,
            Token::Bool(false),
            Token::Semicolon,
            // }
            Token::RBrace,
            // 10 == 10;
            Token::Int(10),
            Token::Equal,
            Token::Int(10),
            Token::Semicolon,
            // 10 != 9;
            Token::Int(10),
            Token::NotEqual,
            Token::Int(9),
            Token::Semicolon,
            // End of file
            Token::Eof,
        ];
        let mut lexer = Lexer::new(input);
        for token in expected {
            assert_eq!(token, lexer.next_token())
        }
    }
}
