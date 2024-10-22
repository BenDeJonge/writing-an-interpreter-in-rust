use crate::token::{
    Token, TokenType, TOKEN_ASSIGN, TOKEN_ASTERISK, TOKEN_BANG, TOKEN_COMMA, TOKEN_EQUAL, TOKEN_GT,
    TOKEN_LBRACE, TOKEN_LPAREN, TOKEN_LT, TOKEN_MINUS, TOKEN_NOTEQUAL, TOKEN_PLUS, TOKEN_RBRACE,
    TOKEN_RPAREN, TOKEN_SEMICOLON, TOKEN_SLASH,
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
    pub fn next(&mut self) -> Token {
        self.skip_whitespace();
        let t = match self.ch {
            // Read past the end.
            None => Token::new(TokenType::EOF, ""),
            // Still reading characters.
            Some(c) => match c {
                // Token can be = or ==.
                TOKEN_ASSIGN => self
                    .try_read_multichar_token(Token::new(TokenType::Equal, TOKEN_EQUAL))
                    .unwrap_or(Token::new(TokenType::Assign, c)),
                // Token can be ! or !=.
                TOKEN_BANG => self
                    .try_read_multichar_token(Token::new(TokenType::NotEqual, TOKEN_NOTEQUAL))
                    .unwrap_or(Token::new(TokenType::Bang, c)),
                // Operators
                TOKEN_PLUS => Token::new(TokenType::Plus, c),
                TOKEN_MINUS => Token::new(TokenType::Minus, c),
                TOKEN_SLASH => Token::new(TokenType::Slash, c),
                TOKEN_ASTERISK => Token::new(TokenType::Asterisk, c),
                TOKEN_LT => Token::new(TokenType::LessThan, c),
                TOKEN_GT => Token::new(TokenType::GreaterThan, c),
                // Delimiters
                TOKEN_COMMA => Token::new(TokenType::Comma, c),
                TOKEN_SEMICOLON => Token::new(TokenType::Semicolon, c),
                TOKEN_LPAREN => Token::new(TokenType::LParen, c),
                TOKEN_RPAREN => Token::new(TokenType::RParen, c),
                TOKEN_LBRACE => Token::new(TokenType::LBrace, c),
                TOKEN_RBRACE => Token::new(TokenType::RBrace, c),
                // Keywords, identifiers and literals.
                c if Self::is_letter(c) => {
                    let ident = self.read_identifier();
                    Token::new(TokenType::lookup_ident(&ident), ident)
                }
                // Integers
                c if Self::is_digit(c) => Token::new(TokenType::Int, self.read_number()),
                // Any other character is illegal.
                _ => Token::new(TokenType::Illegal, c),
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

    fn try_read_multichar_token(&mut self, token: Token) -> Result<Token, &str> {
        let mut token_chars = token.literal.chars();
        // First one matches.
        let mut matches = self.ch == token_chars.next();
        // All peeks match.
        if matches {
            for (i, wanted) in token_chars.enumerate() {
                if let Some(ch) = self.peek(1 + i) {
                    if ch != wanted {
                        matches = false;
                        break;
                    }
                }
            }
        }
        if matches {
            for _ in 0..token.literal.len() {
                self.read_char();
            }
            Ok(token)
        } else {
            Err("could not parse multichar token")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_next_token_simple() {
        let input = "=+(){},;";
        let expected = [
            TokenType::Assign,
            TokenType::Plus,
            TokenType::LParen,
            TokenType::RParen,
            TokenType::LBrace,
            TokenType::RBrace,
            TokenType::Comma,
            TokenType::Semicolon,
            TokenType::EOF,
        ];
        let mut lexer = Lexer::new(input);
        for token_type in expected {
            assert_eq!(token_type, lexer.next().type_)
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
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "five"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::Semicolon, ";"),
            // let ten = 10;
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "ten"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::Semicolon, ";"),
            // let add = fn(x, y) {x + y};
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "add"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Function, "fn"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Ident, "x"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Ident, "y"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::LBrace, "{"),
            Token::new(TokenType::Ident, "x"),
            Token::new(TokenType::Plus, "+"),
            Token::new(TokenType::Ident, "y"),
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Semicolon, ";"),
            // let result = add(five, ten);
            Token::new(TokenType::Let, "let"),
            Token::new(TokenType::Ident, "result"),
            Token::new(TokenType::Assign, "="),
            Token::new(TokenType::Ident, "add"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Ident, "five"),
            Token::new(TokenType::Comma, ","),
            Token::new(TokenType::Ident, "ten"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::Semicolon, ";"),
            // !-/*5;
            Token::new(TokenType::Bang, "!"),
            Token::new(TokenType::Minus, "-"),
            Token::new(TokenType::Slash, "/"),
            Token::new(TokenType::Asterisk, "*"),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::Semicolon, ";"),
            // 5 < 10 > 5;";
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::LessThan, "<"),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::GreaterThan, ">"),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::Semicolon, ";"),
            // if (5 < 10) {
            Token::new(TokenType::If, "if"),
            Token::new(TokenType::LParen, "("),
            Token::new(TokenType::Int, "5"),
            Token::new(TokenType::LessThan, "<"),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::RParen, ")"),
            Token::new(TokenType::LBrace, "{"),
            //     return true;
            Token::new(TokenType::Return, "return"),
            Token::new(TokenType::True, "true"),
            Token::new(TokenType::Semicolon, ";"),
            // } else {
            Token::new(TokenType::RBrace, "}"),
            Token::new(TokenType::Else, "else"),
            Token::new(TokenType::LBrace, "{"),
            //     return false;
            Token::new(TokenType::Return, "return"),
            Token::new(TokenType::False, "false"),
            Token::new(TokenType::Semicolon, ";"),
            // }
            Token::new(TokenType::RBrace, "}"),
            // 10 == 10;
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::Equal, "=="),
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::Semicolon, ";"),
            // 10 != 9;
            Token::new(TokenType::Int, "10"),
            Token::new(TokenType::NotEqual, "!="),
            Token::new(TokenType::Int, "9"),
            Token::new(TokenType::Semicolon, ";"),
            // End of file
            Token::new(TokenType::EOF, ""),
        ];
        let mut lexer = Lexer::new(input);
        for token in expected {
            assert_eq!(token, lexer.next())
        }
    }
}
