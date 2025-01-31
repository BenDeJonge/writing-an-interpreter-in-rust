use super::token::{
    Token, TOKEN_ASSIGN, TOKEN_ASTERISK, TOKEN_BANG, TOKEN_COLON, TOKEN_COMMA, TOKEN_DOUBLE_QUOTE,
    TOKEN_GT, TOKEN_LBRACE, TOKEN_LBRACKET, TOKEN_LPAREN, TOKEN_LT, TOKEN_MINUS, TOKEN_PLUS,
    TOKEN_RBRACE, TOKEN_RBRACKET, TOKEN_RPAREN, TOKEN_SEMICOLON, TOKEN_SLASH,
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
                TOKEN_COLON => Token::Colon,
                TOKEN_LPAREN => Token::LParen,
                TOKEN_RPAREN => Token::RParen,
                TOKEN_LBRACE => Token::LBrace,
                TOKEN_RBRACE => Token::RBrace,
                // Keywords, identifiers and literals.
                c if Self::is_letter(c) => {
                    let ident = self.read_identifier();
                    Token::lookup_ident(&ident)
                }
                // Integers.
                c if Self::is_digit(c) => {
                    let number_str = self.read_number();
                    if let Ok(number) = number_str.parse::<isize>() {
                        number.into()
                    } else {
                        Token::IntegerTooLarge(number_str)
                    }
                }
                // Strings.
                TOKEN_DOUBLE_QUOTE => Token::String(self.read_string()),
                // Arrays.
                TOKEN_LBRACKET => Token::LBracket,
                TOKEN_RBRACKET => Token::RBracket,
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

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            if ch.is_ascii_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    // R E A D   I D E N T I F I E R
    // -----------------------------
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

    // R E A D   N U M B E R
    // ---------------------
    fn is_digit(c: char) -> bool {
        c.is_numeric()
    }

    // FIXME: this cannot read negative numbers!
    fn read_number(&mut self) -> String {
        self.read_while(&|ch: Option<char>| ch.is_some() && Self::is_digit(ch.unwrap()))
    }

    // R E A D   S T R I N G
    // ---------------------
    fn read_string(&mut self) -> String {
        let start = self.position + 1;
        // Skip the first `"`.
        self.read_char();
        while self.ch.is_some() && self.ch.unwrap() != TOKEN_DOUBLE_QUOTE {
            self.read_char();
        }
        self.input[start..self.position].iter().collect::<String>()
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::lexing::token::Token;

    fn test_helper(input: &str, expected: &[Token]) {
        if expected.last() != Some(&Token::Eof) {
            panic!("expected does not end with EOF")
        }
        let mut lexer = Lexer::new(input);
        for token in expected {
            assert_eq!(token, &lexer.next_token())
        }
    }

    #[test]
    fn test_next_token_simple() {
        test_helper(
            "=+(){},;",
            &[
                Token::Assign,
                Token::Plus,
                Token::LParen,
                Token::RParen,
                Token::LBrace,
                Token::RBrace,
                Token::Comma,
                Token::Semicolon,
                Token::Eof,
            ],
        );
    }

    #[test]
    fn test_let() {
        test_helper(
            "
            let five = 5;
            let ten = 10;
            
            let add = fn(x, y) {
            x + y
            };
            
            let result = add(five, ten);",
            &[
                // let five = 5;
                Token::Let,
                Token::Ident("five".to_string()),
                Token::Assign,
                5.into(),
                Token::Semicolon,
                // let ten = 10;
                Token::Let,
                Token::Ident("ten".to_string()),
                Token::Assign,
                10.into(),
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
                Token::Eof,
            ],
        );
    }

    #[test]
    fn test_numbers() {
        test_helper(
            "
        !-/*5;
        5 < 10 > 5;
        
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        
        10 == 10;
        10 != 9;",
            &[
                // !-/*5;
                Token::Bang,
                Token::Minus,
                Token::Slash,
                Token::Asterisk,
                5.into(),
                Token::Semicolon,
                // 5 < 10 > 5;";
                5.into(),
                Token::LessThan,
                10.into(),
                Token::GreaterThan,
                5.into(),
                Token::Semicolon,
                // if (5 < 10) {
                Token::If,
                Token::LParen,
                5.into(),
                Token::LessThan,
                10.into(),
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
                10.into(),
                Token::Equal,
                10.into(),
                Token::Semicolon,
                // 10 != 9;
                10.into(),
                Token::NotEqual,
                9.into(),
                Token::Semicolon,
                Token::Eof,
            ],
        );
    }

    #[test]
    fn test_string() {
        test_helper(
            "
        \"\";
        \"foobar\";
        let foobar = \"foo bar\";
        (\"foo bar\");
        \"foo bar\"
        ",
            &[
                // "";
                "".into(),
                Token::Semicolon,
                // "foobar";
                "foobar".into(),
                Token::Semicolon,
                // let foobar = "foo bar";
                Token::Let,
                Token::Ident("foobar".to_string()),
                Token::Assign,
                "foo bar".into(),
                Token::Semicolon,
                // ("foo bar");
                Token::LParen,
                "foo bar".into(),
                Token::RParen,
                Token::Semicolon,
                // "foo bar"
                "foo bar".into(),
                Token::Eof,
            ],
        )
    }

    #[test]
    fn test_array() {
        test_helper(
            "
            [1, 2];
            [\"a\"];
            [\"foobar\", true, 42]
            ",
            &[
                // [1, 2];
                Token::LBracket,
                1.into(),
                Token::Comma,
                2.into(),
                Token::RBracket,
                Token::Semicolon,
                // ["a"];
                Token::LBracket,
                "a".into(),
                Token::RBracket,
                Token::Semicolon,
                // ["foobar", True, 42]
                Token::LBracket,
                "foobar".into(),
                Token::Comma,
                Token::Bool(true),
                Token::Comma,
                42.into(),
                Token::RBracket,
                // EOF
                Token::Eof,
            ],
        );
    }

    #[test]
    fn test_integer() {
        test_helper(
            "
        5
        55
        9999999999999999999999999999999999999999
        -9999999999999999999999999999999999999999
        ",
            &[
                5.into(),
                55.into(),
                Token::IntegerTooLarge("9999999999999999999999999999999999999999".to_string()),
                // FIXME: a very ugly test because integers are read value first and sign second.
                // As a result, isize::MIN is read as isize::MIN.abs(), which is out of bounds.
                Token::Minus,
                Token::IntegerTooLarge("9999999999999999999999999999999999999999".to_string()),
                Token::Eof,
            ],
        );
    }

    #[test]
    fn test_hash() {
        test_helper(
            "{\"foo\": \"bar\", \"baz\": \"quz\"}[\"foo\"]",
            &[
                Token::LBrace,
                "foo".into(),
                Token::Colon,
                "bar".into(),
                Token::Comma,
                "baz".into(),
                Token::Colon,
                "quz".into(),
                Token::RBrace,
                Token::LBracket,
                "foo".into(),
                Token::RBracket,
                Token::Eof,
            ],
        );
    }
}
