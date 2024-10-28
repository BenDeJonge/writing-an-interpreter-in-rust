use crate::{
    ast::{Expression, Ident},
    token::TOKEN_ASSIGN,
};
use core::fmt;
use std::mem;

use crate::{
    ast::{Program, Statement},
    lexer::Lexer,
    token::Token,
};

#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token, Token),
    MissingToken(Token),
    MissingIdent,
    UnknownToken(Token),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "could not parse")
    }
}

pub type ParseErrors = Vec<ParseError>;

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    next_token: Token,
    errors: ParseErrors,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            // Setting some dummy tokens.
            current_token: Token::Eof,
            next_token: Token::Eof,
            errors: vec![],
        };
        parser.next();
        parser.next();
        parser
    }

    fn next(&mut self) {
        mem::swap(&mut self.current_token, &mut self.next_token);
        self.next_token = self.lexer.next();
    }

    pub fn get_errors(&self) -> ParseErrors {
        self.errors.clone()
    }

    // -------------------------------------------------------------------------
    // P A R S I N G
    // -------------------------------------------------------------------------
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();
        while !self.next_token_is(Token::Eof) {
            match self.parse_statement() {
                Ok(statement) => program.push(statement),
                Err(e) => {
                    // TODO: do not ignore.
                    match e {
                        ParseError::UnknownToken(_) => {}
                        _ => self.errors.push(e),
                    }
                    self.next();
                }
            }
        }
        program
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.current_token {
            Token::Let => self.try_parse_let_statement(),
            Token::Return => self.try_parse_return_statement(),
            _ => Err(ParseError::UnknownToken(self.current_token.clone())),
        }
    }

    // S P E C I F I C   P A R S E R S
    // -------------------------------
    fn try_parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        // let five = 5;
        // Get the `five`.
        let ident = match &self.next_token {
            Token::Ident(s) => s.clone(),
            _ => {
                return Err(ParseError::UnexpectedToken(
                    Token::Ident("".to_string()),
                    self.next_token.clone(),
                ))
            }
        };
        self.next();
        // Get the `=`.
        if !self.expect_next(Token::Assign) {
            // TODO: should raise error?
            return Err(ParseError::UnexpectedToken(
                Token::Assign,
                self.next_token.clone(),
            ));
        };
        // TODO: actually parse instead of skipping expressions until a `;`.
        while !self.current_token_is(Token::Semicolon) {
            self.next()
        }
        let expression = String::new();

        Ok(Statement::LetStatement(
            Ident(ident),
            Expression::Ident(Ident(expression)),
        ))
    }

    fn try_parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        let statement = Statement::ReturnStatement(Expression::Ident(Ident("".to_string())));
        while !self.current_token_is(Token::Semicolon) {
            self.next()
        }
        Ok(statement)
    }

    // H E L P E R   F U N C T I O N S
    // -------------------------------
    fn current_token_is(&self, token: Token) -> bool {
        self.current_token == token
    }

    fn next_token_is(&self, token: Token) -> bool {
        self.next_token == token
    }

    fn expect_next(&mut self, token: Token) -> bool {
        if self.next_token_is(token) {
            self.next();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]

mod tests {
    use crate::{
        ast::{Expression, Ident, Program, Statement},
        lexer::Lexer,
        token::Token,
    };

    use super::{ParseError, Parser};

    #[test]
    fn test_let_statement_ok() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        assert!(!program.is_empty());
        assert_eq!(program.len(), 3);

        let identifiers = [
            Statement::LetStatement(
                Ident(String::from("x")),
                Expression::Ident(Ident(String::from(""))), // TODO: will be parsed in the future
            ),
            Statement::LetStatement(
                Ident(String::from("y")),
                Expression::Ident(Ident(String::from(""))), // TODO: will be parsed in the future
            ),
            Statement::LetStatement(
                Ident(String::from("foobar")),
                Expression::Ident(Ident(String::from(""))), // TODO: will be parsed in the future
            ),
        ];
        for (expected, statement) in identifiers.iter().zip(program.iter()) {
            assert_eq!(expected, statement)
        }
    }

    #[test]
    fn test_let_statement_bad() {
        let input = "
        let x 5;
        let = 10;
        let 838383;
        ";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        assert!(program.is_empty());
        dbg!(&parser.errors);
        assert_eq!(parser.errors.len(), 3);

        let errors = [
            ParseError::UnexpectedToken(Token::Assign, Token::Int(5)),
            ParseError::UnexpectedToken(Token::Ident("".to_string()), Token::Assign),
            ParseError::UnexpectedToken(Token::Ident("".to_string()), Token::Int(838383)),
        ];
        for (expected, error) in errors.iter().zip(parser.errors.iter()) {
            assert_eq!(expected, error)
        }
    }

    fn test_return_statement_ok() {
        let input = "
            return 5;
            return 10;
            return 993322";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert!(!program.is_empty());
        assert!(program.len() == 3);

        let identifiers = [
            Statement::ReturnStatement(Expression::Ident(Ident("".to_string()))),
            Statement::ReturnStatement(Expression::Ident(Ident("".to_string()))),
            Statement::ReturnStatement(Expression::Ident(Ident("".to_string()))),
        ];
        for (expected, statement) in identifiers.iter().zip(program.iter()) {
            assert_eq!(expected, statement)
        }
    }
}
