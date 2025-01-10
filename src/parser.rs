use crate::{
    ast::{Expression, ExpressionPrecedence, Ident, Literal, Node},
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
    InvalidExpression,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "could not parse")
    }
}

pub type ParseErrors = Vec<ParseError>;

pub fn parse(input: &str) -> Result<Node, ParseErrors> {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();
    Ok(Node::Program(program))
}

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
        // Advance the parser twice to load `current_token` and `next_token`.
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
        // TODO: check if errors is empty and return a result instead
        program
    }

    /// There only exist two true statement types in Monkey:
    /// - `let`
    /// - `return`
    ///
    ///  If the token points to neither, parse an expression instead.
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.current_token {
            Token::Let => self.try_parse_let_statement(),
            Token::Return => self.try_parse_return_statement(),
            _ => self.try_parse_expression_statement(),
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

        Ok(Statement::Let(Ident(ident), Expression::Ident(expression)))
    }

    fn try_parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        // TODO: actually parse instead of skipping expressions until a `;`.
        let statement = Statement::Return(Expression::Ident("".to_string()));
        while !self.current_token_is(Token::Semicolon) {
            self.next()
        }
        Ok(statement)
    }

    fn try_parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let statement = Statement::Expression(self.parse_expression(ExpressionPrecedence::Lowest)?);
        if self.next_token_is(Token::Semicolon) {
            self.next();
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

    // TODO: should this not raise an error instead?
    fn expect_next(&mut self, token: Token) -> bool {
        if self.next_token_is(token) {
            self.next();
            true
        } else {
            false
        }
    }

    // E X P R E S S I O N   S T A T E M E N T   P A R S I N G
    // -------------------------------------------------------
    /// Expressions can be parsed with various levels of precedence.
    /// The resulting expressions can later be compared to select only the one
    /// with the highest precedence.
    ///
    /// The `match` statement serves as the hashmap in the book, where every
    /// token is associated with a specific parsing funtionality.
    fn parse_expression(
        &mut self,
        precedence: ExpressionPrecedence,
    ) -> Result<Expression, ParseError> {
        match self.current_token {
            Token::Bang | Token::Minus => self.try_parse_prefix(),
            Token::Ident(ref s) => Ok(Expression::Ident(s.clone())),
            // Integer tokens are already parsed through `lexer.read_number()`.
            Token::Int(i) => Ok(Expression::Literal(Literal::Integer(i))),
            _ => todo!(),
        }
    }

    fn parse_identifier(&mut self) -> Expression {
        match &self.current_token {
            Token::Ident(s) => Expression::Ident(s.to_string()),
            _ => unreachable!(),
        }
    }

    // PREFIXES
    // --------
    fn try_parse_prefix(&mut self) -> Result<Expression, ParseError> {
        let current = self.current_token.clone();
        self.next();
        let expression = self.parse_expression(ExpressionPrecedence::Prefix)?;
        Ok(Expression::Prefix(current, Box::new(expression)))
    }

    // INFIXES
    // --------
    fn try_parse_infix(&mut self) -> Result<Expression, ParseError> {
        todo!()
    }
}

#[cfg(test)]

mod tests {
    use crate::{
        ast::{Expression, Ident, Literal, Program, Statement},
        lexer::Lexer,
        token::Token,
    };

    use super::{parse, ParseError, Parser};

    // #[test]
    // fn test_let_statement_ok() {
    //     let input = "
    //     let x = 5;
    //     let y = 10;
    //     let foobar = 838383;
    //     ";
    //     let mut lexer = Lexer::new(input);
    //     let mut parser = Parser::new(&mut lexer);
    //     let program = parser.parse_program();
    //     assert!(!program.is_empty());
    //     assert_eq!(program.len(), 3);

    //     let identifiers = [
    //         Statement::Let(
    //             Ident(String::from("x")),
    //             Expression::Ident(String::from("")), // TODO: will be parsed in the future
    //         ),
    //         Statement::Let(
    //             Ident(String::from("y")),
    //             Expression::Ident(String::from("")), // TODO: will be parsed in the future
    //         ),
    //         Statement::Let(
    //             Ident(String::from("foobar")),
    //             Expression::Ident(String::from("")), // TODO: will be parsed in the future
    //         ),
    //     ];
    //     for (expected, statement) in identifiers.iter().zip(program.iter()) {
    //         assert_eq!(expected, statement)
    //     }
    // }

    // #[test]
    // fn test_let_statement_bad() {
    //     let input = "
    //     let x 5;
    //     let = 10;
    //     let 838383;
    //     ";
    //     let mut lexer = Lexer::new(input);
    //     let mut parser = Parser::new(&mut lexer);
    //     let program = parser.parse_program();
    //     assert!(program.is_empty());
    //     dbg!(&parser.errors);
    //     assert_eq!(parser.errors.len(), 3);

    //     let errors = [
    //         ParseError::UnexpectedToken(Token::Assign, Token::Int(5)),
    //         ParseError::UnexpectedToken(Token::Ident("".to_string()), Token::Assign),
    //         ParseError::UnexpectedToken(Token::Ident("".to_string()), Token::Int(838383)),
    //     ];
    //     for (expected, error) in errors.iter().zip(parser.errors.iter()) {
    //         assert_eq!(expected, error)
    //     }
    // }

    // #[test]
    // fn test_return_statement_ok() {
    //     let input = "
    //         return 5;
    //         return 10;
    //         return 993322";

    //     let mut lexer = Lexer::new(input);
    //     let mut parser = Parser::new(&mut lexer);
    //     let program = parser.parse_program();

    //     assert!(!program.is_empty());
    //     assert!(program.len() == 3);

    //     let identifiers = [
    //         Statement::Return(Expression::Ident("".to_string())),
    //         Statement::Return(Expression::Ident("".to_string())),
    //         Statement::Return(Expression::Ident("".to_string())),
    //     ];
    //     for (expected, statement) in identifiers.iter().zip(program.iter()) {
    //         assert_eq!(expected, statement)
    //     }
    // }

    fn test_helper(test_case: &[[&str; 2]]) {
        for [input, expected] in test_case {
            match parse(input) {
                Ok(program) => assert_eq!(*expected, &format!("{program}")),
                Err(e) => panic!("Parsing error: {:#?}", e),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        test_helper(&[["foobar;", "foobar"]]);
    }

    #[test]
    fn test_integer_literal_expression() {
        test_helper(&[["5;", "5"]]);
    }

    #[test]
    fn parse_prefix_expression() {
        test_helper(&[["!5", "(!5)"], ["-15", "(-15)"]]);
    }
}
