use crate::{
    ast::{Expression, Ident, Literal, Node, Precedence},
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
        let statement = Statement::Expression(self.parse_expression(Precedence::Lowest)?);
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

    fn current_precedence(&self) -> Precedence {
        Precedence::from(&self.current_token)
    }

    fn next_precedence(&self) -> Precedence {
        Precedence::from(&self.next_token)
    }

    // E X P R E S S I O N   S T A T E M E N T   P A R S I N G
    // -------------------------------------------------------
    /// Expressions can be parsed with various levels of precedence.
    /// The resulting expressions can later be compared to select only the one
    /// with the highest precedence.
    ///
    /// The `match` statement serves as the hashmap in the book, where every
    /// token is associated with a specific parsing funtionality.
    ///
    /// Consider parsing the expression: `1 + 2 - 3;` -> `((1 + 2) - 3)`.
    ///
    /// Initially:
    /// - `current_token` is at `1`, with precedence 0.
    /// - `next_token` is at `+`, with precedence 3.
    ///
    /// As such, `next()` is called to see the effect of this next token.
    /// Now, with:
    /// - `current_token` is at `+`, with precedence 3.
    /// - `next_token` is at `2`, with precedence 0.
    ///
    /// the `try_parse_infix()` method is called, which advances the tokens
    /// again and recursively calls this `parse_expression()` method again
    /// with the precedence of 5.
    ///
    /// In this recursive call:
    /// - `current_token` is at `2`, with precedence 0.
    /// - `next_token` is at `-`, with precedence 3.
    ///
    /// As a result, the below loop is not called again in this stackframe,
    /// but `left_expr` has been updated from `1` to `(1 + 2)`.
    ///
    /// Upon returning to the original stackframe, this loop is evaluated
    /// once again with the same tokens as in the recursive call:
    /// - `current_token` is at `2`, with still the original precedence 0.
    /// - `next_token` is at `-`, with precedence 3.
    ///
    /// So, the loop is evaluated a second time in this stackframe, now using
    /// the minus token. The tokens are updated to:
    /// - `current_token` is at `-`, with precedence 3.
    /// - `next_token` is at `3`, with precedence 0.
    ///
    /// Once again, the `try_parse_infix()` method is called, advancing the
    /// tokens one last time to:
    /// - `current_token` is at `3`, with precedence 0.
    /// - `next_token` is at `;`, with precedence 0.
    ///
    /// A final stackframe is created when `parse_expression()` is called
    /// again with precedence 5. The below loop is not executed because
    /// the next token is `;` and the precedence check does not pass.
    /// The resulting expression in the `try_parse_prefix()` stackframe is
    /// then `3`, which is appended as the right element.
    ///
    /// The final expression is then: `((1 + 2) - 3)`.
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let mut left_expr = match self.current_token {
            Token::Bang | Token::Minus => self.try_parse_prefix(),
            Token::Ident(ref s) => Ok(Expression::Ident(s.clone())),
            // Integer tokens are already parsed through `lexer.read_number()`.
            Token::Int(i) => Ok(Expression::Literal(Literal::Integer(i))),
            Token::Bool(b) => Ok(Expression::Literal(Literal::Bool(b))),
            // Grouped expressions.
            Token::LParen => self.try_parse_grouped_expression(),
            // TODO: if/else
            // TODO: function definitions and calls
            // TODO: hashes
            _ => return Err(ParseError::UnknownToken(self.current_token.clone())),
        };
        // Some tokens can modify the meaning of the previous token. This is
        // governed by relative the `Precedence` of the next token.
        while !self.next_token_is(Token::Semicolon) && precedence < self.next_precedence() {
            match self.next_token {
                // Mathematical operations: a + b
                Token::Plus | Token::Minus | Token::Asterisk | Token::Slash |
                // Testing for equality: a == b
                Token::Equal | Token::NotEqual | Token::GreaterThan | Token::LessThan => {
                    self.next();
                    let expr = left_expr.unwrap();
                    left_expr = self.try_parse_infix(expr);
                }
                // TODO: parse index.
                // TODO: parse function call.
                // All other tokens do not modify the left expression.
                _ => {}
            }
        }
        left_expr
    }

    fn parse_identifier(&mut self) -> Expression {
        match &self.current_token {
            Token::Ident(s) => Expression::Ident(s.to_string()),
            _ => unreachable!(),
        }
    }

    // PREFIXES, INFIXES, GROUPS
    // -------------------------
    fn try_parse_prefix(&mut self) -> Result<Expression, ParseError> {
        let operation = self.current_token.clone();
        self.next();
        let expression = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix(operation, Box::new(expression)))
    }

    fn try_parse_infix(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let operation = self.current_token.clone();
        let precedence = self.current_precedence();
        self.next();
        let right = self.parse_expression(precedence)?;
        Ok(Expression::Infix(
            operation,
            Box::new(left),
            Box::new(right),
        ))
    }

    fn try_parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        // Skip the `(`.
        self.next();
        let expr = self.parse_expression(Precedence::Lowest)?;
        // A group has to end with a `)`.
        if !self.expect_next(Token::RParen) {
            return Err(ParseError::UnexpectedToken(
                Token::RParen,
                self.current_token.clone(),
            ));
        }
        Ok(expr)
    }
}

#[cfg(test)]

mod tests {
    use crate::{lexer::Lexer, token::Token};

    use super::{parse, ParseError, Parser};

    #[test]
    fn test_let_statement_ok() {
        // TODO: parser skips over the expressions: try_parse_let_statement.
        // Whenever it does, add in the values 5, 10, 838383. See pg 99.
        test_helper(&[
            ["let x = 5;", "let x = ;"],
            ["let y = 10;", "let y = ;"],
            ["let foobar = 838383;", "let foobar = ;"],
        ]);
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
        assert_eq!(program.len(), 3);
        assert_eq!(parser.errors.len(), 3);

        // TODO: parser skips over the expressions: try_parse_let_statement.
        // Whenever it does, add in the values 5, 10, 838383. See pg 99.
        let errors = [
            ParseError::UnexpectedToken(Token::Assign, Token::Int(5)),
            ParseError::UnexpectedToken(Token::Ident("".to_string()), Token::Assign),
            ParseError::UnexpectedToken(Token::Ident("".to_string()), Token::Int(838383)),
        ];
        for (expected, error) in errors.iter().zip(parser.errors.iter()) {
            assert_eq!(expected, error)
        }
    }

    #[test]
    fn test_return_statement_ok() {
        // TODO: parser skips over the expressions: try_parse_return_statement.
        // Whenever it does, add in the values 5, 10, 993322. See pg 99.
        test_helper(&[
            ["return 5;", "return ;"],
            ["return 10;", "return ;"],
            ["return 993322;", "return ;"],
        ]);
    }

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
    fn test_boolean_expression() {
        test_helper(&[
            ["true;", "true"],
            ["false;", "false"],
            // TODO: parser skips over the expressions: try_parse_let_statement.
            // Whenever it does, add in the values true, false. See pg 99.
            ["let foobar = true;", "let foobar = ;"],
            ["let foobar = false;", "let foobar = ;"],
        ]);
    }

    #[test]
    fn test_prefix_expression() {
        test_helper(&[["!5", "(!5)"], ["-15", "(-15)"]]);
    }

    #[test]
    fn test_infix_expression() {
        test_helper(&[
            // Using numbers.
            ["5 + 5;", "(5 + 5)"],
            ["5 - 5;", "(5 - 5)"],
            ["5 * 5;", "(5 * 5)"],
            ["5 / 5;", "(5 / 5)"],
            ["5 > 5;", "(5 > 5)"],
            ["5 < 5;", "(5 < 5)"],
            ["5 == 5;", "(5 == 5)"],
            ["5 != 5;", "(5 != 5)"],
            // Using variables.
            ["foobar + barfoo;", "(foobar + barfoo)"],
            ["foobar - barfoo;", "(foobar - barfoo)"],
            ["foobar * barfoo;", "(foobar * barfoo)"],
            ["foobar / barfoo;", "(foobar / barfoo)"],
            ["foobar > barfoo;", "(foobar > barfoo)"],
            ["foobar < barfoo;", "(foobar < barfoo)"],
            ["foobar == barfoo;", "(foobar == barfoo)"],
            ["foobar != barfoo;", "(foobar != barfoo)"],
            // Using booleans.
            ["true == true;", "(true == true)"],
            ["true != false;", "(true != false)"],
            ["false == false;", "(false == false)"],
        ]);
    }

    #[test]
    fn test_operator_precedence() {
        test_helper(&[
            // Using numbers.
            ["3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"],
            ["5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"],
            ["5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"],
            [
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ],
            // ["1 + (2 + 3) + 4"],
            // Using booleans.
            // ["true", "true"],
            // ["false", "false"],
            // Using both.
            // ["3 > 5 == false", "((3 > 5) == false)"],
            // ["3 < 5 == true", "((3 < 5) == true)"],
            // Using variables.
            ["-a * b", "((-a) * b)"],
            ["!-a", "(!(-a))"],
            ["a + b + c", "((a + b) + c)"],
            ["a + b - c", "((a + b) - c)"],
            ["a * b * c", "((a * b) * c)"],
            ["a * b / c", "((a * b) / c)"],
            ["a + b / c", "(a + (b / c))"],
            ["a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"],
        ]);
    }

    #[test]
    fn test_grouped_expression() {
        test_helper(&[
            ["1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"],
            ["(5 + 5) * 2", "((5 + 5) * 2)"],
            ["2 / (5 + 5)", "(2 / (5 + 5))"],
            ["-(5 + 5)", "(-(5 + 5))"],
            ["!(true == true)", "(!(true == true))"],
        ]);
    }
}
