use std::mem;

use crate::lexing::{
    ast::{
        BlockStatement, Expression, FunctionArguments, Identifier, Literal, Node, Precedence,
        Program, Statement,
    },
    lexer::Lexer,
    token::Token,
};

use super::error::{ParseError, ParseErrors};

pub fn parse(input: &str) -> Result<Node, ParseErrors> {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();
    match program {
        Ok(p) => Ok(Node::Program(p)),
        Err(e) => Err(ParseErrors(e.to_vec())),
    }
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
            errors: ParseErrors(vec![]),
        };
        // Advance the parser twice to load `current_token` and `next_token`.
        parser.next();
        parser.next();
        parser
    }

    fn next(&mut self) {
        mem::swap(&mut self.current_token, &mut self.next_token);
        self.next_token = self.lexer.next_token();
    }

    pub fn get_errors(&self) -> &ParseErrors {
        &self.errors
    }

    // -------------------------------------------------------------------------
    // P A R S I N G
    // -------------------------------------------------------------------------
    pub fn parse_program(&mut self) -> Result<Program, &ParseErrors> {
        let mut program = Program::default();
        while !self.current_token_is(&Token::Eof) {
            match self.parse_statement() {
                Ok(statement) => program.push(statement),
                Err(e) => {
                    self.errors.push(e);
                }
            }
            self.next();
        }
        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.get_errors())
        }
    }

    /// There only exist two true statement types in Monkey:
    /// - `let`
    /// - `return`
    ///
    ///  If the token points to neither, parse an expression instead.
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        let res = match self.current_token {
            Token::Let => self.try_parse_let_statement(),
            Token::Return => self.try_parse_return_statement(),
            _ => self.try_parse_expression_statement(),
        };
        if res.is_err() {
            self.skip_rest_of_statement();
        }
        res
    }

    // S P E C I F I C   P A R S E R S
    // -------------------------------
    fn skip_rest_of_statement(&mut self) {
        while !(self.current_token_is(&Token::Semicolon) | self.current_token_is(&Token::Eof)) {
            self.next();
        }
    }

    fn try_parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        // let five = 5;
        // Get the `five`.
        let ident = match &self.next_token {
            Token::Ident(s) => s.clone(),
            _ => return Err(ParseError::MissingIdent(self.next_token.clone())),
        };
        self.next();
        // Get the `=` and skip it.
        self.expect_next(&Token::Assign)?;
        self.next();
        let statement = Ok(Statement::Let(
            Identifier(ident),
            // Get the `5`.
            self.parse_expression(Precedence::Lowest)?,
        ));
        self.expect_next(&Token::Semicolon)?;
        statement
    }

    fn try_parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        // Skip the `return`.
        self.next();
        let statement = Ok(Statement::Return(
            self.parse_expression(Precedence::Lowest)?,
        ));
        // Skip the optional `;`.
        if self.next_token_is(&Token::Semicolon) {
            self.next();
        }
        statement
    }

    fn try_parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let statement = Statement::Expression(self.parse_expression(Precedence::Lowest)?);
        if self.next_token_is(&Token::Semicolon) {
            self.next();
        }
        Ok(statement)
    }

    fn try_parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let mut block = BlockStatement::new();
        // Skip the left brace.
        self.next();
        while !self.current_token_is(&Token::RBrace) && !self.current_token_is(&Token::Eof) {
            block.push(self.parse_statement()?);
            self.next();
        }
        Ok(block)
    }

    // H E L P E R   F U N C T I O N S
    // -------------------------------
    fn current_token_is(&self, token: &Token) -> bool {
        &self.current_token == token
    }

    fn next_token_is(&self, token: &Token) -> bool {
        &self.next_token == token
    }

    fn expect_next(&mut self, token: &Token) -> Result<(), ParseError> {
        if self.next_token_is(token) {
            self.next();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken(
                token.clone(),
                self.next_token.clone(),
            ))
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
        let mut left_expr = match &self.current_token {
            // Prefixes.
            Token::Bang | Token::Minus => self.try_parse_prefix(),
            Token::Ident(ref s) => Ok(Expression::Ident(Identifier(s.clone()))),
            // Literals.
            // Integer tokens are already parsed through `lexer.read_number()`.
            Token::Int(i) => Ok(Expression::Literal(Literal::Integer(*i))),
            Token::Bool(b) => Ok(Expression::Literal(Literal::Bool(*b))),
            Token::String(s) => Ok(Expression::Literal(Literal::String(s.clone()))),
            // Grouped expressions.
            Token::LParen => self.try_parse_grouped_expression(),
            Token::If => self.try_parse_conditional_expression(),
            Token::Function => self.try_parse_fn_expression(),
            // TODO: hashes
            _ => {
                return Err(ParseError::InvalidExpressionToken(
                    self.current_token.clone(),
                ))
            }
        };
        // Some tokens can modify the meaning of the previous token. This is
        // governed by relative the `Precedence` of the next token.
        while !self.next_token_is(&Token::Semicolon) && precedence < self.next_precedence() {
            match self.next_token {
                // Mathematical operations: a + b
                Token::Plus | Token::Minus | Token::Asterisk | Token::Slash |
                // Testing for equality: a == b
                Token::Equal | Token::NotEqual | Token::GreaterThan | Token::LessThan => {
                    self.next();
                    let expr = left_expr.unwrap();
                    left_expr = self.try_parse_infix(expr);
                }
                Token::LParen => {
                    self.next();
                    let expr = left_expr.unwrap();
                    left_expr = self.try_parse_fn_call_expression(expr);
                }
                // TODO: parse index.
                // All other tokens do not modify the left expression.
                _ => {}
            }
        }
        left_expr
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
        self.expect_next(&Token::RParen)?;
        Ok(expr)
    }

    fn try_parse_conditional_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect_next(&Token::LParen)?;
        self.next();
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_next(&Token::RParen)?;
        self.expect_next(&Token::LBrace)?;
        let consequence = self.try_parse_block_statement()?;
        let mut alternative = None;
        if self.next_token_is(&Token::Else) {
            self.next();
            self.expect_next(&Token::LBrace)?;
            alternative = Some(self.try_parse_block_statement()?);
        }
        Ok(Expression::Conditional(
            Box::new(condition),
            consequence,
            alternative,
        ))
    }

    // FUNCTIONS
    // ---------
    fn try_parse_fn_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect_next(&Token::LParen)?;
        let arguments = self.try_parse_fn_arguments()?;
        self.expect_next(&Token::LBrace)?;
        let body = self.try_parse_block_statement()?;
        Ok(Expression::FunctionLiteral(arguments, body))
    }

    fn try_parse_list_helper<F, T>(&mut self, parse_fn: F) -> Result<Vec<T>, ParseError>
    where
        F: Fn(&mut Self) -> Result<T, ParseError>,
    {
        let mut arguments = Vec::new();
        if self.next_token_is(&Token::RParen) {
            self.next();
            return Ok(arguments);
        }
        self.next();
        arguments.push(parse_fn(self)?);
        while self.next_token_is(&Token::Comma) {
            self.next();
            self.next();
            arguments.push(parse_fn(self)?);
        }
        self.expect_next(&Token::RParen)?;
        Ok(arguments)
    }

    fn try_parse_fn_arguments(&mut self) -> Result<FunctionArguments, ParseError> {
        self.try_parse_list_helper(|s: &mut Self| Ok(Identifier(s.current_token.to_string())))
    }

    fn try_parse_fn_call_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        Ok(Expression::FunctionCall(
            Box::new(left),
            self.try_parse_fn_call_arguments()?,
        ))
    }

    fn try_parse_fn_call_arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        self.try_parse_list_helper(|s: &mut Self| Self::parse_expression(s, Precedence::Lowest))
    }
}

#[cfg(test)]

mod tests {
    use crate::lexing::{lexer::Lexer, token::Token};

    use super::{parse, ParseError, Parser};

    fn test_helper(test_case: &[[&str; 2]]) {
        for [input, expected] in test_case {
            match parse(input) {
                Ok(program) => assert_eq!(*expected, &format!("{program}")),
                Err(e) => panic!("Parsing error: {:#?}", e),
            }
        }
    }

    #[test]
    fn test_let_statement_ok() {
        test_helper(&[
            ["let x = 5;", "let x = 5;"],
            ["let y = 10;", "let y = 10;"],
            ["let foobar = 838383;", "let foobar = 838383;"],
        ]);
    }

    #[test]
    fn test_let_statement_bad() {
        let input = "
        let x 5;
        let = 10;
        let 838383;
        let x = 5
        ";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
        match program {
            Ok(_) => {
                panic!("program is not err")
            }
            Err(err) => {
                assert_eq!(err.len(), 4);
                let expected_errors = [
                    ParseError::UnexpectedToken(Token::Assign, Token::Int(5)),
                    ParseError::MissingIdent(Token::Assign),
                    ParseError::MissingIdent(Token::Int(838383)),
                    ParseError::UnexpectedToken(Token::Semicolon, Token::Eof),
                ];
                for (expected, error) in expected_errors.iter().zip(err.iter()) {
                    assert_eq!(expected, error)
                }
            }
        }
    }

    #[test]
    fn test_return_statement_ok() {
        test_helper(&[
            ["return 5;", "return 5;"],
            ["return 10;", "return 10;"],
            ["return 993322;", "return 993322;"],
        ]);
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
            ["let foobar = true;", "let foobar = true;"],
            ["let foobar = false;", "let foobar = false;"],
        ]);
    }

    #[test]
    fn test_string_literal_expression() {
        test_helper(&[
            ["\"foobar\"", "\"foobar\""],
            ["\"foo bar\"", "\"foo bar\""],
            ["let foobar = \"foobar\";", "let foobar = \"foobar\";"],
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
            ["1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"],
            // Using booleans.
            ["true", "true"],
            ["false", "false"],
            // Using both.
            ["3 > 5 == false", "((3 > 5) == false)"],
            ["3 < 5 == true", "((3 < 5) == true)"],
            // Using variables.
            ["-a * b", "((-a) * b)"],
            ["!-a", "(!(-a))"],
            ["a + b + c", "((a + b) + c)"],
            ["a + b - c", "((a + b) - c)"],
            ["a * b * c", "((a * b) * c)"],
            ["a * b / c", "((a * b) / c)"],
            ["a + b / c", "(a + (b / c))"],
            ["a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"],
            // Using function calls.
            ["a + add(b * c) + d", "((a + add((b * c))) + d)"],
            [
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ],
            [
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ],
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

    #[test]
    fn test_conditional_expression() {
        test_helper(&[
            [
                "if (x > y) {
                    return x;
                } else {
                    return y;
                }",
                "(if { (x > y) } { return x; } else { return y; }",
            ],
            [
                "if (x > y) {
                    return x;
                }",
                "(if { (x > y) } { return x; }",
            ],
            [
                "let foobar = if (x > y) { x } else { y };",
                "let foobar = (if { (x > y) } { x } else { y };",
            ],
        ]);
    }

    #[test]
    fn test_functional_expression() {
        test_helper(&[
            ["fn() { return 5; }", "fn() { return 5; }"],
            ["fn(x) { return x + 1;}", "fn(x) { return (x + 1); }"],
            ["fn(x, y) { return x + y; }", "fn(x, y) { return (x + y); }"],
        ]);
    }

    #[test]
    fn test_function_call() {
        test_helper(&[["add(1, 2 * 3, 4 + 5);", "add(1, (2 * 3), (4 + 5))"]]);
    }
}
