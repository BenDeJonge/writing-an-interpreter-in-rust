use crate::lexing::{
    ast::{
        BlockStatement, Expression, FunctionArguments, Identifier, Literal, Node, Program,
        Statement,
    },
    token::Token,
};

use super::{
    environment::{Env, Environment},
    error::EvaluationError,
    object::{to_boolean_object, Object},
};

pub type Evaluation = Result<Object, EvaluationError>;

pub fn format_evaluation(evaluation: &Evaluation) -> String {
    match evaluation {
        Evaluation::Ok(obj) => format!("{obj}"),
        Evaluation::Err(err) => format!("{err}"),
    }
}

pub fn eval(node: Node, env: &Env) -> Evaluation {
    match node {
        Node::Program(program) => eval_program(&program, env),
        Node::Statement(statement) => eval_statement(&statement, env),
        Node::Expression(expression) => eval_expression(&expression, env),
    }
}

// -----------------------------------------------------------------------------
// E V A L U A T I N G   P R O G R A M S
// -----------------------------------------------------------------------------
fn eval_program(program: &Program, env: &Env) -> Evaluation {
    let mut object = Object::Null;
    for statement in program.iter() {
        object = eval_statement(statement, env)?;
        // Return the inner object of the return value.
        if let Object::ReturnValue(ret) = object {
            return Ok(*ret);
        }
    }
    Ok(object)
}

// -----------------------------------------------------------------------------
// E V A L U A T I N G   S T A T E M E N T S
// -----------------------------------------------------------------------------
fn eval_statement(statement: &Statement, env: &Env) -> Evaluation {
    match statement {
        Statement::Expression(expression) => eval_expression(expression, env),
        Statement::Return(expression) => Ok(Object::ReturnValue(Box::new(eval_expression(
            expression, env,
        )?))),
        Statement::Let(id, expression) => eval_let_statement(id.clone(), expression, env),
    }
}

fn eval_block_statement(block: &BlockStatement, env: &Env) -> Evaluation {
    let mut object = Object::Null;
    for statement in block.iter() {
        object = eval_statement(statement, env)?;
        // Do not return the inner object of the return value.
        // So, an `Object::ReturnValue` is returned, which bubbles up in nested
        // `eval_program()` and `eval_block_statement()` calls.
        if let Object::ReturnValue(_) = object {
            return Ok(object);
        }
    }
    Ok(object)
}

fn eval_let_statement(id: Identifier, expression: &Expression, env: &Env) -> Evaluation {
    let val = eval_expression(expression, env)?;
    env.borrow_mut().insert(id, val);
    Ok(Object::Null)
}

// -----------------------------------------------------------------------------
// E V A L U A T I N G   E X P R E S S I O N S
// -----------------------------------------------------------------------------
fn eval_expression(expression: &Expression, env: &Env) -> Evaluation {
    Ok(match expression {
        Expression::Literal(literal) => match literal {
            Literal::Integer(i) => Object::Integer(*i),
            Literal::Bool(b) => to_boolean_object(*b),
        },
        Expression::Prefix(operation, right) => {
            eval_prefix_expression(operation, eval_expression(right, env)?)?
        }

        Expression::Infix(operation, left, right) => eval_infix_expression(
            operation,
            eval_expression(left, env)?,
            eval_expression(right, env)?,
        )?,
        Expression::Conditional(condition, consequence, alternative) => {
            eval_conditional_expression(condition, consequence, alternative, env)?
        }
        Expression::Ident(id) => eval_identity_expression(id, env)?,
        Expression::FunctionLiteral(arguments, definition) => {
            eval_function_definition(arguments, definition)?
        }
        Expression::FunctionCall(name, arguments) => eval_function_call(name, arguments, env)?,
    })
}

fn eval_multiple_expressions(
    expressions: &[Expression],
    env: &Env,
) -> Result<Vec<Object>, EvaluationError> {
    expressions
        .iter()
        .map(|expression| eval_expression(expression, env))
        .collect()
}

// P R E F I X   E X P R E S S I O N S
// -----------------------------------
fn eval_prefix_expression(operation: &Token, right: Object) -> Evaluation {
    match operation {
        Token::Bang => Evaluation::Ok(eval_bang_expression(&right)),
        Token::Minus => eval_minus_expression(right),
        _ => {
            unreachable!("Expected to call method only with prefix operators. Got `{operation}`.",)
        }
    }
}

fn eval_bang_expression(right: &Object) -> Object {
    to_boolean_object(!is_truthy(right))
}

fn eval_minus_expression(right: Object) -> Evaluation {
    match right {
        Object::Integer(i) => Ok(Object::Integer(-i)),
        _ => Err(EvaluationError::InvalidPrefixOperator(Token::Minus, right)),
    }
}

// I N F I X   E X P R E S S I O N S
// -----------------------------------
fn eval_infix_expression(operation: &Token, left: Object, right: Object) -> Evaluation {
    match (&left, &right) {
        (Object::Integer(l), Object::Integer(r)) => eval_infix_integer(operation, *l, *r),
        (Object::Bool(l), Object::Bool(r)) => eval_infix_boolean(operation, *l, *r),
        _ => Err(EvaluationError::TypeMismatch(left, right)),
    }
}

fn eval_infix_integer(operation: &Token, left: isize, right: isize) -> Evaluation {
    Ok(match operation {
        // Arithmetic
        Token::Plus => Object::Integer(left + right),
        Token::Minus => Object::Integer(left - right),
        Token::Asterisk => Object::Integer(left * right),
        Token::Slash => Object::Integer(left / right),
        // Equality
        Token::Equal => to_boolean_object(left == right),
        Token::NotEqual => to_boolean_object(left != right),
        Token::GreaterThan => to_boolean_object(left > right),
        Token::LessThan => to_boolean_object(left < right),
        _ => unreachable!("Expected to call method only with infix operators. Got `{operation}`."),
    })
}

fn eval_infix_boolean(operation: &Token, left: bool, right: bool) -> Evaluation {
    match operation {
        Token::Equal => Ok(to_boolean_object(left == right)),
        Token::NotEqual => Ok(to_boolean_object(left != right)),
        _ => Err(EvaluationError::InvalidInfixOperator(
            operation.clone(),
            to_boolean_object(left),
            to_boolean_object(right),
        )),
    }
}

// I N F I X   E X P R E S S I O N S
// -----------------------------------
fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Null => false,
        Object::Bool(false) => false,
        // TODO: does 0 evaluate to true?
        _ => true,
    }
}

fn eval_conditional_expression(
    condition: &Expression,
    consequence: &BlockStatement,
    alternative: &Option<BlockStatement>,
    env: &Env,
) -> Evaluation {
    if is_truthy(&eval_expression(condition, env)?) {
        eval_block_statement(consequence, env)
    } else if let Some(a) = alternative {
        eval_block_statement(a, env)
    } else {
        Ok(Object::Null)
    }
}

// I D E N T I T Y   E X P R E S S I O N S
// ---------------------------------------
fn eval_identity_expression(id: &Identifier, env: &Env) -> Evaluation {
    env.borrow()
        .get(id)
        .map(|object| Ok(object.clone()))
        .unwrap_or(Err(EvaluationError::UnknowIdentifier(id.clone())))
}

// F U N C T I O N A L   E X P R E S S I O N S
// -------------------------------------------

fn eval_function_definition(
    arguments: &FunctionArguments,
    definition: &BlockStatement,
) -> Evaluation {
    Ok(Object::Function(arguments.to_vec(), definition.to_vec()))
}

fn eval_function_call(name: &Expression, arguments: &[Expression], env: &Env) -> Evaluation {
    let function = eval_expression(name, env)?;
    let args = eval_multiple_expressions(arguments, env)?;
    apply_function(function, &args, env)
}

fn apply_function(function: Object, arg_values: &[Object], env: &Env) -> Evaluation {
    match function {
        Object::Function(arg_names, body) => {
            let env = extend_function_environment(arg_values, arg_names, env)?;
            Ok(unwrap_return(eval_block_statement(&body, &env.into())?))
        }
        _ => todo!("support built-in functions"),
    }
}

/// Create a new local scope extended with a copy of the outer scope.
fn extend_function_environment(
    arg_values: &[Object],
    arg_names: Vec<Identifier>,
    env: &Env,
) -> Result<Environment, EvaluationError> {
    if arg_names.len() != arg_values.len() {
        return Err(EvaluationError::IncorrectArgumentCount(
            arg_names.len(),
            arg_values.len(),
        ));
    }
    let mut env = Environment::enclose(env);
    arg_names
        .into_iter()
        .zip(arg_values)
        .for_each(|(name, val)| {
            env.insert(name, val.clone());
        });
    Ok(env)
}

fn unwrap_return(object: Object) -> Object {
    if let Object::ReturnValue(ret) = object {
        return *ret;
    }
    object
}

#[cfg(test)]
mod tests {
    use std::vec;

    use crate::{
        evaluating::{
            environment::Env,
            error::EvaluationError,
            evaluator::eval,
            object::{IntoEval, Object, OBJECT_FALSE, OBJECT_TRUE},
        },
        lexing::{
            ast::{Expression, Identifier, Literal, Statement},
            token::Token,
        },
        parsing::parser::parse,
    };

    fn test_helper<T: IntoEval>(test_case: Vec<(&str, T)>) {
        for (input, object) in test_case {
            assert_eq!(
                eval(
                    parse(input).expect("parsed succesfully"),
                    // Unit tests should be stateless, so a new Env is
                    // created for each test case.
                    &Env::default()
                ),
                object.into_eval()
            )
        }
    }

    #[test]
    fn test_integer_expression() {
        test_helper(vec![
            // Simple integers.
            ("5", 5),
            ("10", 10),
            // Minus prefixes.
            ("-5", -5),
            ("-10", -10),
            ("--5", 5),
            ("--10", 10),
            // Arithmetic.
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 - 50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ]);
    }

    #[test]
    fn test_boolean_expression() {
        test_helper(vec![
            // Simple booleans.
            ("true", true),
            ("false", false),
            // Integers.
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            // Mixed booleans and integers.
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ])
    }

    #[test]
    fn test_bang_operator() {
        test_helper(vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ]);
    }

    #[test]
    fn test_conditional_expression() {
        test_helper(vec![
            ("if (true) {10}", Some(10)),
            ("if (false) {10}", None),
            ("if (1) {10}", Some(10)),
            ("if (1 < 2) {10}", Some(10)),
            ("if (1 > 2) {10}", None),
            ("if (1 > 2) {10} else {20}", Some(20)),
            ("if (1 < 2) {10} else {20}", Some(10)),
        ]);
    }

    #[test]
    fn test_return_statement() {
        test_helper(vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
                    if (10 > 1) { 
                        return 10; 
                }
                return 1;
            }",
                10,
            ),
        ]);
    }

    #[test]
    fn test_error_handling() {
        test_helper(vec![
            (
                "5 + true;",
                EvaluationError::TypeMismatch(5.into_eval().unwrap(), OBJECT_TRUE),
            ),
            (
                "5 + true; 5",
                EvaluationError::TypeMismatch(5.into_eval().unwrap(), OBJECT_TRUE),
            ),
            (
                "true + false",
                EvaluationError::InvalidInfixOperator(Token::Plus, OBJECT_TRUE, OBJECT_FALSE),
            ),
            (
                "5; true - false; 5",
                EvaluationError::InvalidInfixOperator(Token::Minus, OBJECT_TRUE, OBJECT_FALSE),
            ),
            (
                "5; true * false; 5",
                EvaluationError::InvalidInfixOperator(Token::Asterisk, OBJECT_TRUE, OBJECT_FALSE),
            ),
            (
                "if (10 > 1) {true / false; }",
                EvaluationError::InvalidInfixOperator(Token::Slash, OBJECT_TRUE, OBJECT_FALSE),
            ),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        true + false;
                    }
                }",
                EvaluationError::InvalidInfixOperator(Token::Plus, OBJECT_TRUE, OBJECT_FALSE),
            ),
            (
                "-true",
                EvaluationError::InvalidPrefixOperator(Token::Minus, OBJECT_TRUE),
            ),
        ])
    }

    #[test]
    fn test_let_statement() {
        test_helper(vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ]);
    }

    #[test]
    fn test_function_definition() {
        // Low level test method to ensure that the function representation in
        // the backend is fully correct.
        test_helper(vec![(
            "fn(x) { x + 2; };",
            Object::Function(
                vec![Identifier("x".to_string())],
                vec![Statement::Expression(Expression::Infix(
                    Token::Plus,
                    Box::new(Expression::Ident(Identifier("x".to_string()))),
                    Box::new(Expression::Literal(Literal::Integer(2))),
                ))],
            ),
        )]);
    }

    #[test]
    fn test_function_call() {
        test_helper(vec![
            (
                "
                let identity = fn(x) { x; };
                identity(5);
                ",
                5,
            ),
            (
                "
                let identity = fn(x) { return x; };
                identity(5);
                ",
                5,
            ),
            (
                "
                let double = fn(x) { x * 2; };
                double(5);
                ",
                10,
            ),
            (
                "
                let add = fn(x, y) { x + y; };
                add(5, 5);
                ",
                10,
            ),
            (
                "
                let add = fn(x, y) { x + y; };
                add(5 + 5, add(5, 5));
                ",
                20,
            ),
            (
                "
                fn(x) { x; }(5)
                ",
                5,
            ),
        ]);
    }
}
