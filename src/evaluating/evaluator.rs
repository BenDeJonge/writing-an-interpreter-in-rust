use crate::lexing::{
    ast::{Expression, Literal, Node, Program, Statement},
    token::Token,
};

use super::object::{to_boolean_object, Object, OBJECT_FALSE, OBJECT_NULL, OBJECT_TRUE};

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(program) => eval_program(&program),
        Node::Statement(statement) => eval_statement(&statement),
        Node::Expression(expression) => eval_expression(&expression),
    }
}

// -----------------------------------------------------------------------------
// E V A L U A T I N G   P R O G R A M S
// -----------------------------------------------------------------------------
fn eval_program(program: &Program) -> Object {
    let mut object = Object::Null;
    for statement in program.iter() {
        object = eval_statement(statement);
    }
    object
}

// -----------------------------------------------------------------------------
// E V A L U A T I N G   S T A T E M E N T S
// -----------------------------------------------------------------------------
fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression),
        _ => todo!("unsported statement {statement}"),
    }
}

// -----------------------------------------------------------------------------
// E V A L U A T I N G   E X P R E S S I O N S
// -----------------------------------------------------------------------------
fn eval_expression(expression: &Expression) -> Object {
    match expression {
        Expression::Literal(literal) => match literal {
            Literal::Integer(i) => Object::Integer(*i),
            Literal::Bool(b) => to_boolean_object(*b),
        },
        Expression::Prefix(operation, right) => {
            eval_prefix_expression(operation, &eval_expression(right))
        }

        Expression::Infix(operation, left, right) => {
            eval_infix_expression(operation, &eval_expression(left), &eval_expression(right))
        }
        _ => todo!("unsupported expression {expression}"),
    }
}

// P R E F I X   E X P R E S S I O N S
// -----------------------------------
fn eval_prefix_expression(operation: &Token, right: &Object) -> Object {
    match operation {
        Token::Bang => eval_bang_expression(right),
        Token::Minus => eval_minus_expression(right),
        _ => unreachable!("unsupported token {operation}"),
    }
}

fn eval_bang_expression(right: &Object) -> Object {
    match *right {
        OBJECT_TRUE => OBJECT_FALSE,
        OBJECT_FALSE => OBJECT_TRUE,
        OBJECT_NULL => OBJECT_TRUE,
        // TODO: all other objects evaluate to false. Is that correct? 0?
        _ => OBJECT_FALSE,
    }
}

fn eval_minus_expression(right: &Object) -> Object {
    match *right {
        Object::Integer(i) => Object::Integer(-i),
        _ => panic!("error handling needed"),
    }
}

// I N F I X   E X P R E S S I O N S
// -----------------------------------
fn eval_infix_expression(operation: &Token, left: &Object, right: &Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => eval_infix_integer(operation, l, r),
        (Object::Bool(l), Object::Bool(r)) => eval_infix_booleann(operation, l, r),
        _ => todo!(),
    }
}

fn eval_infix_integer(operation: &Token, left: &isize, right: &isize) -> Object {
    match operation {
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
        _ => unreachable!("unexpected operation {operation}"),
    }
}

fn eval_infix_booleann(operation: &Token, left: &bool, right: &bool) -> Object {
    match operation {
        Token::Equal => to_boolean_object(left == right),
        Token::NotEqual => to_boolean_object(left != right),
        _ => unreachable!("unexpected operation {operation}"),
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::parser::parse;

    use super::{eval, Object, OBJECT_FALSE, OBJECT_TRUE};

    fn test_helper(test_case: &[(&str, Object)]) {
        for (input, object) in test_case {
            // TODO: how to handle incorrect programs?
            assert_eq!(&eval(parse(input).expect("parsed succesfully")), object)
        }
    }

    #[test]
    fn test_integer_expression() {
        test_helper(&[
            // Simple integers.
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            // Minus prefixes.
            ("-5", Object::Integer(-5)),
            ("-10", Object::Integer(-10)),
            ("--5", Object::Integer(5)),
            ("--10", Object::Integer(10)),
            // Arithmetic.
            ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            ("-50 + 100 - 50", Object::Integer(0)),
            ("5 * 2 + 10", Object::Integer(20)),
            ("5 + 2 * 10", Object::Integer(25)),
            ("50 / 2 * 2 + 10", Object::Integer(60)),
            ("2 * (5 + 10)", Object::Integer(30)),
            ("3 * 3 * 3 + 10", Object::Integer(37)),
            ("3 * (3 * 3) + 10", Object::Integer(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
        ]);
    }

    #[test]
    fn test_boolean_expression() {
        test_helper(&[
            // Simple booleans.
            ("true", OBJECT_TRUE),
            ("false", OBJECT_FALSE),
            // Integers.
            ("1 < 2", OBJECT_TRUE),
            ("1 > 2", OBJECT_FALSE),
            ("1 < 1", OBJECT_FALSE),
            ("1 > 1", OBJECT_FALSE),
            ("1 == 1", OBJECT_TRUE),
            ("1 != 1", OBJECT_FALSE),
            ("1 == 2", OBJECT_FALSE),
            ("1 != 2", OBJECT_TRUE),
            // Mixed booleans and integers.
            ("true == true", OBJECT_TRUE),
            ("false == false", OBJECT_TRUE),
            ("true == false", OBJECT_FALSE),
            ("false != true", OBJECT_TRUE),
            ("(1 < 2) == true", OBJECT_TRUE),
            ("(1 < 2) == false", OBJECT_FALSE),
            ("(1 > 2) == true", OBJECT_FALSE),
            ("(1 > 2) == false", OBJECT_TRUE),
        ])
    }

    #[test]
    fn test_bang_operator() {
        test_helper(&[
            ("!true", OBJECT_FALSE),
            ("!false", OBJECT_TRUE),
            ("!5", OBJECT_FALSE),
            ("!!true", OBJECT_TRUE),
            ("!!false", OBJECT_FALSE),
            ("!!5", OBJECT_TRUE),
        ]);
    }
}
