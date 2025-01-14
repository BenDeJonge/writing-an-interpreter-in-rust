use crate::lexing::ast::{Expression, Literal, Node, Program, Statement};

use super::object::{Object, OBJECT_BOOL_FALSE, OBJECT_BOOL_TRUE};

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(program) => eval_program(&program),
        Node::Statement(statement) => eval_statement(&statement),
        Node::Expression(expression) => eval_expression(&expression),
    }
}

fn eval_program(program: &Program) -> Object {
    let mut object = Object::Null;
    for statement in program.iter() {
        object = eval_statement(statement);
    }
    object
}

fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression),
        _ => todo!("only expression statements are supported"),
    }
}

fn eval_expression(expression: &Expression) -> Object {
    match expression {
        Expression::Literal(literal) => match literal {
            Literal::Integer(i) => Object::Integer(*i),
            Literal::Bool(b) => match b {
                true => OBJECT_BOOL_TRUE,
                false => OBJECT_BOOL_FALSE,
            },
        },
        _ => todo!("only literal expressions are supported"),
    }
}

mod tests {
    use crate::parsing::parser::parse;

    use super::{eval, Object};

    fn test_helper(test_case: &[(&str, Object)]) {
        for (input, object) in test_case {
            // TODO: how to handle incorrect programs?
            assert_eq!(&eval(parse(input).expect("parsed succesfully")), object)
        }
    }

    #[test]
    fn test_integer_expression() {
        test_helper(&[
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            ("65535", Object::Integer(65535)),
        ]);
    }

    #[test]
    fn test_boolean_expression() {
        test_helper(&[("true", Object::Bool(true)), ("false", Object::Bool(false))])
    }
}
