use std::result;

use crate::{
    ast::{
        expression::{
            Expression, IfExpression, InfixExpression, InfixOperator, PrefixExpression,
            PrefixOperator,
        },
        node::Node,
        program::Program,
        statement::{BlockStatement, ReturnStatement, Statement},
    },
    object::{ErrorObject, Object},
};

pub fn eval(node: Node) -> Object {
    match node {
        Node::Expression(expression) => eval_expression(expression),
        Node::Statement(statement) => eval_statement(statement),
        Node::Program(program) => eval_program(program),
    }
}

fn eval_statement(statement: Statement) -> Object {
    match statement {
        Statement::Expression(expression_statement) => {
            eval_expression(expression_statement.expression)
        }
        Statement::Block(block) => eval_block_statement(block),
        Statement::Return(return_statement) => eval_return_statement(return_statement),
        statement => Object::Error(ErrorObject::Generic(format!(
            "statement type not yet implemented: {}",
            statement.type_string()
        ))),
    }
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::IntegerLiteral(integer_literal) => Object::Integer(integer_literal.value),
        Expression::Boolean(boolean) => Object::Boolean(boolean.value),
        Expression::Prefix(prefix) => eval_prefix_expression(prefix),
        Expression::Infix(infix) => eval_infix_expression(infix),
        Expression::If(conditional) => eval_conditional_expression(conditional),
        exp => Object::Error(ErrorObject::Generic(format!(
            "expression type not yet implemented: {}",
            exp.type_string()
        ))),
    }
}

fn eval_prefix_expression(expression: PrefixExpression) -> Object {
    let right = eval(Node::Expression(*expression.right));
    if matches!(right, Object::Error(_)) {
        return right;
    }
    match expression.operator {
        PrefixOperator::Not => eval_bang_operator_expression(right),
        PrefixOperator::Minus => eval_minus_prefix_expression(right),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(val) => Object::Boolean(!val),
        Object::Null => Object::Boolean(true),
        Object::Integer(integer) => Object::Boolean(integer == 0),
        obj => Object::Error(ErrorObject::UnknownPrefixOperator(
            PrefixOperator::Not,
            obj.get_type(),
        )),
    }
}

fn eval_minus_prefix_expression(right: Object) -> Object {
    match right {
        Object::Integer(val) => Object::Integer(-val),
        obj => Object::Error(ErrorObject::UnknownPrefixOperator(
            PrefixOperator::Minus,
            obj.get_type(),
        )),
    }
}

fn eval_infix_expression(expression: InfixExpression) -> Object {
    let (left, operator, right) = (
        eval(Node::Expression(*expression.left)),
        expression.operator,
        eval(Node::Expression(*expression.right)),
    );

    if matches!(left, Object::Error(_)) {
        return left;
    }
    if matches!(right, Object::Error(_)) {
        return right;
    }

    match operator {
        InfixOperator::Plus => eval_plus_infix_expression(left, right),
        InfixOperator::Minus => eval_minus_infix_expression(left, right),
        InfixOperator::Multiply => eval_multiply_infix_expression(left, right),
        InfixOperator::Divide => eval_divide_infix_expression(left, right),
        InfixOperator::LessThan => eval_lt_infix_expression(left, right),
        InfixOperator::GreaterThan => eval_gt_infix_expression(left, right),
        InfixOperator::Equal => eval_equal_infix_expression(left, right),
        InfixOperator::NotEqual => eval_not_equal_infix_expression(left, right),
    }
}

fn eval_plus_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Object::Integer(left + right),
        (left, right) => Object::Error(ErrorObject::UnknownInfixOperator(
            left.get_type(),
            InfixOperator::Plus,
            right.get_type(),
        )),
    }
}

fn eval_minus_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Object::Integer(left - right),
        (left, right) => Object::Error(ErrorObject::UnknownInfixOperator(
            left.get_type(),
            InfixOperator::Minus,
            right.get_type(),
        )),
    }
}

fn eval_multiply_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Object::Integer(left * right),
        (left, right) => Object::Error(ErrorObject::UnknownInfixOperator(
            left.get_type(),
            InfixOperator::Multiply,
            right.get_type(),
        )),
    }
}

fn eval_divide_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => {
            if right == 0 {
                Object::Error(ErrorObject::DivisionByZero)
            } else {
                Object::Integer(left / right)
            }
        }
        (left, right) => Object::Error(ErrorObject::UnknownInfixOperator(
            left.get_type(),
            InfixOperator::Divide,
            right.get_type(),
        )),
    }
}

fn eval_lt_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left < right),
        (left, right) => Object::Error(ErrorObject::UnknownInfixOperator(
            left.get_type(),
            InfixOperator::LessThan,
            right.get_type(),
        )),
    }
}

fn eval_gt_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left > right),
        (left, right) => Object::Error(ErrorObject::UnknownInfixOperator(
            left.get_type(),
            InfixOperator::GreaterThan,
            right.get_type(),
        )),
    }
}

fn eval_equal_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left == right),
        (Object::Boolean(left), Object::Boolean(right)) => Object::Boolean(left == right),
        (left, right) => Object::Error(ErrorObject::UnknownInfixOperator(
            left.get_type(),
            InfixOperator::Equal,
            right.get_type(),
        )),
    }
}

fn eval_not_equal_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left != right),
        (Object::Boolean(left), Object::Boolean(right)) => Object::Boolean(left != right),
        (left, right) => Object::Error(ErrorObject::UnknownInfixOperator(
            left.get_type(),
            InfixOperator::NotEqual,
            right.get_type(),
        )),
    }
}

fn eval_conditional_expression(expression: IfExpression) -> Object {
    let condition = eval(Node::Expression(*expression.condition));
    if matches!(condition, Object::Error(_)) {
        return condition;
    }

    if let Object::Boolean(true) | Object::Integer(1..=9) = condition {
        return eval(Node::Statement(Statement::Block(expression.consequence)));
    }
    if let Some(alternative) = expression.alternative {
        return eval(Node::Statement(Statement::Block(alternative)));
    }
    Object::Null
}

fn eval_program(program: Program) -> Object {
    let mut obj = Object::Null;
    for statement in program.statements {
        obj = eval(Node::Statement(statement));

        // note: early return in case of return statement
        match obj {
            Object::Return(return_object) => return *return_object,
            Object::Error(error_object) => return Object::Error(error_object),
            _ => (),
        }
    }
    obj
}

fn eval_block_statement(block: BlockStatement) -> Object {
    let mut obj = Object::Null;
    for statement in block.statements {
        obj = eval(Node::Statement(statement));

        // note: early return without evaluating the return value
        if matches!(obj, Object::Return(_) | Object::Error(_)) {
            return obj;
        }
    }
    obj
}

fn eval_return_statement(statement: ReturnStatement) -> Object {
    let val = eval(Node::Expression(statement.value));
    if matches!(val, Object::Error(_)) {
        val
    } else {
        Object::Return(Box::new(val))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Lexer,
        object::{ErrorObject, Object, ObjectType},
        parser::Parser,
    };

    use super::*;

    // todo: this should be shared!
    enum Literal<'a> {
        Ident(&'a str),
        Int(i64),
        Bool(bool),
        Null,
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            check_integer_object(check_eval(input), expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for (input, expected) in tests {
            check_boolean_object(check_eval(input), expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for (input, expected) in tests {
            check_boolean_object(check_eval(input), expected);
        }
    }

    #[test]
    fn test_if_else_expression() {
        let tests = [
            ("if (true) { 10 }", Literal::Int(10)),
            ("if (false) { 10 }", Literal::Null),
            ("if (1) { 10 }", Literal::Int(10)),
            ("if (1 < 2) { 10 }", Literal::Int(10)),
            ("if (1 > 2) { 10 }", Literal::Null),
            ("if (1 > 2) { 10 } else { 20 }", Literal::Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", Literal::Int(10)),
        ];
        for (input, expected) in tests {
            let evaluated = check_eval(input);
            match (&evaluated, expected) {
                (Object::Null, Literal::Null) => {}
                (Object::Null, Literal::Int(_) | Literal::Bool(_) | Literal::Ident(_)) => {
                    panic!("expected null, got: {}", evaluated)
                }
                (evaluated, Literal::Int(expected)) => {
                    check_integer_object(evaluated.clone(), expected)
                }
                _ => panic!("unexpected object {}", evaluated),
            }
        }
    }

    #[test]
    fn test_return_statement() {
        let tests = vec![
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
        ];

        for (input, expected) in tests {
            check_integer_object(check_eval(input), expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = [
            (
                "5 + true;",
                ErrorObject::UnknownInfixOperator(
                    ObjectType::Integer,
                    InfixOperator::Plus,
                    ObjectType::Boolean,
                ),
            ),
            (
                "5 + true; 5;",
                ErrorObject::UnknownInfixOperator(
                    ObjectType::Integer,
                    InfixOperator::Plus,
                    ObjectType::Boolean,
                ),
            ),
            (
                "-true;",
                ErrorObject::UnknownPrefixOperator(PrefixOperator::Minus, ObjectType::Boolean),
            ),
            (
                "true + false;",
                ErrorObject::UnknownInfixOperator(
                    ObjectType::Boolean,
                    InfixOperator::Plus,
                    ObjectType::Boolean,
                ),
            ),
            (
                "5; true + false; 5",
                ErrorObject::UnknownInfixOperator(
                    ObjectType::Boolean,
                    InfixOperator::Plus,
                    ObjectType::Boolean,
                ),
            ),
            (
                "if (10 > 1) { true + false; }",
                ErrorObject::UnknownInfixOperator(
                    ObjectType::Boolean,
                    InfixOperator::Plus,
                    ObjectType::Boolean,
                ),
            ),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }",
                ErrorObject::UnknownInfixOperator(
                    ObjectType::Boolean,
                    InfixOperator::Plus,
                    ObjectType::Boolean,
                ),
            ),
        ];

        for (input, error) in tests {
            let evaluated = match check_eval(input) {
                Object::Error(obj) => obj,
                object => panic!("unexpected object: {}, wanted error", object.get_type()),
            };
            assert_eq!(
                evaluated, error,
                "unexpected error message: '{}', wanted='{}'",
                evaluated, error,
            );
        }
    }

    fn check_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        eval(Node::Program(program))
    }

    fn check_integer_object(obj: Object, expected: i64) {
        let obj = match obj {
            Object::Integer(obj) => obj,
            _ => panic!("expected IntegerObject, got: {}", obj.get_type()),
        };
        assert_eq!(obj, expected, "expected={}, got={}", expected, obj);
    }

    fn check_boolean_object(obj: Object, expected: bool) {
        let obj = match obj {
            Object::Boolean(obj) => obj,
            _ => panic!("expected BooleanObject, got: {}", obj.get_type()),
        };
        assert_eq!(obj, expected, "expected={}, got={}", expected, obj);
    }
}
