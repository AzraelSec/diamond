use crate::{
    ast::{
        expression::{
            Expression, IfExpression, InfixExpression, InfixOperator, PrefixExpression,
            PrefixOperator,
        },
        node::Node,
        statement::Statement,
    },
    object::Object,
};

pub fn eval(node: Node) -> Option<Object> {
    match node {
        Node::Expression(expression) => eval_expression(expression),
        Node::Statement(statement) => eval_statement(statement),
        Node::Program(program) => eval_statements(program.statements),
    }
}

fn eval_statement(statement: Statement) -> Option<Object> {
    match statement {
        Statement::Expression(expression_statement) => {
            eval_expression(expression_statement.expression)
        }
        Statement::Block(block) => eval_statements(block.statements),
        _ => None,
    }
}

fn eval_expression(expression: Expression) -> Option<Object> {
    match expression {
        Expression::IntegerLiteral(integer_literal) => Some(Object::Integer(integer_literal.value)),
        Expression::Boolean(boolean) => Some(Object::Boolean(boolean.value)),
        Expression::Prefix(prefix) => eval_prefix_expression(prefix),
        Expression::Infix(infix) => eval_infix_expression(infix),
        Expression::If(conditional) => eval_conditional_expression(conditional),
        _ => None,
    }
}

fn eval_prefix_expression(expression: PrefixExpression) -> Option<Object> {
    let right = eval(Node::Expression(*expression.right))?;
    match expression.operator {
        PrefixOperator::Not => eval_bang_operator_expression(right),
        PrefixOperator::Minus => eval_minus_prefix_expression(right),
    }
}

fn eval_bang_operator_expression(right: Object) -> Option<Object> {
    match right {
        Object::Boolean(val) => Some(Object::Boolean(!val)),
        Object::Null => Some(Object::Boolean(true)),
        Object::Integer(integer) => Some(Object::Boolean(integer == 0)),
    }
}

fn eval_minus_prefix_expression(right: Object) -> Option<Object> {
    match right {
        Object::Integer(val) => Some(Object::Integer(-val)),
        _ => None,
    }
}

fn eval_infix_expression(expression: InfixExpression) -> Option<Object> {
    let (left, operator, right) = (
        eval(Node::Expression(*expression.left))?,
        expression.operator,
        eval(Node::Expression(*expression.right))?,
    );
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

fn eval_plus_infix_expression(left: Object, right: Object) -> Option<Object> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Some(Object::Integer(left + right)),
        _ => None,
    }
}

fn eval_minus_infix_expression(left: Object, right: Object) -> Option<Object> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Some(Object::Integer(left - right)),
        _ => None,
    }
}

fn eval_multiply_infix_expression(left: Object, right: Object) -> Option<Object> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Some(Object::Integer(left * right)),
        _ => None,
    }
}

fn eval_divide_infix_expression(left: Object, right: Object) -> Option<Object> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => {
            if right == 0 {
                None
            } else {
                Some(Object::Integer(left / right))
            }
        }
        _ => None,
    }
}

fn eval_lt_infix_expression(left: Object, right: Object) -> Option<Object> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Some(Object::Boolean(left < right)),
        _ => None,
    }
}

fn eval_gt_infix_expression(left: Object, right: Object) -> Option<Object> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Some(Object::Boolean(left > right)),
        _ => None,
    }
}

fn eval_equal_infix_expression(left: Object, right: Object) -> Option<Object> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Some(Object::Boolean(left == right)),
        (Object::Boolean(left), Object::Boolean(right)) => Some(Object::Boolean(left == right)),
        _ => None,
    }
}

fn eval_not_equal_infix_expression(left: Object, right: Object) -> Option<Object> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Some(Object::Boolean(left != right)),
        (Object::Boolean(left), Object::Boolean(right)) => Some(Object::Boolean(left != right)),
        _ => None,
    }
}

fn eval_conditional_expression(expression: IfExpression) -> Option<Object> {
    let condition = eval(Node::Expression(*expression.condition))?;
    if let Object::Boolean(true) | Object::Integer(1..=9) = condition {
        return eval(Node::Statement(Statement::Block(expression.consequence)));
    }
    if let Some(alternative) = expression.alternative {
        return eval(Node::Statement(Statement::Block(alternative)));
    }
    Some(Object::Null)
}

fn eval_statements(statements: Vec<Statement>) -> Option<Object> {
    let mut obj = None;
    for statement in statements {
        obj = eval(Node::Statement(statement));
    }
    obj
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, object::Object, parser::Parser};

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
            let evaluated = assert_some_object(check_eval(input));
            check_integer_object(evaluated, expected);
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
            let evaluated = assert_some_object(check_eval(input));
            check_boolean_object(evaluated, expected);
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
            let evaluated = assert_some_object(check_eval(input));
            check_boolean_object(evaluated, expected);
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
            let evaluated = assert_some_object(check_eval(input));
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

    fn check_eval(input: &str) -> Option<Object> {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        eval(Node::Program(program))
    }

    fn assert_some_object(obj: Option<Object>) -> Object {
        match obj {
            Some(obj) => obj,
            _ => panic!("unexpected null object"),
        }
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
