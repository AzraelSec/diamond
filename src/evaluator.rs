use std::{ops::Index, rc::Rc};

use crate::{
    ast::{
        expression::{
            ArrayIndex, ArrayLiteral, Expression, FunctionCall, FunctionLiteral, Identifier,
            IfExpression, InfixExpression, InfixOperator, PrefixExpression, PrefixOperator,
        },
        node::Node,
        program::Program,
        statement::{BlockStatement, LetStatement, ReturnStatement, Statement},
    },
    environment::Environment,
    object::{
        builtins::get_builtin,
        object::{ErrorObject, FunctionObject, Object},
    },
};

pub fn eval(node: Node, env: &mut Environment) -> Object {
    match node {
        Node::Expression(expression) => eval_expression(expression, env),
        Node::Statement(statement) => eval_statement(statement, env),
        Node::Program(program) => eval_program(program, env),
    }
}

fn eval_statement(statement: Statement, env: &mut Environment) -> Object {
    match statement {
        Statement::Expression(expression_statement) => {
            eval_expression(expression_statement.expression, env)
        }
        Statement::Let(statement) => eval_let_statement(statement, env),
        Statement::Block(block) => eval_block_statement(block, env),
        Statement::Return(return_statement) => eval_return_statement(return_statement, env),
    }
}

fn eval_let_statement(statement: LetStatement, env: &mut Environment) -> Object {
    let val = eval(Node::Expression(statement.value), env);
    if matches!(val, Object::Error(_)) {
        return val;
    }
    env.set(statement.name.value, val.clone());

    // note: we could return the value here, but we'd need to define let as an expression
    Object::Null
}

fn eval_expression(expression: Expression, env: &mut Environment) -> Object {
    match expression {
        Expression::IntegerLiteral(integer_literal) => Object::Integer(integer_literal.value),
        Expression::Boolean(boolean) => Object::Boolean(boolean.value),
        Expression::StringLiteral(string_literal) => Object::String(string_literal),
        Expression::Prefix(prefix) => eval_prefix_expression(prefix, env),
        Expression::Infix(infix) => eval_infix_expression(infix, env),
        Expression::If(conditional) => eval_conditional_expression(conditional, env),
        Expression::Identifier(identifier) => eval_identifier_expression(identifier, env),
        Expression::FunctionLiteral(function) => eval_function_literal(function, env),
        Expression::FunctionCall(function_call) => eval_function_call(function_call, env),
        Expression::ArrayLiteral(array_literal) => eval_array_literal(array_literal, env),
        Expression::ArrayIndex(array_index) => eval_array_index(array_index, env),
    }
}

fn eval_expressions(expressions: Vec<Expression>, env: &mut Environment) -> Vec<Object> {
    let mut objects: Vec<Object> = Vec::new();
    for expression in expressions {
        let object = eval_expression(expression, env);
        if matches!(object, Object::Error(_)) {
            return Vec::from([object]);
        }
        objects.push(object);
    }
    objects
}

fn eval_identifier_expression(identifier: Identifier, env: &mut Environment) -> Object {
    if let Some(val) = env.get(&identifier.value) {
        return val.clone();
    }
    if let Some(fun) = get_builtin(&identifier.value) {
        return fun;
    }
    Object::Error(ErrorObject::IdentifierNotFound(identifier.value))
}

fn eval_prefix_expression(expression: PrefixExpression, env: &mut Environment) -> Object {
    let right = eval(Node::Expression(*expression.right), env);
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

fn eval_infix_expression(expression: InfixExpression, env: &mut Environment) -> Object {
    let (left, operator, right) = (
        eval(Node::Expression(*expression.left), env),
        expression.operator,
        eval(Node::Expression(*expression.right), env),
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
        (Object::String(left), Object::String(right)) => {
            Object::String(format!("{}{}", left, right))
        }
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

fn eval_conditional_expression(expression: IfExpression, env: &mut Environment) -> Object {
    let condition = eval(Node::Expression(*expression.condition), env);
    if matches!(condition, Object::Error(_)) {
        return condition;
    }

    if let Object::Boolean(true) | Object::Integer(1..=9) = condition {
        return eval(
            Node::Statement(Statement::Block(expression.consequence)),
            env,
        );
    }
    if let Some(alternative) = expression.alternative {
        return eval(Node::Statement(Statement::Block(alternative)), env);
    }
    Object::Null
}

fn eval_function_literal(function: FunctionLiteral, env: &mut Environment) -> Object {
    Object::Function(FunctionObject {
        params: function.params,
        body: function.body,
        // note: should this be a copy?
        env: env.clone(),
    })
}

fn eval_function_call(function_call: FunctionCall, env: &mut Environment) -> Object {
    let function = eval_expression(*function_call.function, env);
    if matches!(function, Object::Error(_)) {
        return function;
    }

    let params = eval_expressions(function_call.args, env);
    if params.len() == 1 && matches!(params[0], Object::Error(_)) {
        return params[0].clone();
    };

    if let Object::Function(function_obj) = function {
        let mut extended_env = extend_function_env(function_obj.clone(), params);
        let evaluated = eval(
            Node::Statement(Statement::Block(function_obj.body)),
            &mut extended_env,
        );

        return if let Object::Return(return_object) = evaluated {
            *return_object
        } else {
            evaluated
        };
    }
    if let Object::BuiltIn(builtin_function) = function {
        return builtin_function(params);
    }
    Object::Error(ErrorObject::CallOnNonFunction(function.get_type()))
}

fn extend_function_env(function: FunctionObject, args: Vec<Object>) -> Environment {
    let mut env = Environment::from_outer(Rc::new(function.env));
    for (i, arg) in function.params.into_iter().enumerate() {
        env.set(arg.value, args[i].clone());
    }
    env
}

fn eval_program(program: Program, env: &mut Environment) -> Object {
    let mut obj = Object::Null;
    for statement in program.statements {
        obj = eval(Node::Statement(statement), env);

        // note: early return in case of return statement
        match obj {
            Object::Return(return_object) => return *return_object,
            Object::Error(error_object) => return Object::Error(error_object),
            _ => (),
        }
    }
    obj
}

fn eval_block_statement(block: BlockStatement, env: &mut Environment) -> Object {
    let mut obj = Object::Null;
    for statement in block.statements {
        obj = eval(Node::Statement(statement), env);

        // note: early return without evaluating the return value
        if matches!(obj, Object::Return(_) | Object::Error(_)) {
            return obj;
        }
    }
    obj
}

fn eval_return_statement(statement: ReturnStatement, env: &mut Environment) -> Object {
    let val = eval(Node::Expression(statement.value), env);
    if matches!(val, Object::Error(_)) {
        val
    } else {
        Object::Return(Box::new(val))
    }
}

fn eval_array_literal(array: ArrayLiteral, env: &mut Environment) -> Object {
    let elements = eval_expressions(array.elements, env);
    if elements.len() == 1 && matches!(elements[0], Object::Error(_)) {
        return elements[0].clone();
    };
    Object::Array(elements)
}

fn eval_array_index(expression: ArrayIndex, env: &mut Environment) -> Object {
    let left = eval(Node::Expression(*expression.left), env);
    if matches!(left, Object::Error(_)) {
        return left;
    }

    let index = eval(Node::Expression(*expression.index), env);
    if matches!(index, Object::Error(_)) {
        return index;
    }

    match (left, index) {
        (Object::Array(left), Object::Integer(index)) => {
            let max = if left.is_empty() {
                -1
            } else {
                (left.len() - 1) as i64
            };

            if index < 0 || index > max {
                Object::Null
            } else {
                left[index as usize].clone()
            }
        }
        (left, index) => Object::Error(ErrorObject::Generic(format!(
            "array index not supported for {}[{}]",
            left.get_type(),
            index.get_type()
        ))),
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{
        lexer::Lexer,
        object::object::{ErrorObject, Object, ObjectType},
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
    fn test_eval_string_literal() {
        check_string_object(check_eval("\"Hello world!\""), "Hello world!");
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
            (
                "foobar",
                ErrorObject::IdentifierNotFound("foobar".to_string()),
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

    #[test]
    fn test_let_statement() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];
        for (input, expected) in tests {
            check_integer_object(check_eval(input), expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = check_eval(input);

        let fn_obj = match evaluated {
            Object::Function(obj) => obj,
            obj => panic!(
                "unexpected object type {}, want={}",
                obj.get_type(),
                ObjectType::Function
            ),
        };

        assert_eq!(
            fn_obj.params.len(),
            1,
            "wrong number of params {}, want={}",
            fn_obj.params.len(),
            1
        );

        assert_eq!(
            fn_obj.params[0].value, "x",
            "wrong param name {}, want={}",
            fn_obj.params[0].value, "x"
        );

        assert_eq!(
            fn_obj.body.to_string(),
            "(x + 2)",
            "unexpected body {}, want={}",
            fn_obj.body,
            "(x + 2)"
        );
    }

    #[test]
    fn test_function_call() {
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, result) in tests {
            let obj = check_eval(input);
            check_integer_object(obj, result);
        }
    }

    #[test]
    fn test_function_closure() {
        let input = "
        let newAdder = fn(x) {
            fn(y) { x + y };
        };
        let addTwo = newAdder(2);
        addTwo(2);`
        ";
        check_integer_object(check_eval(input), 4);
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"world!\"";
        check_string_object(check_eval(input), "Hello world!");
    }

    #[test]
    fn test_builtin_function() {
        let tests = [
            ("len(\"\")", Object::Integer(0)),
            ("len(\"four\")", Object::Integer(4)),
            (
                "len(1)",
                Object::Error(ErrorObject::Generic(
                    "len builtin does not support type integer".to_string(),
                )),
            ),
            (
                "len(\"one\", \"tw\")",
                Object::Error(ErrorObject::Generic(
                    "wrong number of params: expected=1, got=2".to_string(),
                )),
            ),
            ("len([1, 2, 3])", Object::Integer(3)),
            ("len([])", Object::Integer(0)),
            ("first([1, 2, 3])", Object::Integer(1)),
            ("first([])", Object::Null),
            ("last([1, 2, 3])", Object::Integer(3)),
            ("last([])", Object::Null),
            (
                "push([1, 2, 3], 4)",
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                ]),
            ),
            ("push([], 1)", Object::Array(vec![Object::Integer(1)])),
        ];

        for (input, result) in tests {
            let obj = check_eval(input);
            match result {
                Object::Integer(result) => check_integer_object(obj, result),
                Object::Array(array) => check_array_object(obj, array),
                Object::Error(error) => {
                    let obj = match obj {
                        Object::Error(obj) => obj,
                        _ => panic!(
                            "unexpected object type {}, want={}",
                            obj.get_type(),
                            ObjectType::Error
                        ),
                    };
                    assert_eq!(
                        obj.to_string(),
                        error.to_string(),
                        "error message mismatch. want={}, got={}",
                        error.to_string(),
                        obj.to_string()
                    );
                }
                Object::Null => assert!(matches!(obj, Object::Null)),
                s => panic!("unexpected object {}", s),
            }
        }
    }

    #[test]
    fn test_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = check_eval(input);
        let array = match evaluated {
            Object::Array(array) => array,
            obj => panic!(
                "unexpected object type {}, want={}",
                obj.get_type(),
                ObjectType::Array
            ),
        };

        assert_eq!(
            array.len(),
            3,
            "wrong number of elements {}, want={}",
            array.len(),
            3
        );

        check_integer_object(array[0].clone(), 1);
        check_integer_object(array[1].clone(), 4);
        check_integer_object(array[2].clone(), 6);
    }

    #[test]
    fn test_array_index() {
        let tests = [
            ("[1, 2, 3][0]", Literal::Int(1)),
            ("[1, 2, 3][1]", Literal::Int(2)),
            ("[1, 2, 3][2]", Literal::Int(3)),
            ("let i = 0; [1][i];", Literal::Int(1)),
            ("[1, 2, 3][1 + 1];", Literal::Int(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Literal::Int(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Literal::Int(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Literal::Int(2),
            ),
            ("[1, 2, 3][3]", Literal::Null),
            ("[1, 2, 3][-1]", Literal::Null),
        ];

        for (input, expected) in tests {
            let obj = check_eval(input);
            match expected {
                Literal::Int(expected) => check_integer_object(obj, expected),
                Literal::Null => assert!(matches!(obj, Object::Null)),
                _ => panic!("unexpected object {}", obj),
            }
        }
    }

    fn check_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        eval(Node::Program(program), &mut Environment::new())
    }

    fn check_integer_object(obj: Object, expected: i64) {
        let obj = match obj {
            Object::Integer(obj) => obj,
            Object::Error(error) => panic!("expected IntegerObject, got error: {}", error),
            _ => panic!("expected IntegerObject, got: {}", obj.get_type()),
        };
        assert_eq!(obj, expected, "expected={}, got={}", expected, obj);
    }

    fn check_boolean_object(obj: Object, expected: bool) {
        let obj = match obj {
            Object::Boolean(obj) => obj,
            Object::Error(error) => panic!("expected BooleanObject, got error: {}", error),
            _ => panic!("expected BooleanObject, got: {}", obj.get_type()),
        };
        assert_eq!(obj, expected, "expected={}, got={}", expected, obj);
    }

    fn check_string_object(obj: Object, expected: &str) {
        let obj = match obj {
            Object::String(obj) => obj,
            Object::Error(error) => panic!("expected StringObject, got error: {}", error),
            _ => panic!("expected StringObject, got: {}", obj.get_type()),
        };
        assert_eq!(obj, expected, "expected={}, got={}", expected, obj);
    }

    fn check_array_object(obj: Object, expected: Vec<Object>) {
        let obj = match obj {
            Object::Array(obj) => obj,
            Object::Error(error) => panic!("expected ArrayObject, got error: {}", error),
            _ => panic!("expected ArrayObject, got: {}", obj.get_type()),
        };
        assert_eq!(obj, expected, "expected={:?}, got={:?}", expected, obj);
    }
}
