use std::rc::Rc;

use crate::{
    ast::{
        expression::{
            ArrayIndex, ArrayLiteral, BooleanExpression, Expression, FunctionCall, FunctionLiteral,
            HashLiteral, Identifier, IfExpression, InfixExpression, InfixOperator, IntegerLiteral,
            PrefixExpression, PrefixOperator,
        },
        program::Program,
        statement::{
            BlockStatement, ExpressionStatement, LetStatement, ReturnStatement, Statement,
        },
    },
    lexer::Lexer,
    token::Token,
};

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    LOWEST = 1,
    EQUALS = 2,      // ==
    LESSGREATER = 3, // > or <
    SUM = 4,         // +
    PRODUCT = 5,     // *
    PREFIX = 6,      // -X or !X
    CALL = 7,        // myFunction(X)
    INDEX = 8,       // myArray[X]
}

impl Precedence {
    fn for_token(token: &Token) -> Self {
        match token {
            Token::EQ | Token::NotEq => Self::EQUALS,
            Token::LT | Token::GT => Self::LESSGREATER,
            Token::Plus | Token::Minus => Self::SUM,
            Token::Slash | Token::Asterisk => Self::PRODUCT,
            Token::Lparen => Self::CALL,
            Token::Lbracket => Self::INDEX,
            _ => Self::LOWEST,
        }
    }
}

pub struct Parser {
    lexer: Lexer,

    errors: Vec<String>,

    curr_token: Rc<Token>,
    peek_token: Rc<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut s = Self {
            lexer,
            errors: Vec::new(),

            curr_token: Rc::new(Token::Eof),
            peek_token: Rc::new(Token::Eof),
        };

        s.next_token();
        s.next_token();

        s
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = vec![];

        while !self.curr_token_is(Token::Eof) {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            };
            self.next_token();
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match *self.curr_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = self.curr_token.clone();

        if !self.expect_peek_ident() {
            return None;
        }

        let identifier = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.to_string(),
        };

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        self.next_token();
        let value = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let(LetStatement {
            token: let_token,
            name: identifier,
            value,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let curr_token = self.curr_token.clone();
        self.next_token();

        let value = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(ReturnStatement {
            token: curr_token,
            value,
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let curr_token = self.curr_token.clone();
        let exp = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Expression(ExpressionStatement {
            token: curr_token,
            expression: exp,
        }))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let curr_token = self.curr_token.clone();
        let mut statements: Vec<Statement> = Vec::new();

        self.next_token();

        while !self.curr_token_is(Token::Rbrace) && !self.curr_token_is(Token::Eof) {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }

            self.next_token();
        }

        BlockStatement {
            token: curr_token,
            statements,
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left = self.parse_prefix_expression();

        if left.is_none() {
            self.errors.push(format!(
                "no prefix parse function for {}",
                self.curr_token.to_string()
            ));
            return None;
        }

        while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_precedence() {
            if !self.peek_token.supports_infix() {
                return left;
            }

            self.next_token();

            // note: left is Some(X) for sure otherwise it would have been catched by
            // `left.is_none()`
            left = self.parse_infix_expression(left.unwrap());
        }

        left
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        match self.curr_token.as_ref() {
            Token::Ident(_) => self.parse_identifier_expression(),
            Token::Int(_) => self.parse_integer_literal_expression(),
            Token::Bang | Token::Minus => self.parse_prefix_modifier_expression(),
            Token::True | Token::False => self.parse_boolean_expression(),
            Token::Lparen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            Token::String(_) => self.parse_string_literal_expression(),
            Token::Lbracket => self.parse_array_literal_expression(),
            Token::Lbrace => self.parse_hash_literal_expression(),
            _ => None,
        }
    }

    fn parse_prefix_modifier_expression(&mut self) -> Option<Expression> {
        let token = self.curr_token.clone();
        let operator = PrefixOperator::from_str(&self.curr_token.to_string())?;

        self.next_token();

        self.parse_expression(Precedence::PREFIX).and_then(|v| {
            Some(Expression::Prefix(PrefixExpression {
                token,
                operator,
                right: Box::new(v),
            }))
        })
    }

    fn parse_string_literal_expression(&mut self) -> Option<Expression> {
        Some(Expression::StringLiteral(self.curr_token.to_string()))
    }

    fn parse_identifier_expression(&mut self) -> Option<Expression> {
        Some(Expression::Identifier(Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.to_string(),
        }))
    }

    fn parse_integer_literal_expression(&mut self) -> Option<Expression> {
        if let Token::Int(integer_literal) = self.curr_token.as_ref() {
            match integer_literal.parse::<i64>() {
                Ok(integer) => Some(Expression::IntegerLiteral(IntegerLiteral {
                    token: self.curr_token.clone(),
                    value: integer,
                })),
                Err(_) => {
                    self.errors
                        .push(format!("could not parse {} as integer", self.curr_token));
                    None
                }
            }
        } else {
            None
        }
    }

    fn parse_boolean_expression(&mut self) -> Option<Expression> {
        Some(Expression::Boolean(BooleanExpression {
            token: self.curr_token.clone(),
            value: self.curr_token_is(Token::True),
        }))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let exp = self.parse_expression(Precedence::LOWEST)?;
        self.expect_peek(Token::Rparen).then_some(exp).or(None)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let curr_token = self.curr_token.clone();

        if !self.expect_peek(Token::Lparen) {
            return None;
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST)?;

        if !self.expect_peek(Token::Rparen) {
            return None;
        }

        if !self.expect_peek(Token::Lbrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token_is(Token::Else) {
            self.next_token();

            if !self.expect_peek(Token::Lbrace) {
                return None;
            }

            Some(self.parse_block_statement())
        } else {
            None
        };

        Some(Expression::If(IfExpression {
            token: curr_token,
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        match self.curr_token.as_ref() {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::EQ
            | Token::NotEq
            | Token::LT
            | Token::GT => self.parse_infix_modifier_expression(left),
            Token::Lparen => self.parse_function_call(left),
            Token::Lbracket => self.parse_array_index(left),
            _ => None,
        }
    }

    fn parse_infix_modifier_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.curr_token.clone();
        let operator = InfixOperator::from_str(&self.curr_token.to_string())?;
        let precedence = self.curr_precedence();

        self.next_token();

        Some(Expression::Infix(InfixExpression {
            token,
            operator,
            left: Box::new(left),
            right: Box::new(self.parse_expression(precedence)?),
        }))
    }

    fn parse_function_call(&mut self, left: Expression) -> Option<Expression> {
        Some(Expression::FunctionCall(FunctionCall {
            token: self.curr_token.clone(),
            function: Box::new(left),
            args: self.parse_expression_list(Token::Rparen)?,
        }))
    }

    fn parse_array_index(&mut self, left: Expression) -> Option<Expression> {
        let curr_token = self.curr_token.clone();
        self.next_token();

        let index = self.parse_expression(Precedence::LOWEST)?;
        if !self.expect_peek(Token::Rbracket) {
            None
        } else {
            Some(Expression::ArrayIndex(ArrayIndex {
                token: curr_token,
                left: Box::new(left),
                index: Box::new(index),
            }))
        }
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        let curr_token = self.curr_token.clone();
        if !self.expect_peek(Token::Lparen) {
            return None;
        }

        let params = self.parse_function_params()?;

        if !self.expect_peek(Token::Lbrace) {
            return None;
        }

        let body = self.parse_block_statement();

        return Some(Expression::FunctionLiteral(FunctionLiteral {
            token: curr_token,
            body,
            params,
        }));
    }

    fn parse_function_params(&mut self) -> Option<Vec<Identifier>> {
        let mut params: Vec<Identifier> = Vec::new();
        if self.peek_token_is(Token::Rparen) {
            self.next_token();
            return Some(params);
        }

        self.next_token();
        params.push(Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.to_string(),
        });

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();

            params.push(Identifier {
                token: self.curr_token.clone(),
                value: self.curr_token.to_string(),
            });
        }

        if !self.expect_peek(Token::Rparen) {
            return None;
        }

        Some(params)
    }

    fn parse_array_literal_expression(&mut self) -> Option<Expression> {
        let curr_token = self.curr_token.clone();
        let elements = self.parse_expression_list(Token::Rbracket)?;
        Some(Expression::ArrayLiteral(ArrayLiteral {
            token: curr_token,
            elements,
        }))
    }

    fn parse_expression_list(&mut self, end_token: Token) -> Option<Vec<Expression>> {
        let mut elements: Vec<Expression> = Vec::new();
        if self.peek_token_is(end_token.clone()) {
            self.next_token();
            return Some(elements);
        };

        self.next_token();
        elements.push(self.parse_expression(Precedence::LOWEST)?);

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            elements.push(self.parse_expression(Precedence::LOWEST)?);
        }

        if !self.expect_peek(end_token) {
            return None;
        }

        Some(elements)
    }

    fn parse_hash_literal_expression(&mut self) -> Option<Expression> {
        let curr_token = self.curr_token.clone();
        let mut pairs = Vec::new();

        while !self.peek_token_is(Token::Rbrace) {
            self.next_token();
            let key = self.parse_expression(Precedence::LOWEST)?;

            if !self.expect_peek(Token::Colon) {
                return None;
            }

            self.next_token();
            let value = self.parse_expression(Precedence::LOWEST)?;
            pairs.push((key, value));
            if !self.peek_token_is(Token::Rbrace) && !self.expect_peek(Token::Comma) {
                return None;
            }
        }

        if !self.expect_peek(Token::Rbrace) {
            return None;
        }

        Some(Expression::HashLiteral(HashLiteral {
            token: curr_token,
            pairs,
        }))
    }

    fn curr_token_is(&self, t: Token) -> bool {
        *self.curr_token == t
    }

    fn peek_token_is(&self, t: Token) -> bool {
        *self.peek_token == t
    }

    fn peek_error(&mut self, t: Token) {
        self.errors.push(format!(
            "next token expected {}, found {}",
            t.to_string(),
            self.peek_token.to_string()
        ));
    }

    fn peek_precedence(&mut self) -> Precedence {
        Precedence::for_token(self.peek_token.as_ref())
    }

    fn curr_precedence(&mut self) -> Precedence {
        Precedence::for_token(self.curr_token.as_ref())
    }

    fn expect_peek_ident(&mut self) -> bool {
        if let Token::Ident(_) = *self.peek_token {
            self.next_token();
            true
        } else {
            // note: this should be changed - we want for the Ident token to print "IDENT"
            self.peek_error(Token::Ident("ident".to_string()));
            false
        }
    }

    fn expect_peek(&mut self, t: Token) -> bool {
        if self.peek_token_is(t.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = Rc::new(self.lexer.next_token());
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::ast::node::NodeTrait;

    use super::*;

    #[test]
    fn test_precedence_enum() {
        assert!(Precedence::LOWEST < Precedence::EQUALS);
        assert!(Precedence::CALL > Precedence::PREFIX);
        assert!(Precedence::LOWEST < Precedence::PREFIX);
    }

    #[test]
    fn test_let_statement() {
        let input = r#"
            let x = 5;
            let y = true;
            let foobar = y;"#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_statements_len(&program, 3);

        let tests = vec![
            ("x", Literal::Int(5)),
            ("y", Literal::Bool(true)),
            ("foobar", Literal::Ident("y")),
        ];
        for (i, (identifier, value)) in tests.into_iter().enumerate() {
            let let_statement = match &program.statements[i] {
                Statement::Let(ls) => ls,
                generic => panic!("expected let statement, found {}", generic.type_string()),
            };

            assert_eq!(let_statement.token.to_string(), Token::Let.to_string());
            assert_eq!(let_statement.name.token.to_string(), identifier);
            check_literal_expression(&let_statement.value, value);
        }
    }

    #[test]
    fn test_return_statement() {
        let input = "
            return 5;
            return true;
            return y;
        ";

        let tests = vec![
            (Literal::Int(5)),
            (Literal::Bool(true)),
            (Literal::Ident("y")),
        ];

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_statements_len(&program, 3);

        for (i, expected) in tests.into_iter().enumerate() {
            let return_statement = match &program.statements[i] {
                Statement::Return(rs) => rs,
                generic => panic!("expected return statement, found {}", generic.type_string()),
            };

            assert_eq!(
                return_statement.token.to_string(),
                Token::Return.to_string()
            );
            check_literal_expression(&return_statement.value, expected);
        }

        for statement in program.statements.iter() {
            assert!(
                matches!(*statement, Statement::Return(_)),
                "expected return statement, found {}",
                statement.token_literal()
            );
        }
    }

    #[test]
    fn test_literal_expression() {
        let tests = [
            ("foobar;", Literal::Ident("foobar")),
            ("5;", Literal::Int(5)),
            ("true;", Literal::Bool(true)),
            ("false;", Literal::Bool(false)),
            ("\"hello world\"", Literal::String("hello world")),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            check_statements_len(&program, 1);

            let statement = match &program.statements[0] {
                Statement::Expression(statement) => statement,
                generic => panic!(
                    "expected expression statement, found: {}",
                    generic.type_string()
                ),
            };

            check_literal_expression(&statement.expression, expected);
        }
    }

    #[test]
    fn test_prefix_expression() {
        let tests = [
            ("!5;", PrefixOperator::Not, Literal::Int(5)),
            ("-15;", PrefixOperator::Minus, Literal::Int(15)),
            ("!true;", PrefixOperator::Not, Literal::Bool(true)),
            ("!false;", PrefixOperator::Not, Literal::Bool(false)),
        ];

        for (input, operator, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            check_statements_len(&program, 1);

            let statement = match &program.statements[0] {
                Statement::Expression(expression) => expression,
                generic => panic!(
                    "expected expression statement, found {}",
                    generic.type_string()
                ),
            };

            let expression = match &statement.expression {
                Expression::Prefix(prefix_expression) => prefix_expression,
                generic => panic!(
                    "expected prefix expression, found {}",
                    generic.type_string()
                ),
            };

            assert_eq!(
                expression.operator, operator,
                "unexpected operator {} for prefix expression, expected: {}",
                expression.operator, operator
            );
            check_literal_expression(&expression.right, expected);
        }
    }

    #[test]
    fn test_infix_expression() {
        let tests = [
            (
                "5 + 5;",
                Literal::Int(5),
                InfixOperator::Plus,
                Literal::Int(5),
            ),
            (
                "5 - 5;",
                Literal::Int(5),
                InfixOperator::Minus,
                Literal::Int(5),
            ),
            (
                "5 * 5;",
                Literal::Int(5),
                InfixOperator::Multiply,
                Literal::Int(5),
            ),
            (
                "5 / 5;",
                Literal::Int(5),
                InfixOperator::Divide,
                Literal::Int(5),
            ),
            (
                "5 > 5;",
                Literal::Int(5),
                InfixOperator::GreaterThan,
                Literal::Int(5),
            ),
            (
                "5 < 5;",
                Literal::Int(5),
                InfixOperator::LessThan,
                Literal::Int(5),
            ),
            (
                "5 == 5;",
                Literal::Int(5),
                InfixOperator::Equal,
                Literal::Int(5),
            ),
            (
                "5 != 5;",
                Literal::Int(5),
                InfixOperator::NotEqual,
                Literal::Int(5),
            ),
            (
                "true == true",
                Literal::Bool(true),
                InfixOperator::Equal,
                Literal::Bool(true),
            ),
            (
                "true != false",
                Literal::Bool(true),
                InfixOperator::NotEqual,
                Literal::Bool(false),
            ),
            (
                "false == false",
                Literal::Bool(false),
                InfixOperator::Equal,
                Literal::Bool(false),
            ),
        ];

        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            check_statements_len(&program, 1);

            let statement = match &program.statements[0] {
                Statement::Expression(statement) => statement,
                generic => panic!(
                    "expected expression statement, found {}",
                    generic.type_string()
                ),
            };

            check_infix_expression(&statement.expression, left, operator, right)
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (index, (input, expected)) in tests.into_iter().enumerate() {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);

            assert_eq!(
                program.to_string(),
                expected,
                "test case {} wrong: {} != {}",
                index,
                program.to_string(),
                expected
            );
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = [
            ("let foobar = true;", "foobar", true),
            ("let barfoo = false;", "barfoo", false),
        ];

        for (input, expected_name, expected_value) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            check_statements_len(&program, 1);

            let statement = match &program.statements[0] {
                Statement::Let(statement) => statement,
                generic => panic!(
                    "expected expression statement, found {}",
                    generic.type_string()
                ),
            };

            assert_eq!(statement.name.value, expected_name);
            check_boolean_literal(&statement.value, expected_value);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_statements_len(&program, 1);

        let statement = match &program.statements[0] {
            Statement::Expression(expression) => expression,
            generic => panic!(
                "expected expression statement, found {}",
                generic.type_string()
            ),
        };

        let expression = match &statement.expression {
            Expression::If(if_expression) => if_expression,
            generic => panic!("expected if expression, found {}", generic.type_string()),
        };

        check_infix_expression(
            &expression.condition,
            Literal::Ident("x"),
            InfixOperator::LessThan,
            Literal::Ident("y"),
        );

        assert!(
            expression.consequence.statements.len() == 1,
            "consequence is not 1 statements. got={}",
            expression.consequence.statements.len()
        );

        let consequence_expression = match &expression.consequence.statements[0] {
            Statement::Expression(statement) => statement,
            generic => panic!(
                "expected expression statement, found {}",
                generic.type_string()
            ),
        };

        check_ident_literal(&consequence_expression.expression, "x");
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_statements_len(&program, 1);

        let statement = match &program.statements[0] {
            Statement::Expression(statement) => statement,
            generic => panic!(
                "expected expression statement, found {}",
                generic.type_string()
            ),
        };

        let if_expression = match &statement.expression {
            Expression::If(expression) => expression,
            generic => panic!("expected if expression, found {}", generic.type_string()),
        };

        check_infix_expression(
            &if_expression.condition,
            Literal::Ident("x"),
            InfixOperator::LessThan,
            Literal::Ident("y"),
        );

        assert!(
            if_expression.consequence.statements.len() == 1,
            "consequence is not 1 statements. got={}",
            if_expression.consequence.statements.len()
        );

        let consequence_statement = match &if_expression.consequence.statements[0] {
            Statement::Expression(statement) => statement,
            generic => panic!(
                "expected expression statement, found {}",
                generic.type_string()
            ),
        };
        check_ident_literal(&consequence_statement.expression, "x");

        let alternative_statement = match &if_expression.alternative {
            Some(block) => block,
            _ => panic!("alternative block not found, expected it to be there"),
        };

        assert!(
            alternative_statement.statements.len() == 1,
            "consequence is not 1 statements. got={}",
            alternative_statement.statements.len()
        );

        let alternative_expression = match &alternative_statement.statements[0] {
            Statement::Expression(statement) => statement,
            generic => panic!(
                "expected expression statement, found {}",
                generic.type_string()
            ),
        };
        check_ident_literal(&alternative_expression.expression, "y");
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_statements_len(&program, 1);

        let expression_statement = match &program.statements[0] {
            Statement::Expression(statement) => statement,
            generic => panic!(
                "expected expression statement, found {}",
                generic.type_string()
            ),
        };

        let function_literal_expression = match &expression_statement.expression {
            Expression::FunctionLiteral(expression) => expression,
            generic => panic!(
                "expected function literal expression, found {}",
                generic.type_string()
            ),
        };

        assert_eq!(
            function_literal_expression.params.len(),
            2,
            "wrong number of parameters {}, want={}",
            function_literal_expression.params.len(),
            2
        );

        assert_eq!(&function_literal_expression.params[0].value, "x");
        assert_eq!(&function_literal_expression.params[1].value, "y");

        assert_eq!(
            function_literal_expression.body.statements.len(),
            1,
            "wrong number of body statements {}, want={}",
            function_literal_expression.body.statements.len(),
            1
        );

        let body_expression_statement = match &function_literal_expression.body.statements[0] {
            Statement::Expression(statement) => statement,
            generic => panic!(
                "expected expression statement in body, found {}",
                generic.type_string()
            ),
        };

        check_infix_expression(
            &body_expression_statement.expression,
            Literal::Ident("x"),
            InfixOperator::Plus,
            Literal::Ident("y"),
        );
    }

    #[test]
    fn test_function_params() {
        let tests = [
            ("fn () {};", Vec::new()),
            ("fn (x) {};", vec!["x".to_string()]),
            (
                "fn (x, y, z) {};",
                vec!["x".to_string(), "y".to_string(), "z".to_string()],
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            check_statements_len(&program, 1);

            let statement = match &program.statements[0] {
                Statement::Expression(statement) => statement,
                generic => panic!(
                    "expected expression statement, found {}",
                    generic.type_string()
                ),
            };

            let function_literal_expression = match &statement.expression {
                Expression::FunctionLiteral(expression) => expression,
                generic => panic!(
                    "expected function literal expression, found {}",
                    generic.type_string()
                ),
            };

            assert_eq!(
                function_literal_expression.params.len(),
                expected.len(),
                "length params wrong. want={}, got={}",
                expected.len(),
                function_literal_expression.params.len()
            );

            for (i, expected_identifier) in expected.iter().enumerate() {
                assert_eq!(
                    function_literal_expression.params[i].value,
                    expected_identifier.to_string(),
                    "wrong param value at index {}, want={}, got={}",
                    i,
                    expected_identifier,
                    function_literal_expression.params[i].value
                );
            }
        }
    }

    #[test]
    fn test_function_call() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_statements_len(&program, 1);

        let statement = match &program.statements[0] {
            Statement::Expression(statement) => statement,
            generic => panic!(
                "expected expression statement, found {}",
                generic.type_string()
            ),
        };

        let function_call_expression = match &statement.expression {
            Expression::FunctionCall(expression) => expression,
            generic => panic!(
                "expected function call expression, found {}",
                generic.type_string()
            ),
        };

        check_ident_literal(&function_call_expression.function, "add");
        assert_eq!(
            function_call_expression.args.len(),
            3,
            "wrong number of args {}, want={}",
            function_call_expression.args.len(),
            3
        );
        check_literal_expression(&function_call_expression.args[0], Literal::Int(1));
        check_infix_expression(
            &function_call_expression.args[1],
            Literal::Int(2),
            InfixOperator::Multiply,
            Literal::Int(3),
        );
        check_infix_expression(
            &function_call_expression.args[2],
            Literal::Int(4),
            InfixOperator::Plus,
            Literal::Int(5),
        );
    }

    #[test]
    fn test_array_literal() {
        let input = "[1, 2 * 2, 3 + 3];";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_statements_len(&program, 1);

        let statement = match &program.statements[0] {
            Statement::Expression(statement) => statement,
            generic => panic!(
                "expected expression statement, found {}",
                generic.type_string()
            ),
        };

        let array_literal_expression = match &statement.expression {
            Expression::ArrayLiteral(expression) => expression,
            generic => panic!(
                "expected array literal expression, found {}",
                generic.type_string()
            ),
        };

        assert_eq!(
            array_literal_expression.elements.len(),
            3,
            "wrong number of elements {}, want={}",
            array_literal_expression.elements.len(),
            3
        );
        check_integer_literal(&array_literal_expression.elements[0], 1);
        check_infix_expression(
            &array_literal_expression.elements[1],
            Literal::Int(2),
            InfixOperator::Multiply,
            Literal::Int(2),
        );
        check_infix_expression(
            &array_literal_expression.elements[2],
            Literal::Int(3),
            InfixOperator::Plus,
            Literal::Int(3),
        );
    }

    #[test]
    fn test_array_index() {
        let input = "myArray[1 + 1]";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_statements_len(&program, 1);

        let statement = match &program.statements[0] {
            Statement::Expression(statement) => statement,
            generic => panic!(
                "expected expression statement, found {}",
                generic.type_string()
            ),
        };

        let array_index_expression = match &statement.expression {
            Expression::ArrayIndex(expression) => expression,
            generic => panic!(
                "expected array index expression, found {}",
                generic.type_string()
            ),
        };

        check_ident_literal(&array_index_expression.left, "myArray");
        check_infix_expression(
            &array_index_expression.index,
            Literal::Int(1),
            InfixOperator::Plus,
            Literal::Int(1),
        );
    }

    #[test]
    fn test_hash_literal() {
        let tests = [
            ("{}", vec![]),
            (
                "{\"one\": 1, \"two\": 2, \"three\": 3}",
                vec![("one", 1), ("two", 2), ("three", 3)],
            ),
        ];

        for (idx, (input, expected)) in tests.iter().enumerate() {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);
            check_statements_len(&program, 1);

            let statement = match &program.statements[0] {
                Statement::Expression(statement) => statement,
                generic => panic!(
                    "[{}] expected expression statement, found {}",
                    idx,
                    generic.type_string()
                ),
            };

            let hash_literal_expression = match &statement.expression {
                Expression::HashLiteral(expression) => expression,
                generic => panic!(
                    "[{}] expected hash literal expression, found {}",
                    idx,
                    generic.type_string()
                ),
            };

            assert_eq!(
                hash_literal_expression.pairs.len(),
                expected.len(),
                "[{}] wrong number of keys found {}, want={}",
                idx,
                hash_literal_expression.pairs.len(),
                expected.len()
            );

            for (i, (exp_key, exp_value)) in expected.into_iter().enumerate() {
                let (key, value) = &hash_literal_expression.pairs[i];
                check_string_literal(key, exp_key);
                check_integer_literal(value, *exp_value);
            }
        }
    }

    #[test]
    fn test_hash_literal_with_expressions() {
        let input = "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_statements_len(&program, 1);

        let statement = match &program.statements[0] {
            Statement::Expression(statement) => statement,
            generic => panic!(
                "expected expression statement, found {}",
                generic.type_string()
            ),
        };

        let hash_literal_expression = match &statement.expression {
            Expression::HashLiteral(expression) => expression,
            generic => panic!(
                "expected hash literal expression, found {}",
                generic.type_string()
            ),
        };

        let expected: Vec<(&str, Box<dyn Fn(&Expression)>)> = vec![
            (
                "one",
                Box::new(|exp: &Expression| {
                    check_infix_expression(
                        &exp,
                        Literal::Int(0),
                        InfixOperator::Plus,
                        Literal::Int(1),
                    )
                }),
            ),
            (
                "two",
                Box::new(|exp: &Expression| {
                    check_infix_expression(
                        &exp,
                        Literal::Int(10),
                        InfixOperator::Minus,
                        Literal::Int(8),
                    )
                }),
            ),
            (
                "three",
                Box::new(|exp: &Expression| {
                    check_infix_expression(
                        &exp,
                        Literal::Int(15),
                        InfixOperator::Divide,
                        Literal::Int(5),
                    )
                }),
            ),
        ];

        for (i, (key, value_checker)) in expected.into_iter().enumerate() {
            let (exp_key, exp_value) = &hash_literal_expression.pairs[i];
            check_string_literal(exp_key, key);
            value_checker(exp_value);
        }
    }

    enum Literal<'a> {
        Ident(&'a str),
        Int(i64),
        Bool(bool),
        String(&'a str),
    }

    fn check_infix_expression(
        exp: &Expression,
        left: Literal,
        operator: InfixOperator,
        right: Literal,
    ) {
        let infix_expression = match exp {
            Expression::Infix(infix) => infix,
            generic => panic!(
                "expected infix expression, found: {}",
                generic.type_string()
            ),
        };

        check_literal_expression(&infix_expression.left, left);
        assert_eq!(
            infix_expression.operator, operator,
            "wrong infix operator {}, want={}",
            &infix_expression.operator, operator
        );
        check_literal_expression(&infix_expression.right, right);
    }

    fn check_literal_expression(exp: &Expression, expected: Literal) {
        match expected {
            Literal::Ident(expected) => check_ident_literal(exp, expected),
            Literal::Int(expected) => check_integer_literal(exp, expected),
            Literal::Bool(expected) => check_boolean_literal(exp, expected),
            Literal::String(expected) => check_string_literal(exp, expected),
        }
    }

    fn check_ident_literal(exp: &Expression, expected: &str) {
        let identifier = match exp {
            Expression::Identifier(identifier) => identifier,
            generic => panic!("expected ident literal, found {}", generic.type_string()),
        };

        assert_eq!(identifier.value, expected);
        assert_eq!(identifier.token_literal(), expected);
    }

    fn check_boolean_literal(exp: &Expression, expected: bool) {
        let boolean_expression = match exp {
            Expression::Boolean(boolean) => boolean,
            generic => panic!("expected integer literal, found {}", generic.type_string()),
        };

        assert_eq!(boolean_expression.value, expected);
        assert_eq!(boolean_expression.token_literal(), expected.to_string());
    }

    fn check_integer_literal(exp: &Expression, expected: i64) {
        let integer_literal = match exp {
            Expression::IntegerLiteral(integer_literal) => integer_literal,
            generic => panic!("expected integer literal, found: {}", generic.type_string()),
        };

        assert_eq!(
            integer_literal.value, expected,
            "unexpected integer literal value {}, expected: {}",
            integer_literal.value, expected
        );
        assert_eq!(
            integer_literal.token_literal(),
            expected.to_string(),
            "unexpected integer literal token {}, expected: {}",
            integer_literal.token_literal(),
            expected.to_string()
        );
    }

    fn check_string_literal(string_literal: &Expression, expected: &str) {
        let string_literal = match string_literal {
            Expression::StringLiteral(string_literal) => string_literal,
            generic => panic!("expected string literal, found: {}", generic.type_string()),
        };

        assert_eq!(
            string_literal, expected,
            "unexpected string literal {}, expected: {}",
            string_literal, expected
        );
    }

    fn check_statements_len(program: &Program, wanted: usize) {
        assert_eq!(
            program.statements.len(),
            wanted,
            "expected {} statements, found {}",
            wanted,
            program.statements.len()
        );
    }

    fn check_parser_errors(parser: &Parser) {
        if parser.errors.is_empty() {
            return;
        }

        eprintln!("parser has {} errors:", parser.errors.len());
        for msg in &parser.errors {
            eprintln!("\tparser error: {}", msg);
        }

        panic!("test failed because the parser found errors")
    }
}
