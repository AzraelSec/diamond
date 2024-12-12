use std::rc::Rc;

use crate::{
    ast::{
        expression::{
            BooleanExpression, Expression, Identifier, IfExpression, InfixExpression,
            IntegerLiteral, PrefixExpression,
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
}

impl Precedence {
    fn for_token(token: &Token) -> Self {
        match token {
            Token::EQ | Token::NotEq => Self::EQUALS,
            Token::LT | Token::GT => Self::LESSGREATER,
            Token::Plus | Token::Minus => Self::SUM,
            Token::Slash | Token::Asterisk => Self::PRODUCT,
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

        while !self.curr_token_is(Token::Semicolon) {
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

        while !self.curr_token_is(Token::Semicolon) {
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
            _ => None,
        }
    }

    fn parse_prefix_modifier_expression(&mut self) -> Option<Expression> {
        let token = self.curr_token.clone();
        let operator = self.curr_token.to_string();

        self.next_token();

        self.parse_expression(Precedence::PREFIX).and_then(|v| {
            Some(Expression::Prefix(PrefixExpression {
                token,
                operator,
                right: Box::new(v),
            }))
        })
    }

    fn parse_identifier_expression(&mut self) -> Option<Expression> {
        Some(Expression::Identifier(Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.to_string(),
        }))
    }

    fn parse_integer_literal_expression(&mut self) -> Option<Expression> {
        if let Token::Int(integer_literal) = self.curr_token.as_ref() {
            match integer_literal.parse::<u64>() {
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
        self.curr_token
            .supports_infix()
            .then(|| self.parse_infix_modifier_expression(left))?
    }

    fn parse_infix_modifier_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.curr_token.clone();
        let operator = self.curr_token.to_string();
        let precedence = self.curr_precedence();

        self.next_token();

        Some(Expression::Infix(InfixExpression {
            token,
            operator,
            left: Box::new(left),
            right: Box::new(self.parse_expression(precedence)?),
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
            let y = 10;
            let foobar = 838383;"#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_statements_len(&program, 3);

        let tests = vec![("x"), ("y"), ("foobar")];
        for (i, expected_identifier) in tests.iter().enumerate() {
            let statement = &program.statements[i];
            assert!(
                check_let_statement(statement, *expected_identifier),
                "test_let_statement failed for expected_identifier={}",
                expected_identifier
            );
        }
    }

    fn check_let_statement(statement: &Statement, expected_identifier: &str) -> bool {
        if let Statement::Let(ls) = statement {
            if ls.token.to_string() != "let" {
                return false;
            };

            if ls.name.token.to_string() != expected_identifier {
                return false;
            };

            true
        } else {
            false
        }
    }

    #[test]
    fn test_return_statement() {
        let input = "
            return 5;
            return 10;
            return 1337;
        ";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(&parser);
        check_statements_len(&program, 3);

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
            ("!5;", "!", Literal::Int(5)),
            ("-15;", "-", Literal::Int(15)),
            ("!true;", "!", Literal::Bool(true)),
            ("!false;", "!", Literal::Bool(false)),
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
            ("5 + 5;", Literal::Int(5), "+", Literal::Int(5)),
            ("5 - 5;", Literal::Int(5), "-", Literal::Int(5)),
            ("5 * 5;", Literal::Int(5), "*", Literal::Int(5)),
            ("5 / 5;", Literal::Int(5), "/", Literal::Int(5)),
            ("5 > 5;", Literal::Int(5), ">", Literal::Int(5)),
            ("5 < 5;", Literal::Int(5), "<", Literal::Int(5)),
            ("5 == 5;", Literal::Int(5), "==", Literal::Int(5)),
            ("5 != 5;", Literal::Int(5), "!=", Literal::Int(5)),
            (
                "true == true",
                Literal::Bool(true),
                "==",
                Literal::Bool(true),
            ),
            (
                "true != false",
                Literal::Bool(true),
                "!=",
                Literal::Bool(false),
            ),
            (
                "false == false",
                Literal::Bool(false),
                "==",
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
            "<",
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
            "<",
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

    enum Literal<'a> {
        Ident(&'a str),
        Int(u64),
        Bool(bool),
    }

    fn check_infix_expression(exp: &Expression, left: Literal, operator: &str, right: Literal) {
        let infix_expression = match exp {
            Expression::Infix(infix) => infix,
            generic => panic!(
                "expected infix expression, found: {}",
                generic.type_string()
            ),
        };

        check_literal_expression(&infix_expression.left, left);
        assert_eq!(
            &infix_expression.operator, operator,
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

    fn check_integer_literal(exp: &Expression, expected: u64) {
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
