use std::{fmt::Display, rc::Rc};

use crate::lexer::token::Token;

use super::{
    expression::{Expression, Identifier},
    node::NodeTrait,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
    While(WhileStatement),
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::Let(s) => s.token_literal(),
            Statement::Return(s) => s.token_literal(),
            Statement::Expression(s) => s.token_literal(),
            Statement::Block(s) => s.token_literal(),
            Statement::While(s) => s.token_literal(),
        }
    }

    pub fn type_string(&self) -> &str {
        match self {
            Statement::Let(_) => "Statement::Let",
            Statement::Return(_) => "Statement::Return",
            Statement::Expression(_) => "Statement::Expression",
            Statement::Block(_) => "Statement::Block",
            Statement::While(_) => "Statement::While",
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Statement::Let(s) => s.to_string(),
                Statement::Return(s) => s.to_string(),
                Statement::Expression(s) => s.to_string(),
                Statement::Block(s) => s.to_string(),
                Statement::While(s) => s.to_string(),
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub token: Rc<Token>,
    pub name: Identifier,
    pub value: Expression,
}

impl NodeTrait for LetStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token_literal(),
            self.name.to_string(),
            self.value.to_string()
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub token: Rc<Token>,
    pub value: Expression,
}

impl NodeTrait for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.token.to_string(), self.value.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub token: Rc<Token>,
    pub expression: Expression,
}

impl NodeTrait for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub token: Rc<Token>,
    pub statements: Vec<Statement>,
}

impl NodeTrait for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|stmt| stmt.to_string())
                .collect::<String>()
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub token: Rc<Token>,
    pub guard: Expression,
    pub body: BlockStatement,
}

impl NodeTrait for WhileStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for WhileStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "while ({}) {{{}}}",
            self.guard.to_string(),
            self.body.to_string()
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::program::Program;

    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Rc::new(Token::Let),
                name: Identifier {
                    token: Rc::new(Token::Ident("myVar".to_string())),
                    value: "myVar".to_string(),
                },
                value: Expression::Identifier(Identifier {
                    token: Rc::new(Token::Ident("anotherVar".to_string())),
                    value: "anotherVar".to_string(),
                }),
            })],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }
}
