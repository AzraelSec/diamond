use std::{fmt::Display, rc::Rc};

use crate::token::Token;

use super::{node::NodeTrait, statement::BlockStatement};

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanExpression),
    If(IfExpression),
    FunctionLiteral(FunctionLiteral),
}

impl Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(x) => x.token_literal(),
            Expression::IntegerLiteral(x) => x.token_literal(),
            Expression::Prefix(x) => x.token_literal(),
            Expression::Infix(x) => x.token_literal(),
            Expression::Boolean(x) => x.token_literal(),
            Expression::If(x) => x.token_literal(),
            Expression::FunctionLiteral(x) => x.token_literal(),
        }
    }

    pub fn type_string(&self) -> &str {
        match self {
            Expression::Identifier(_) => "Expression::Identifier",
            Expression::IntegerLiteral(_) => "Expression::IntegerLiteral",
            Expression::Prefix(_) => "Expression::Prefix",
            Expression::Infix(_) => "Expression::Infix",
            Expression::Boolean(_) => "Expression::Boolean",
            Expression::If(_) => "Expression::If",
            Expression::FunctionLiteral(_) => "Expression::FunctionLiteral",
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expression::Identifier(x) => x.to_string(),
                Expression::IntegerLiteral(x) => x.to_string(),
                Expression::Prefix(x) => x.to_string(),
                Expression::Infix(x) => x.to_string(),
                Expression::Boolean(x) => x.to_string(),
                Expression::If(x) => x.to_string(),
                Expression::FunctionLiteral(x) => x.to_string(),
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Rc<Token>,
    pub value: String,
}

impl NodeTrait for Identifier {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Rc<Token>,
    pub value: u64,
}

impl NodeTrait for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Rc<Token>,
    pub operator: String,
    // note: this was needed to avoid infinite recursion between Expression and PrefixExpression
    // :'(
    pub right: Box<Expression>,
}

impl NodeTrait for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Rc<Token>,
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl NodeTrait for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.left.to_string(),
            self.operator,
            self.right.to_string()
        )
    }
}

#[derive(Debug, Clone)]
pub struct BooleanExpression {
    pub token: Rc<Token>,
    pub value: bool,
}

impl NodeTrait for BooleanExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for BooleanExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Rc<Token>,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl NodeTrait for IfExpression {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "if {}{}",
            self.token.to_string(),
            self.alternative
                .as_ref()
                .map_or_else(|| "".to_string(), |e| format!("else {}", e.to_string()))
        )
    }
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub token: Rc<Token>,
    pub params: Vec<Identifier>,
    pub body: BlockStatement,
}

impl NodeTrait for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({}) {}",
            self.token_literal(),
            self.params
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(","),
            self.body.to_string()
        )
    }
}
