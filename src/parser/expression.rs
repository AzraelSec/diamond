use std::{fmt::Display, rc::Rc};

use crate::lexer::token::Token;

use super::{node::NodeTrait, statement::BlockStatement};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    StringLiteral(String),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanExpression),
    If(IfExpression),
    FunctionLiteral(FunctionLiteral),
    FunctionCall(FunctionCall),
    ArrayLiteral(ArrayLiteral),
    ArrayIndex(ArrayIndex),
    HashLiteral(HashLiteral),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrefixOperator {
    Not,
    Minus,
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PrefixOperator::Not => "!",
                PrefixOperator::Minus => "-",
            }
        )
    }
}

impl PrefixOperator {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "!" => Some(PrefixOperator::Not),
            "-" => Some(PrefixOperator::Minus),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InfixOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
}

impl Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InfixOperator::Plus => "+",
                InfixOperator::Minus => "-",
                InfixOperator::Multiply => "*",
                InfixOperator::Divide => "/",
                InfixOperator::Equal => "==",
                InfixOperator::NotEqual => "!=",
                InfixOperator::LessThan => "<",
                InfixOperator::LessThanOrEqualTo => "<=",
                InfixOperator::GreaterThan => ">",
                InfixOperator::GreaterThanOrEqualTo => ">=",
            }
        )
    }
}

impl InfixOperator {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "+" => Some(InfixOperator::Plus),
            "-" => Some(InfixOperator::Minus),
            "*" => Some(InfixOperator::Multiply),
            "/" => Some(InfixOperator::Divide),
            "==" => Some(InfixOperator::Equal),
            "!=" => Some(InfixOperator::NotEqual),
            "<" => Some(InfixOperator::LessThan),
            "<=" => Some(InfixOperator::LessThanOrEqualTo),
            ">" => Some(InfixOperator::GreaterThan),
            ">=" => Some(InfixOperator::GreaterThanOrEqualTo),
            _ => None,
        }
    }
}

impl Expression {
    pub fn type_string(&self) -> &str {
        match self {
            Expression::Identifier(_) => "Expression::Identifier",
            Expression::IntegerLiteral(_) => "Expression::IntegerLiteral",
            Expression::StringLiteral(_) => "Expression:StringLiteral",
            Expression::Prefix(_) => "Expression::Prefix",
            Expression::Infix(_) => "Expression::Infix",
            Expression::Boolean(_) => "Expression::Boolean",
            Expression::If(_) => "Expression::If",
            Expression::FunctionLiteral(_) => "Expression::FunctionLiteral",
            Expression::FunctionCall(_) => "Expression::FunctionCall",
            Expression::ArrayLiteral(_) => "Expression::ArrayLiteral",
            Expression::ArrayIndex(_) => "Expression::ArrayIndex",
            Expression::HashLiteral(_) => "Expression::HashLiteral",
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
                Expression::StringLiteral(x) => x.to_string(),
                Expression::Prefix(x) => x.to_string(),
                Expression::Infix(x) => x.to_string(),
                Expression::Boolean(x) => x.to_string(),
                Expression::If(x) => x.to_string(),
                Expression::FunctionLiteral(x) => x.to_string(),
                Expression::FunctionCall(x) => x.to_string(),
                Expression::ArrayLiteral(x) => x.to_string(),
                Expression::ArrayIndex(x) => x.to_string(),
                Expression::HashLiteral(x) => x.to_string(),
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub token: Rc<Token>,
    pub value: i64,
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

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpression {
    pub token: Rc<Token>,
    pub operator: PrefixOperator,
    // NOTE: this was needed to avoid infinite recursion between Expression and PrefixExpression
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

#[derive(Debug, Clone, PartialEq)]
pub struct InfixExpression {
    pub token: Rc<Token>,
    pub operator: InfixOperator,
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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
            "if {} {{{}}} {}",
            self.condition.to_string(),
            self.consequence.to_string(),
            self.alternative
                .as_ref()
                .map_or_else(|| "".to_string(), |e| format!("else {{{}}}", e.to_string()))
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub token: Rc<Token>,
    pub function: Box<Expression>,
    pub args: Vec<Expression>,
}

impl NodeTrait for FunctionCall {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.function.to_string(),
            self.args
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayLiteral {
    pub token: Rc<Token>,
    pub elements: Vec<Expression>,
}

impl NodeTrait for ArrayLiteral {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.elements
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayIndex {
    pub token: Rc<Token>,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl NodeTrait for ArrayIndex {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for ArrayIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left.to_string(), self.index.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HashLiteral {
    pub token: Rc<Token>,
    // NOTE: apparently implementing this as a map is very hard. I'let this be a vec to flat during
    // evaluation. Not very happy about it :(
    pub pairs: Vec<(Expression, Expression)>,
}

impl NodeTrait for HashLiteral {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}

impl Display for HashLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.pairs
                .iter()
                .map(|(k, v)| format!("{}: {}", k.to_string(), v.to_string()))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}
