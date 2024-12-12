use std::fmt::Display;

use super::{expression::Expression, program::Program, statement::Statement};

pub enum Node {
    Statement(Statement),
    Expression(Expression),
    Program(Program),
}

pub trait NodeTrait: Display {
    fn token_literal(&self) -> String;
}
