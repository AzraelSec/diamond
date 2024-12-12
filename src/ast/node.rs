use std::fmt::Display;

use super::{expression::Expression, program::Program, statement::Statement};

pub(crate) enum Node {
    Statement(Statement),
    Expression(Expression),
    Program(Program),
}

pub(crate) trait NodeTrait: Display {
    fn token_literal(&self) -> String;
}
