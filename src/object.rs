use std::fmt::Display;

use crate::ast::expression::{InfixOperator, PrefixOperator};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ObjectType {
    Null,
    Integer,
    Boolean,
    Return,
    Error,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectType::Null => write!(f, "null"),
            ObjectType::Integer => write!(f, "integer"),
            ObjectType::Boolean => write!(f, "boolean"),
            ObjectType::Return => write!(f, "return"),
            ObjectType::Error => write!(f, "error"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(ErrorObject),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ErrorObject {
    Generic(String),
    DivisionByZero,
    UnknownPrefixOperator(PrefixOperator, ObjectType),
    UnknownInfixOperator(ObjectType, InfixOperator, ObjectType),
}

impl Display for ErrorObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorObject::Generic(msg) => write!(f, "{}", msg),
            ErrorObject::DivisionByZero => {
                write!(f, "attempted to perform a division by zero operation")
            }
            ErrorObject::UnknownPrefixOperator(op, obj) => {
                write!(f, "unknown operator: {}{}", op, obj)
            }
            ErrorObject::UnknownInfixOperator(left, op, right) => {
                write!(f, "unknown operator: {} {} {}", left, op, right)
            }
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Null => write!(f, "null"),
            Self::Integer(obj) => write!(f, "{}", obj),
            Self::Boolean(obj) => write!(f, "{}", obj),
            Self::Return(obj) => write!(f, "{}", obj),
            Self::Error(msg) => write!(f, "{}", msg),
        }
    }
}

impl Object {
    pub fn get_type(&self) -> ObjectType {
        match self {
            Object::Null => ObjectType::Null,
            Object::Integer(_) => ObjectType::Integer,
            Object::Boolean(_) => ObjectType::Boolean,
            Object::Return(_) => ObjectType::Return,
            Object::Error(_) => ObjectType::Error,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let tests = vec![
            (Object::Integer(1), "1"),
            (
                Object::Error(ErrorObject::Generic("something went wrong".to_string())),
                "something went wrong",
            ),
            (
                Object::Error(ErrorObject::UnknownPrefixOperator(
                    PrefixOperator::Not,
                    ObjectType::Boolean,
                )),
                "unknown operator: !boolean",
            ),
            (
                Object::Error(ErrorObject::UnknownInfixOperator(
                    ObjectType::Boolean,
                    InfixOperator::Plus,
                    ObjectType::Boolean,
                )),
                "unknown operator: boolean + boolean",
            ),
        ];

        for (obj, expected) in tests {
            assert_eq!(obj.to_string(), expected);
        }
    }
}
