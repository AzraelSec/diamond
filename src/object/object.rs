use std::hash::Hash;
use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{
        expression::{Identifier, InfixOperator, PrefixOperator},
        statement::BlockStatement,
    },
    environment::Environment,
};

type BuiltInFun = fn(Vec<Object>) -> Object;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ObjectType {
    Null,
    Integer,
    String,
    Boolean,
    Return,
    Error,
    Function,
    BuiltIn,
    Array,
    HashMap,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectType::Null => write!(f, "null"),
            ObjectType::Integer => write!(f, "integer"),
            ObjectType::String => write!(f, "string"),
            ObjectType::Boolean => write!(f, "boolean"),
            ObjectType::Return => write!(f, "return"),
            ObjectType::Function => write!(f, "function"),
            ObjectType::BuiltIn => write!(f, "built-in"),
            ObjectType::Error => write!(f, "error"),
            ObjectType::Array => write!(f, "array"),
            ObjectType::HashMap => write!(f, "hashmap"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Null,
    Integer(i64),
    String(String),
    Boolean(bool),
    Return(Box<Object>),
    Function(FunctionObject),
    BuiltIn(BuiltInFun),
    Error(ErrorObject),
    Array(Vec<Object>),
    HashMap(HashMap<Object, Object>),
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::String(val) => val.hash(state),
            Self::Integer(val) => val.hash(state),
            Self::Boolean(val) => val.hash(state),
            generic => panic!(
                "unexpected object used as hashmap key: {}",
                generic.get_type()
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorObject {
    Generic(String),
    DivisionByZero,
    // note: this could be an identifier (?)
    CallOnNonFunction(ObjectType),
    WrongNumberOfParams(usize, usize),
    InvalidHashKey(ObjectType),
    IdentifierNotFound(String),
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
            ErrorObject::CallOnNonFunction(obj) => {
                write!(
                    f,
                    "impossible to perform a function call on a non-function: {}",
                    obj
                )
            }
            ErrorObject::WrongNumberOfParams(expected, got) => {
                write!(
                    f,
                    "wrong number of params: expected={}, got={}",
                    expected, got
                )
            }
            ErrorObject::InvalidHashKey(tpe) => write!(
                f,
                "hash key must be a string, integer or boolean, found {}",
                tpe
            ),
            ErrorObject::IdentifierNotFound(identifier) => {
                write!(f, "identifier not found: {}", identifier)
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

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionObject {
    pub params: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Display for FunctionObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn ({}) {{\n{}\n}}",
            self.params
                .iter()
                .map(|p| p.value.clone())
                .collect::<Vec<String>>()
                .join(", "),
            self.body,
        )
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Null => write!(f, "null"),
            Self::Integer(obj) => write!(f, "{}", obj),
            Self::String(obj) => write!(f, "{}", obj),
            Self::Boolean(obj) => write!(f, "{}", obj),
            Self::Return(obj) => write!(f, "{}", obj),
            Self::Function(obj) => write!(f, "{}", obj),
            Self::BuiltIn(_) => write!(f, "<runtime function>"),
            Self::Error(msg) => write!(f, "{}", msg),
            Self::Array(objs) => write!(
                f,
                "[{}]",
                objs.iter()
                    .map(|o| o.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::HashMap(hashmap) => write!(
                f,
                "{{{}}}",
                hashmap
                    .iter()
                    .map(|(key, val)| format!("{}: {}", key.to_string(), val.to_string()))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Object {
    pub fn get_type(&self) -> ObjectType {
        match self {
            Object::Null => ObjectType::Null,
            Object::Integer(_) => ObjectType::Integer,
            Object::String(_) => ObjectType::String,
            Object::Boolean(_) => ObjectType::Boolean,
            Object::Return(_) => ObjectType::Return,
            Object::Function(_) => ObjectType::Function,
            Object::BuiltIn(_) => ObjectType::BuiltIn,
            Object::Error(_) => ObjectType::Error,
            Object::Array(_) => ObjectType::Array,
            Object::HashMap(_) => ObjectType::HashMap,
        }
    }

    pub fn can_be_hash_key(&self) -> bool {
        matches!(
            self,
            Object::String(_) | Object::Integer(_) | Object::Boolean(_)
        )
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
                Object::Error(ErrorObject::WrongNumberOfParams(1, 2)),
                "wrong number of params: expected=1, got=2",
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
            (
                Object::Error(ErrorObject::IdentifierNotFound("foobar".to_string())),
                "identifier not found: foobar",
            ),
            (
                Object::Error(ErrorObject::InvalidHashKey(ObjectType::Function)),
                "hash key must be a string, integer or boolean, found function",
            ),
        ];

        for (obj, expected) in tests {
            assert_eq!(obj.to_string(), expected);
        }
    }
}
