use std::fmt::Display;

pub enum ObjectType {
    Null,
    Integer,
    Boolean,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectType::Null => write!(f, "ObjectType::Null"),
            ObjectType::Integer => write!(f, "ObjectType::Integer"),
            ObjectType::Boolean => write!(f, "ObjectType::Boolean"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Null => write!(f, "null"),
            Self::Integer(obj) => write!(f, "{}", obj),
            Self::Boolean(obj) => write!(f, "{}", obj),
        }
    }
}

impl Object {
    pub fn get_type(&self) -> ObjectType {
        match self {
            Object::Null => ObjectType::Null,
            Object::Integer(_) => ObjectType::Integer,
            Object::Boolean(_) => ObjectType::Boolean,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let tests = vec![(Object::Integer(1), "1")];

        for (obj, expected) in tests {
            assert_eq!(obj.to_string(), expected);
        }
    }
}
