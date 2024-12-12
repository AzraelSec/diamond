use super::object::{ErrorObject, Object};

pub type BuiltInFun = fn(Vec<Object>) -> Object;
pub fn get_builtin(ident: &str) -> Option<Object> {
    match ident {
        "len" => Some(Object::BuiltIn(|args| -> Object {
            if args.len() != 1 {
                return Object::Error(ErrorObject::WrongNumberOfParams(1, args.len()));
            };

            match &args[0] {
                Object::String(val) => Object::Integer(val.len() as i64),
                Object::Array(array) => Object::Integer(array.len() as i64),
                generic => Object::Error(ErrorObject::Generic(format!(
                    "len builtin does not support type {}",
                    generic.get_type()
                ))),
            }
        })),
        "first" => Some(Object::BuiltIn(|args| -> Object {
            if args.len() != 1 {
                return Object::Error(ErrorObject::WrongNumberOfParams(1, args.len()));
            };

            match &args[0] {
                Object::Array(array) => {
                    if array.is_empty() {
                        return Object::Null;
                    } else {
                        return array.get(0).unwrap_or(&Object::Null).clone();
                    }
                }
                generic => Object::Error(ErrorObject::Generic(format!(
                    "first builtin does not support type {}",
                    generic.get_type()
                ))),
            }
        })),
        "last" => Some(Object::BuiltIn(|args| -> Object {
            if args.len() != 1 {
                return Object::Error(ErrorObject::WrongNumberOfParams(1, args.len()));
            };

            match &args[0] {
                Object::Array(array) => {
                    if array.is_empty() {
                        return Object::Null;
                    } else {
                        return array.get(array.len() - 1).unwrap_or(&Object::Null).clone();
                    }
                }
                generic => Object::Error(ErrorObject::Generic(format!(
                    "last builtin does not support type {}",
                    generic.get_type()
                ))),
            }
        })),
        "rest" => Some(Object::BuiltIn(|args| -> Object {
            if args.len() != 1 {
                return Object::Error(ErrorObject::WrongNumberOfParams(1, args.len()));
            };

            match &args[0] {
                Object::Array(array) => Object::Array(array.iter().skip(1).cloned().collect()),
                generic => Object::Error(ErrorObject::Generic(format!(
                    "rest builtin does not support type {}",
                    generic.get_type(),
                ))),
            }
        })),
        "push" => Some(Object::BuiltIn(|args| -> Object {
            if args.len() != 2 {
                return Object::Error(ErrorObject::WrongNumberOfParams(2, args.len()));
            };

            match (&args[0], &args[1]) {
                (Object::Array(array), value) => {
                    let mut src = array.clone();
                    src.push(value.clone());
                    Object::Array(src)
                }
                (generic, _) => Object::Error(ErrorObject::Generic(format!(
                    "push builtin does not support type {}",
                    generic.get_type()
                ))),
            }
        })),
        _ => None,
    }
}
