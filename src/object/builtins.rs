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
                generic => Object::Error(ErrorObject::Generic(format!(
                    "len builtin does not support type {}",
                    generic.get_type()
                ))),
            }
        })),
        _ => None,
    }
}
