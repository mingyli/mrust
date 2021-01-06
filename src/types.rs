use std::convert::From;

// Handle built-in types and user-defined types.
#[derive(Debug)]
pub enum Type {
    Int,
    Bool,
    Unit,
    UserDefined(String),
}

impl From<String> for Type {
    fn from(s: String) -> Type {
        match s.as_str() {
            "i64" => Type::Int,
            "bool" => Type::Bool,
            "()" => Type::Unit,
            _ => Type::UserDefined(s),
        }
    }
}
