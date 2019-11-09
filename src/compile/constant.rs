use crate::{
    compile::builtins::*,
    vm::fun::*,
};

#[derive(Debug)]
pub enum ConstValue {
    String(String),
    BuiltinFun(BuiltinFun),
    BuiltinType(BuiltinType),
    UserFun(UserFun),
}

impl From<String> for ConstValue {
    fn from(other: String) -> Self {
        ConstValue::String(other)
    }
}

impl From<&'_ str> for ConstValue {
    fn from(other: &'_ str) -> Self {
        ConstValue::String(other.to_string())
    }
}
