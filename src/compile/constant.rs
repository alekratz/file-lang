use crate::{
    compile::builtins::*,
    vm::{fun::*, value::ValueRef},
};

#[derive(Debug, Clone)]
pub enum ConstValue {
    String(String),
    BuiltinFun(BuiltinFunPtr),
    BuiltinType(BuiltinType),
    UserFun(UserFun),
    Placeholder,
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

impl From<BuiltinFunPtr> for ConstValue {
    fn from(other: BuiltinFunPtr) -> Self {
        ConstValue::BuiltinFun(other)
    }
}
