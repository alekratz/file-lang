mod base;
mod fun;
mod string;

pub use self::base::*;
pub use self::string::*;

use crate::{
    common::types::Mapping,
    vm::{fun::BuiltinFun, storage::Storage, value::*},
};

use std::{any::Any, collections::HashMap, fmt::Debug};

pub trait Object: Debug {
    fn get_attr(&self, name: &str) -> Option<StackValue>;
    fn set_attr(&self, name: String, value: StackValue);
    fn attrs(&self) -> Vec<String>;
    fn value_ref(&self) -> ValueRef;
    fn as_any(&self) -> &dyn Any;
}

/// A boxed, "live" object value.
pub type ObjectValue = Box<dyn Object>;
pub type ObjectMembers = Mapping<String, StackValue>;
