mod base;
mod fun;
mod string;
mod ty;

pub use self::base::*;
pub use self::fun::*;
pub use self::string::*;
pub use self::ty::*;

use crate::{
    common::types::Mapping,
    vm::{value::*},
};

use std::{any::Any, fmt::Debug};

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
