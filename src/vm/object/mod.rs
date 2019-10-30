mod base;
mod string;
mod ty;

pub use self::base::*;
pub use self::string::*;
pub use self::ty::*;

use crate::vm::{fun::BuiltinFun, storage::Storage, value::*};

use std::{any::Any, fmt::Debug};

pub trait Object: Debug {
    fn get_attr(&self, name: &str) -> Option<StackValue>;
    fn set_attr(&self, name: String, value: StackValue);

    fn as_any(&self) -> &dyn Any;
}

/// A boxed, "live" object value.
pub type ObjectValue = Box<dyn Object>;
