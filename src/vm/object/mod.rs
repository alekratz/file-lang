mod ty;
mod base;
mod string;

pub use self::ty::*;
pub use self::base::*;
pub use self::string::*;

use crate::{
    common::prelude::*,
    vm::{fun::BuiltinFun, inst::Inst, storage::Storage, value::*},
};
use lazy_static::lazy_static;
use shrinkwraprs::Shrinkwrap;
use std::{
    any::Any,
    cell::RefCell,
    fmt::{self, Debug, Display, Formatter},
};

pub trait Object: Debug {
    fn get_attr(&self, name: &str) -> Option<StackValue>;
    fn set_attr(&self, name: String, value: StackValue);

    fn as_any(&self) -> &dyn Any;
}

/// A boxed, "live" object value.
pub type ObjectValue = Box<dyn Object>;
