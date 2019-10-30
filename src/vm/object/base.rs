use crate::{
    common::prelude::*,
    vm::{fun::BuiltinFun, inst::Inst, storage::Storage, value::*, object::*},
};
use lazy_static::lazy_static;
use shrinkwraprs::Shrinkwrap;
use std::{
    any::Any,
    cell::RefCell,
    fmt::{self, Debug, Display, Formatter},
};

/// The base object type.
///
/// This probably should not be used by the language directly, and instead should be used as a way
/// of backing object storage.
#[derive(Debug, Default)]
pub struct BaseObject {
    members: RefCell<Mapping<String, StackValue>>,
}

impl BaseObject {
    pub fn new(members: Mapping<String, StackValue>) -> Self {
        BaseObject {
            members: members.into(),
        }
    }
}

impl Object for BaseObject {
    fn get_attr(&self, name: &str) -> Option<StackValue> {
        let members = self.members.borrow();
        members.get(name).copied()
    }

    fn set_attr(&self, name: String, value: StackValue) {
        let mut members = self.members.borrow_mut();
        members.insert(name, value);
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum ProtoValue {
    String(String),
    Int(i64),
    Float(String), // XXX floats aren't Hash and I'm lazy, so we'll parse them
    Function(BuiltinFun),
}

impl ProtoValue {
    fn into_stack_value(self, storage: &mut Storage) -> StackValue {
        unimplemented!()
    }
}

#[derive(Default)]
struct BaseObjectBuilder {
    name: Option<String>,
    members: Mapping<String, ProtoValue>,
}

impl BaseObjectBuilder {
    fn builtin_fun(
        mut self,
        name: &str,
        value: impl 'static + Fn(&mut Storage, Vec<StackValue>) + Sync,
    ) -> Self {
        self.member(
            name.to_string(),
            ProtoValue::Function(BuiltinFun::new(name.into(), Box::new(value))),
        )
    }

    fn member(mut self, name: String, value: ProtoValue) -> Self {
        self.members.insert(name, value);
        self
    }

    fn name(mut self, name: String) -> Self {
        self.name = Some(name);
        self
    }

    fn finish(self, storage: &mut Storage) -> BaseObject {
        unimplemented!()
    }
}


lazy_static! {
    static ref BASE_OBJECT_BUILDER: BaseObjectBuilder = {
        /*
         * Things that the base object should have:
         *  * __setattr__(self, name, value: Any) => sets an attribute
         *  * __getattr__(self, name) -> Any => gets an attribute
         */
        BaseObjectBuilder::default()
            .builtin_fun("__setattr__", |storage, mut args| {
                if args.len() != 3 {
                    // TODO(exception)
                    // Turn all of these panics and expects into exceptions
                    panic!("__setattr__: incorrect function arity; expected 3 but got {} instead",
                           args.len());
                }
                let value = args.pop().unwrap();
                let name_value = args.pop().unwrap();
                let object_value = args.pop().unwrap();

                let name = if let Some(name) = storage.downcast_stack_value::<StringObject>(name_value) {
                    name.to_string()
                } else {
                    panic!("__setattr__: name must be a string");
                };

                let object = storage.deref_stack_value(object_value)
                    .expect("tried to call __setattr__ on a non-object value");

                object.set_attr(name, value);
            })
            .builtin_fun("__getattr__", |_, args| {
                if args.len() != 2 {
                    // TODO(exception)
                    // Turn all of these panics and expects into exceptions
                    panic!("__getattr__: incorrect function arity; expected 2 but got {} instead",
                           args.len());
                }
                unimplemented!("__getattr__")
            })
    };
}
