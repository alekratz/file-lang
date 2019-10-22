use crate::{
    syn::{op::OpKind, parser::Parser},
    vm::{fun::BuiltinFunPtr, value::{CopyValue, Value}, Vm},
};
use lazy_static::lazy_static;
use maplit::hashmap;
use std::collections::HashMap;

macro_rules! builtins {
    ($($builtin_name:expr => fn $fn_name:ident ($($param_name:ident : $param_t:ty),*) $fn_body:block)*) => {
        $(
            fn $fn_name ( $($param_name : $param_t),* ) $fn_body
        )*
        lazy_static! {
            pub static ref FUNS: HashMap<&'static str, BuiltinFunPtr> = hashmap! {
                $(
                     $builtin_name => $fn_name as BuiltinFunPtr
                ),*
            };
        }
    };
}

macro_rules! bin_op_builtins {
    ($($builtin_name:expr => fn $fn_name:ident ($($param_name:ident : $param_t:ty),*) $fn_body:block)*) => {
        $(
            fn $fn_name ( $($param_name : $param_t),* ) $fn_body
        )*
        lazy_static! {
            pub static ref BIN_OPS: HashMap<Vec<OpKind>, BuiltinFunPtr> = hashmap! {
                $(
                    {
                        let op = Parser::parse_ops($builtin_name)
                            .expect(&format!("invalid builtin op: {:?}", $builtin_name));
                        op.kind
                    } => $fn_name as BuiltinFunPtr
                ),*
            };
        }
    };
}

macro_rules! bin_op_builtin {
    ($name:ident, $expr:expr) => {{
        fn $name (vm: &mut Vm, args: Vec<CopyValue>) {
            let mut args = args.into_iter();
            let lhs = args.next().unwrap();
            let rhs = args.next().unwrap();
            $expr
        }
    }};
}

macro_rules! un_op_builtins {
    ($($builtin_name:expr => fn $fn_name:ident ($($param_name:ident : $param_t:ty),*) $fn_body:block)*) => {
        $(
            fn $fn_name ( $($param_name : $param_t),* ) $fn_body
        )*
        lazy_static! {
            pub static ref UN_OPS: HashMap<Vec<OpKind>, BuiltinFunPtr> = hashmap! {
                $(
                    {
                        let op = Parser::parse_ops($builtin_name)
                            .expect(&format!("invalid builtin op: {:?}", $builtin_name));
                        op.kind
                    } => $fn_name as BuiltinFunPtr
                ),*
            };
        }
    };
}

builtins! {
    "println" => fn builtin_println(vm: &mut Vm, args: Vec<CopyValue>) {
        builtin_print(vm, args);
        println!();
    }

    "print" => fn builtin_print(vm: &mut Vm, args: Vec<CopyValue>) {
        if args.len() == 0 {
            return;
        }
        print!("{}", args.first().unwrap().value_display(vm.storage()));
        for arg in args.iter().skip(1) {
            print!(" {}", arg.value_display(vm.storage()));
        }
    }

    "str" => fn builtin_str(vm: &mut Vm, args: Vec<CopyValue>) {
        let mut args = args;
        let arg = args.pop()
            .expect("no args");
        assert!(args.is_empty());

        let value_string = arg.value_display(vm.storage()).to_string();
        let heap_ref = vm.storage_mut().allocate_heap(Value::String(value_string));
        vm.set_return_value(Some(CopyValue::HeapRef(heap_ref)));
    }
}

bin_op_builtins! {
    "+" => fn builtin_bin_plus_op(vm: &mut Vm, args: Vec<CopyValue>) {
        let mut args = args.into_iter();
        let lhs = args.next().unwrap();
        let rhs = args.next().unwrap();
        assert_eq!(args.next(), None);

        let result = match (lhs, rhs) {
            (CopyValue::Int(i), CopyValue::Int(j)) => CopyValue::Int(i + j),
            (CopyValue::Real(f), CopyValue::Int(i)) | (CopyValue::Int(i), CopyValue::Real(f)) => CopyValue::Real(i as f64 + f),
            (CopyValue::Real(f), CopyValue::Real(r)) => CopyValue::Real(f + r),
            _ => { unimplemented!() }
        };
        vm.set_return_value(Some(result));
    }

    "-" => fn builtin_bin_minus_op(vm: &mut Vm, args: Vec<CopyValue>) {
        let mut args = args.into_iter();
        let lhs = args.next().unwrap();
        let rhs = args.next().unwrap();
        assert_eq!(args.next(), None);

        let result = match (lhs, rhs) {
            (CopyValue::Int(i), CopyValue::Int(j)) => CopyValue::Int(i - j),
            (CopyValue::Real(f), CopyValue::Int(i)) => CopyValue::Real(f - (i as f64)),
            (CopyValue::Int(i), CopyValue::Real(f)) => CopyValue::Real((i as f64) - f),
            (CopyValue::Real(f), CopyValue::Real(r)) => CopyValue::Real(f - r),
            _ => { unimplemented!() }
        };
        vm.set_return_value(Some(result));
    }

    "*" => fn builtin_bin_splat_op(_vm: &mut Vm, args: Vec<CopyValue>) {
        let mut args = args.into_iter();
        let _lhs = args.next().unwrap();
        let _rhs = args.next().unwrap();
        assert_eq!(args.next(), None);
        unimplemented!();
    }

    "/" => fn builtin_bin_fslash_op(_vm: &mut Vm, args: Vec<CopyValue>) {
        let mut args = args.into_iter();
        let _lhs = args.next().unwrap();
        let _rhs = args.next().unwrap();
        assert_eq!(args.next(), None);
        unimplemented!();
    }

    "|>" => fn builtin_bin_pipe_gt_op(_vm: &mut Vm, args: Vec<CopyValue>) {
        let mut args = args.into_iter();
        let _lhs = args.next().unwrap();
        let _rhs = args.next().unwrap();
        assert_eq!(args.next(), None);
        unimplemented!();
    }
}

un_op_builtins! {
    "-" => fn builtin_un_minus_op(_vm: &mut Vm, _args: Vec<CopyValue>) {
        unimplemented!()
    }
}
