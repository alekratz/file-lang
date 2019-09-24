use crate::{
    syn::{op::OpKind, parser::Parser},
    vm::{
        Vm,
        fun::BuiltinFun,
        value::{CopyValue, Binding},
    },
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
            pub static ref FUNS: HashMap<String, BuiltinFun> = hashmap! {
                $(
                     $builtin_name.to_string() => BuiltinFun::new(
                         Binding(usize::max_value()),
                         $builtin_name.to_string(),
                         Box::new($fn_name),
                     )
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
            pub static ref BIN_OPS: HashMap<Vec<OpKind>, BuiltinFun> = hashmap! {
                $(
                    {
                        let op = Parser::parse_ops($builtin_name)
                            .expect(&format!("invalid builtin op: {:?}", $builtin_name));
                        op.kind
                    } => BuiltinFun::new(
                        Binding(usize::max_value()),
                        format!("<operator {}>", $builtin_name),
                        Box::new($fn_name)
                    )
                ),*
            };
        }
    };
}

macro_rules! un_op_builtins {
    ($($builtin_name:expr => fn $fn_name:ident ($($param_name:ident : $param_t:ty),*) $fn_body:block)*) => {
        $(
            fn $fn_name ( $($param_name : $param_t),* ) $fn_body
        )*
        lazy_static! {
            pub static ref UN_OPS: HashMap<Vec<OpKind>, BuiltinFun> = hashmap! {
                $(
                    {
                        let op = Parser::parse_ops($builtin_name)
                            .expect(&format!("invalid builtin op: {:?}", $builtin_name));
                        op.kind
                    } => BuiltinFun::new(
                        Binding(usize::max_value()),
                        format!("<operator {}>", $builtin_name),
                        Box::new($fn_name)
                    )
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

    "print" => fn builtin_print(_vm: &mut Vm, args: Vec<CopyValue>) {
        if args.len() == 0 {
            return;
        }
        print!("{:?}", args.first().unwrap());
        for arg in args.iter().skip(1) {
            print!(" {:?}", arg);
        }
    }

    "str" => fn builtin_str(_vm: &mut Vm, _args: Vec<CopyValue>) {
    }
}

bin_op_builtins! {
    "+" => fn builtin_bin_plus_op(_vm: &mut Vm, _args: Vec<CopyValue>) {
        unimplemented!();
    }

    "-" => fn builtin_bin_minus_op(_vm: &mut Vm, _args: Vec<CopyValue>) {
        unimplemented!();
    }

    "*" => fn builtin_bin_splat_op(_vm: &mut Vm, _args: Vec<CopyValue>) {
        unimplemented!();
    }

    "/" => fn builtin_bin_fslash_op(_vm: &mut Vm, _args: Vec<CopyValue>) {
        unimplemented!();
    }

    "./" => fn builtin_bin_dot_fslash_op(_vm: &mut Vm, _args: Vec<CopyValue>) {
        unimplemented!();
    }

    "|>" => fn builtin_bin_pipe_gt_op(_vm: &mut Vm, _args: Vec<CopyValue>) {
        unimplemented!();
    }
}

un_op_builtins! {
    "-" => fn builtin_un_minus_op(_vm: &mut Vm, _args: Vec<CopyValue>) {
        unimplemented!()
    }
}