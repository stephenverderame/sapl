use super::environment::Scope;
use super::environment::Environment;
use super::Res;
use super::Values;
use super::exn::*;
use rand::Rng;
use std::rc::Rc;

use super::Res::Vl;

fn add_func(scope: &mut Scope, name: &str, func: fn(Vec<Values>) -> Res, min_args: usize) {
    scope.add(name.to_owned(), Values::RustFunc(Rc::new(func), min_args), false);
}

fn add_name(scope: &mut Scope, name: &str, val: Values) {
    scope.add(name.to_owned(), val, false);
}

pub fn get_std_environment() -> Scope {
    let mut scope = Scope::new();
    add_func(&mut scope, "typeof", eval_type, 1);
    add_func(&mut scope, "assert", eval_assert, 1);
    add_func(&mut scope, "len", eval_len, 1);
    add_func(&mut scope, "random", eval_rand, 1);
    add_name(&mut scope, "None", Values::Unit);
    scope
}

/// Evaluates typeof()
fn eval_type(args: Vec<Values>) -> Res {
    if args.len() == 1 {
        Vl(Values::Str(type_of(&args[0])))
    } else {
        inv_arg("typeof", Some("expect 1 argument"))
    }
}

/// Gets the length of `arg[0]`
/// @see size_of
/// References will return the length of whatever they refer to
fn eval_len(args: Vec<Values>) -> Res {
    if args.len() == 1 {
        Vl(Values::Int(size_of(&args[0])))
    } else {
        inv_arg("sizeof", None)
    }
}

fn eval_rand(mut args: Vec<Values>) -> Res {
    let mut rng = rand::thread_rng();
    if args.len() == 2 {
        match (args.swap_remove(0), args.pop().unwrap()) {
            (Values::Int(min), Values::Int(max)) =>
                 Vl(Values::Int(rng.gen_range(min..max))),
            (Values::Float(min), Values::Float(max)) =>
                Vl(Values::Float(rng.gen_range(min..max))),
            _ => inv_arg("random", None),
        }
    } else if args.len() == 1 {
        match args.pop().unwrap() {
            Values::Range(min, max) => {
                match (*min, *max) {
                    (Values::Int(start), Values::Int(end)) => Vl(Values::Int(rng.gen_range(start..end))),
                    (Values::Float(start), Values::Float(end)) => Vl(Values::Float(rng.gen_range(start..end))),
                    _ => inv_arg("random", None),
                }
            },
            Values::Int(x) => Vl(Values::Int(rng.gen_range(0..x))),
            Values::Float(x) => Vl(Values::Float(rng.gen_range(0.0..x))),
            _ => inv_arg("random", None),
        }
    } else {
        inv_arg("random", None)
    }
}

/// Gets the size of `val`
/// String, maps, tuples, and arrays is their length
/// range is 2, unless it is a valid range then its the different between start and end
/// Unit is 0
/// References is the size of its data
/// the rest are 1
fn size_of(val: &Values) -> i32 {
    match val {
        Values::Str(s) => s.len() as i32,
        Values::Range(start, end) => {
            if let (Values::Int(s), Values::Int(e)) = (&**start, &**end) {
                (*e - *s).abs()
            } else {
                2
            }
        },
        Values::Array(x) => x.len() as i32,
        Values::Map(x) => x.len() as i32,
        Values::Tuple(x) => x.len() as i32,
        Values::Unit => 0,
        Values::Ref(ptr, _) => size_of(&ptr.borrow()),
        _ => 1,
    }
}

/// Evaluates assert()
fn eval_assert(mut args: Vec<Values>) -> Res {
    if args.len() == 2 {
        if let Values::Str(msg) = args.pop().unwrap() {
            match args.pop().unwrap() {
                Values::Bool(true) => Vl(Values::Unit),
                Values::Bool(false) => Res::Bad(format!("Assertation error: '{}'", msg)),
                _ => inv_arg("assert", Some("expects a boolean")),
            }
        } else {
            inv_arg("assert", Some("expects an error message"))
        }
    } else if args.len() == 1 {
        match args.pop().unwrap() {
            Values::Bool(true) => Vl(Values::Unit),
            Values::Bool(false) => Res::Bad(format!("Assertation error")),
            _ => inv_arg("assert", Some("expects a boolean")),
        }
    } else {
        inv_arg("assert", Some("invalid arg count"))
    }
}

/// Gets the string representation of the type of `v`
pub fn type_of(v: &Values) -> String {
    match v {
        Values::Int(_) => "int".to_owned(),
        Values::Bool(_) => "bool".to_owned(),
        Values::Unit => "unit".to_owned(),
        Values::Float(_) => "float".to_owned(),
        Values::Str(_) => "string".to_owned(),
        Values::Range(..) => "range".to_owned(),
        Values::Tuple(x) => format!("tuple_{}", x.len()),
        Values::Array(_) => "array".to_owned(),
        Values::Map(_) => "map".to_owned(),
        Values::Func(..) => "function".to_owned(),
        Values::Placeholder => "partial app placeholder".to_owned(),
        Values::Ref(..) => "ref".to_owned(),
        Values::RustFunc(..) => "function".to_owned(),
    }
}