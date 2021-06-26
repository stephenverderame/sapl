use super::environment::Scope;
use super::environment::Environment;
use super::Res;
use super::Values;
use super::exn::*;
use rand::Rng;
use std::rc::Rc;
use std::collections::HashMap;
use super::eval_class::Class;

use super::Res::Vl;

pub fn get_std_environment() -> Scope {
    let mut scope = Scope::new();
    add_func(&mut scope, "typeof", eval_type, 1);
    add_func(&mut scope, "assert", eval_assert, 1);
    add_func(&mut scope, "len", eval_len, 1);
    add_func(&mut scope, "random", eval_rand, 1);
    add_name(&mut scope, "None", Values::Unit);
    add_func(&mut scope, "array::push_back", arr_push_back, 2);
    add_func(&mut scope, "array::contains", arr_contains_all, 2);
    add_func(&mut scope, "map::contains", map_contains_all, 2);
    add_func(&mut scope, "array::set", arr_set, 3);
    add_func(&mut scope, "array::insert", arr_insert, 3);
    add_func(&mut scope, "array::remove", arr_remove, 2);
    add_func(&mut scope, "map::insert", map_insert, 2);
    add_func(&mut scope, "map::remove", map_remove, 2);
    scope
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
        Values::Ref(_, _) => "ref".to_owned(),//type_of(&ptr.borrow()),
        Values::RustFunc(..) => "function".to_owned(),
        Values::Object(ptr) => {
            let Class {name, ..} = &**ptr;
            name.clone()
        },
        Values::Type(ptr) => {
            let Class {name, ..} = &**ptr;
            name.clone()
        },
    }
}

fn add_func(scope: &mut Scope, name: &str, func: fn(Vec<Values>) -> Res, min_args: usize) {
    scope.add(name.to_owned(), Values::RustFunc(Rc::new(func), min_args), false);
}

fn add_name(scope: &mut Scope, name: &str, val: Values) {
    scope.add(name.to_owned(), val, false);
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
        Values::Object(ptr) => {
            let Class {name: _, members, ..} = &**ptr;
            members.len() as i32
        },
        Values::Type(ptr) => {
            let Class {name: _, members, ..} = &**ptr;
            members.len() as i32
        },
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

fn arr_push_back(args: Vec<Values>) -> Res {
    let closure = |val : &mut Values, args: Vec<Values>| -> Res {
        if let Values::Array(val) = val {
            for a in args.into_iter() {
                val.push(a);
            }
            Vl(Values::Unit)
        } else { inv_arg("array::push_back", None) }
        
    };
    eval_dot_func(args, 1, "array::push_back", true, Box::new(closure))
}

/// Evaluates the arguments of the dot application
/// Ensures that `min_args` are passed (excluding the dot argument)
/// Ensures that only mutable values are passed when `need_mut` is true
/// Calls `closure` passing the dot argument and remaining arguments if all is well
fn eval_dot_func(mut args: Vec<Values>, min_args: usize, name: &str, 
    need_mut: bool, closure: Box<dyn FnOnce(&mut Values, Vec<Values>) -> Res>) -> Res
{
    if args.len() < min_args + 1 { return inv_arg(name, Some("Not enough arguments")); }
    let context = args.remove(0);
    match context {
        Values::Ref(ptr, _) if !need_mut => closure(&mut *ptr.borrow_mut(), args),
        Values::Ref(ptr, true) if need_mut => closure(&mut *ptr.borrow_mut(), args),
        Values::Ref(..) => str_exn(IMMU_ERR),
        mut v => closure(&mut v, args),
    }
}

/// True if `arr` contains all of the vales from the evaluated asts in `args`
fn arr_contains_all(args: Vec<Values>) -> Res
{
    eval_dot_func(args, 1, "array::contains", false, 
    Box::new(|first: &mut Values, args: Vec<Values>| -> Res {
        if let Values::Array(x) = first {
            for a in args.into_iter() {
                if x.iter().find(|bx| {**bx == a}) == None { 
                    return Vl(Values::Bool(false));
                }
            }
            Vl(Values::Bool(true))
        } else { inv_arg("array::contains", None) }
    }))
}

fn arr_insert(args: Vec<Values>) -> Res {
    eval_dot_func(args, 2, "array::insert", true, 
    Box::new(|val : &mut Values, mut args: Vec<Values>| -> Res {
        if let Values::Array(val) = val {
            match (args.swap_remove(0), args.pop().unwrap()) {
                (Values::Int(idx), v) if idx >= 0 && idx < val.len() as i32 =>
                    val.insert(idx as usize, v),
                (Values::Int(idx), v) if idx >= val.len() as i32 =>
                    val.push(v),
                (Values::Int(_), _) => return str_exn(IDX_BNDS),
                (x, _) => return inv_arg("array::insert", Some(
                    &format!("Index must be an int. Got '{:?}'", x)[..]
                )),
            };
            Vl(Values::Unit)
        } else { inv_arg("array::insert", None) }
        
    }))
}

fn arr_set(args: Vec<Values>) -> Res {
    eval_dot_func(args, 2, "array::set", true, 
    Box::new(|val : &mut Values, mut args: Vec<Values>| -> Res {
        if let Values::Array(val) = val {
            match (args.swap_remove(0), args.pop().unwrap()) {
                (Values::Int(idx), v) if idx >= 0 && idx < val.len() as i32 =>
                    val[idx as usize] = v,
                (Values::Int(_), _) => return str_exn(IDX_BNDS),
                _ => return inv_arg("array::set", None),
            };
            Vl(Values::Unit)
        } else { inv_arg("array::set", None) }
        
    }))
}

fn arr_remove(args: Vec<Values>) -> Res {
    eval_dot_func(args, 1, "array::remove", true, 
    Box::new(|val : &mut Values, args: Vec<Values>| -> Res {
        if let Values::Array(val) = val {
            if let Values::Int(idx) = args[0] {
                if idx >= 0 && idx < val.len() as i32 {
                    val.remove(idx as usize);
                    Vl(Values::Unit)
                } else { str_exn(IDX_BNDS) }
            } else { inv_arg("array::remove", None) }
        } else { inv_arg("array::remove", None) }      
    }))
}



/// True if `arr` contains all of the vales from the evaluated asts in `args`
fn map_contains_all(args: Vec<Values>) -> Res
{
    eval_dot_func(args, 1, "map::contains", false, 
    Box::new(|first: &mut Values, args: Vec<Values>| -> Res {
        if let Values::Map(x) = first {
            for a in args.into_iter() {
                if let Values::Str(name) = a {
                    if x.get(&name) == None {
                        return Vl(Values::Bool(false))
                    }
                } else { 
                    return inv_arg("map::contains", Some("Must pass string keys"));
                }
            }
            Vl(Values::Bool(true))
        } else { inv_arg("map::contains", None) }
    }))
}

fn map_insert(args: Vec<Values>) -> Res {
    eval_dot_func(args, 1, "map::insert", true, Box::new(
        |first: &mut Values, args: Vec<Values>| -> Res {
            if let Values::Map(map) = first {
                map_insert_helper(map, args)
            } else { inv_arg("map::insert", None) }
        }
    ))
}

fn map_insert_helper(map: &mut HashMap<String, Values>, mut args: Vec<Values>) -> Res 
{
    if args.len() == 1 {
        match args.pop().unwrap() {
            Values::Tuple(mut x) if x.len() == 2 => {
                match (x.swap_remove(0), x.pop().unwrap()) {
                    (Values::Str(x), val) => {
                        map.insert(x, val);
                        Vl(Values::Unit)
                    },
                    _ => inv_arg("map::insert",
                        Some("Inserting tuple must be a string value pair")),
                }
            },
            _ => inv_arg("map::insert", None),
        }
    }
    else if args.len() == 2 {
        match (args.swap_remove(0), args.pop().unwrap()) {
            (Values::Str(key), val) => {
                map.insert(key, val);
                Vl(Values::Unit)
            },
            _ => inv_arg("map::insert", 
                Some("Can only insert kv pairs into a map")),

        }
    } else {
        inv_arg("map::insert", Some("Must insert a key/value pair!"))
    }
}

fn map_remove(args: Vec<Values>) -> Res {
    eval_dot_func(args, 1, "map::remove", true, 
    Box::new(|val : &mut Values, args: Vec<Values>| -> Res {
        if let Values::Map(val) = val {
            for a in args.into_iter() {
                if let Values::Str(key) = a {
                    val.remove(&key);
                } else { return inv_arg("map::remove", Some("Only strings allowed")); }
            }
            Vl(Values::Unit)
        } else { inv_arg("map::remove", None) }      
    }))
}