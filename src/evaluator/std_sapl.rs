use super::environment::Environment;
use super::environment::Scope;
use super::eval_class::Class;
use super::eval_class::Member;
use super::exn::*;
use super::Res;
use super::Values;
use rand::Rng;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::rc::Rc;

use super::Res::Vl;

pub fn get_std_environment() -> Scope {
    let mut scope = Scope::new();
    add_func(&mut scope, "typeof", eval_type, 1);
    add_func(&mut scope, "assert", eval_assert, 1);
    add_func(&mut scope, "len", eval_len, 1);
    add_func(&mut scope, "random", eval_rand, 1);
    add_func(&mut scope, "array::push_back", arr_push_back, 2);
    add_func(&mut scope, "array::contains", arr_contains_all, 2);
    add_func(&mut scope, "map::contains", map_contains_all, 2);
    add_func(&mut scope, "array::set", arr_set, 3);
    add_func(&mut scope, "array::insert", arr_insert, 3);
    add_func(&mut scope, "array::remove", arr_remove, 2);
    add_func(&mut scope, "map::insert", map_insert, 2);
    add_func(&mut scope, "map::remove", map_remove, 2);
    add_func(&mut scope, "template", template_file, 2);
    add_func(&mut scope, "cinln", eval_rdline, 0);
    add_func(&mut scope, "cout", eval_cout, 1);
    add_func(&mut scope, "coutln", eval_cout_ln, 0);
    add_func(&mut scope, "print", eval_cout, 1);
    add_func(&mut scope, "println", eval_cout_ln, 0);
    add_func(&mut scope, "string::contains", string_contains, 2);
    add_func(&mut scope, "string::split", str_split, 2);
    add_func(&mut scope, "Some", eval_some, 1);
    add_func(&mut scope, "clone", eval_clone, 1);
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
        Values::Ref(_, _) | Values::WeakRef(..) => "ref".to_owned(), //type_of(&ptr.borrow()),
        Values::RustFunc(..) => "function".to_owned(),
        Values::Object(ptr, _) => {
            let Class { name, .. } = &*ptr.borrow();
            name.clone()
        }
        Values::Type(ptr) => {
            let Class { name, .. } = &**ptr;
            name.clone()
        }
    }
}

fn add_func(scope: &mut Scope, name: &str, func: fn(Vec<Values>) -> Res, min_args: usize) {
    scope.add(
        name.to_owned(),
        Values::RustFunc(Rc::new(func), min_args),
        false,
    );
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
        let first = args.swap_remove(0);
        match (first, args.pop().unwrap()) {
            (Values::Int(min), Values::Int(max)) => Vl(Values::Int(rng.gen_range(min..max))),
            (Values::Float(min), Values::Float(max)) => Vl(Values::Float(rng.gen_range(min..max))),
            _ => inv_arg("random", None),
        }
    } else if args.len() == 1 {
        match args.pop().unwrap() {
            Values::Range(min, max) => match (*min, *max) {
                (Values::Int(start), Values::Int(end)) => {
                    Vl(Values::Int(rng.gen_range(start..end)))
                }
                (Values::Float(start), Values::Float(end)) => {
                    Vl(Values::Float(rng.gen_range(start..end)))
                }
                _ => inv_arg("random", None),
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
        }
        Values::Array(x) => x.len() as i32,
        Values::Map(x) => x.len() as i32,
        Values::Tuple(x) => x.len() as i32,
        Values::Unit => 0,
        Values::Ref(ptr, _) => size_of(&ptr.borrow()),
        Values::WeakRef(ptr, _) => size_of(&ptr.upgrade().unwrap().borrow()),
        Values::Object(ptr, cc) => {
            let Class {
                name: _, members, ..
            } = &*ptr.borrow();
            if let Some(Member { val, .. }) = members.get("__len__") {
                if let func @ Values::RustFunc(..) = &*val.borrow() {
                    match super::eval_functions::apply_function(
                        func,
                        vec![Values::Object(ptr.clone(), *cc)],
                        false,
                        false,
                    ) {
                        Vl(Values::Int(x)) => return x,
                        e => panic!("Error evaluating custom length function: {:?}", e),
                    }
                }
            }
            members.len() as i32
        }
        Values::Type(ptr) => {
            let Class {
                name: _, members, ..
            } = &**ptr;
            members.len() as i32
        }
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
    let closure = |val: &mut Values, args: Vec<Values>| -> Res {
        if let Values::Array(val) = val {
            for a in args.into_iter() {
                val.push(a);
            }
            Vl(Values::Unit)
        } else {
            inv_arg("array::push_back", None)
        }
    };
    eval_dot_func(args, 1, "array::push_back", true, Box::new(closure))
}

/// Evaluates the arguments of the dot application
/// Ensures that `min_args` are passed (excluding the dot argument)
/// Ensures that only mutable values are passed when `need_mut` is true
/// Calls `closure` passing the dot argument and remaining arguments if all is well
fn eval_dot_func(
    mut args: Vec<Values>,
    min_args: usize,
    name: &str,
    need_mut: bool,
    closure: Box<dyn FnOnce(&mut Values, Vec<Values>) -> Res>,
) -> Res {
    if args.len() < min_args + 1 {
        return inv_arg(name, Some("Not enough arguments"));
    }
    let context = args.remove(0);
    match context {
        Values::Ref(ptr, _) if !need_mut => closure(&mut *ptr.borrow_mut(), args),
        Values::Ref(ptr, true) if need_mut => closure(&mut *ptr.borrow_mut(), args),
        Values::WeakRef(ptr, _) if !need_mut => {
            closure(&mut *ptr.upgrade().unwrap().borrow_mut(), args)
        }
        Values::WeakRef(ptr, true) => closure(&mut *ptr.upgrade().unwrap().borrow_mut(), args),
        x @ Values::Ref(..) | x @ Values::WeakRef(..) => {
            str_exn(&format!("{} during dot application of {:?}", IMMU_ERR, x))
        }
        mut v => closure(&mut v, args),
    }
}

/// True if `arr` contains all of the vales from the evaluated asts in `args`
fn arr_contains_all(args: Vec<Values>) -> Res {
    eval_dot_func(
        args,
        1,
        "array::contains",
        false,
        Box::new(|first: &mut Values, args: Vec<Values>| -> Res {
            if let Values::Array(x) = first {
                for a in args.into_iter() {
                    if x.iter().find(|bx| **bx == a) == None {
                        return Vl(Values::Bool(false));
                    }
                }
                Vl(Values::Bool(true))
            } else {
                inv_arg("array::contains", None)
            }
        }),
    )
}

fn arr_insert(args: Vec<Values>) -> Res {
    eval_dot_func(
        args,
        2,
        "array::insert",
        true,
        Box::new(|val: &mut Values, mut args: Vec<Values>| -> Res {
            if let Values::Array(val) = val {
                let first = args.swap_remove(0);
                match (first, args.pop().unwrap()) {
                    (Values::Int(idx), v) if idx >= 0 && idx < val.len() as i32 => {
                        val.insert(idx as usize, v)
                    }
                    (Values::Int(idx), v) if idx >= val.len() as i32 => val.push(v),
                    (Values::Int(_), _) => return str_exn(IDX_BNDS),
                    (x, _) => {
                        return inv_arg(
                            "array::insert",
                            Some(&format!("Index must be an int. Got '{:?}'", x)[..]),
                        )
                    }
                };
                Vl(Values::Unit)
            } else {
                inv_arg("array::insert", None)
            }
        }),
    )
}

fn arr_set(args: Vec<Values>) -> Res {
    eval_dot_func(
        args,
        2,
        "array::set",
        true,
        Box::new(|val: &mut Values, mut args: Vec<Values>| -> Res {
            if let Values::Array(val) = val {
                let first = args.swap_remove(0);
                match (first, args.pop().unwrap()) {
                    (Values::Int(idx), v) if idx >= 0 && idx < val.len() as i32 => {
                        val[idx as usize] = v
                    }
                    (Values::Int(_), _) => return str_exn(IDX_BNDS),
                    _ => return inv_arg("array::set", None),
                };
                Vl(Values::Unit)
            } else {
                inv_arg("array::set", None)
            }
        }),
    )
}

fn arr_remove(args: Vec<Values>) -> Res {
    eval_dot_func(
        args,
        1,
        "array::remove",
        true,
        Box::new(|val: &mut Values, args: Vec<Values>| -> Res {
            if let Values::Array(val) = val {
                if let Values::Int(idx) = args[0] {
                    if idx >= 0 && idx < val.len() as i32 {
                        val.remove(idx as usize);
                        Vl(Values::Unit)
                    } else {
                        str_exn(IDX_BNDS)
                    }
                } else {
                    inv_arg("array::remove", None)
                }
            } else {
                inv_arg("array::remove", None)
            }
        }),
    )
}

/// True if `arr` contains all of the vales from the evaluated asts in `args`
fn map_contains_all(args: Vec<Values>) -> Res {
    eval_dot_func(
        args,
        1,
        "map::contains",
        false,
        Box::new(|first: &mut Values, args: Vec<Values>| -> Res {
            if let Values::Map(x) = first {
                for a in args.into_iter() {
                    if x.get(&a) == None {
                        return Vl(Values::Bool(false));
                    }
                }
                Vl(Values::Bool(true))
            } else {
                inv_arg("map::contains", None)
            }
        }),
    )
}

fn map_insert(args: Vec<Values>) -> Res {
    eval_dot_func(
        args,
        1,
        "map::insert",
        true,
        Box::new(|first: &mut Values, args: Vec<Values>| -> Res {
            if let Values::Map(map) = first {
                map_insert_helper(map, args)
            } else {
                inv_arg("map::insert", None)
            }
        }),
    )
}

fn map_insert_helper(map: &mut HashMap<Values, Values>, mut args: Vec<Values>) -> Res {
    if args.len() == 1 {
        match args.pop().unwrap() {
            Values::Tuple(mut x) if x.len() == 2 => {
                let first = x.swap_remove(0);
                let val = x.pop().unwrap();
                map.insert(first, val);
                Vl(Values::Unit)
            }
            _ => inv_arg("map::insert", None),
        }
    } else if args.len() == 2 {
        let first = args.swap_remove(0);
        let val = args.pop().unwrap();
        map.insert(first, val);
        Vl(Values::Unit)
    } else {
        inv_arg("map::insert", Some("Must insert a key/value pair!"))
    }
}

fn map_remove(args: Vec<Values>) -> Res {
    eval_dot_func(
        args,
        1,
        "map::remove",
        true,
        Box::new(|val: &mut Values, args: Vec<Values>| -> Res {
            if let Values::Map(val) = val {
                for key in args.into_iter() {
                    val.remove(&key);
                }
                Vl(Values::Unit)
            } else {
                inv_arg("map::remove", None)
            }
        }),
    )
}

fn template_file(mut args: Vec<Values>) -> Res {
    if args.len() == 2 {
        let first = args.swap_remove(0);
        eval_template(first, args.pop().unwrap(), "$$")
    } else if args.len() == 3 {
        if let Some(Values::Str(delim)) = args.pop() {
            let first = args.swap_remove(0);
            eval_template(first, args.pop().unwrap(), &delim[..])
        } else {
            inv_arg("template", None)
        }
    } else {
        inv_arg("template", None)
    }
}

fn eval_template(file: Values, map: Values, delim: &str) -> Res {
    match (file, map) {
        (Values::Str(file_name), Values::Map(map)) => {
            if let Ok(file) = fs::File::open(&file_name) {
                let mut scope = append_map_to_std_env(&map);
                let res = Rc::new(RefCell::new(Vec::<u8>::new()));
                crate::parse_sapl_inplace(file, res.clone(), delim, &mut Some(&mut scope));
                let x = Vl(Values::Str(
                    String::from_utf8(res.borrow().clone()).unwrap(),
                ));
                x
            } else {
                str_exn(&format!("{} cannot be opened", file_name))
            }
        }
        _ => inv_arg("template", None),
    }
}

fn append_map_to_std_env(map: &HashMap<Values, Values>) -> Scope {
    let mut sc = get_std_environment();
    for (name, val) in map {
        if let Values::Str(name) = name {
            sc.add(name.to_string(), val.clone(), false);
        } else {
            panic!("Map keys to add to environment must be strings!");
        }
    }
    sc
}

fn eval_rdline(_: Vec<Values>) -> Res {
    use std::io;
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
    Vl(Values::Str(buf.trim_end().to_owned()))
}

fn eval_cout(args: Vec<Values>) -> Res {
    print_args(args, &mut std::io::stdout(), false);
    Vl(Values::Unit)
}

fn eval_cout_ln(args: Vec<Values>) -> Res {
    print_args(args, &mut std::io::stdout(), true);
    Vl(Values::Unit)
}

fn print_args(args: Vec<Values>, out: &mut impl Write, newline: bool) {
    for arg in args {
        out.write_all(format!("{}", arg).as_bytes()).unwrap();
    }
    if newline {
        out.write_all("\n".as_bytes()).unwrap()
    }
    out.flush().unwrap();
}

fn string_contains(mut args: Vec<Values>) -> Res {
    let fst = args.swap_remove(0);
    match fst {
        Values::Str(s) => eval_str_contains(&s, args),
        Values::Ref(ptr, _) => {
            if let Values::Str(s) = &*ptr.borrow() {
                eval_str_contains(s, args)
            } else {
                inv_arg(
                    "string::contains",
                    Some(&format!("First argument must be a string. Got {:?}", ptr)),
                )
            }
        }
        fst => inv_arg(
            "string::contains",
            Some(&format!("First argument must be a string. Got {:?}", fst)),
        ),
    }
}

fn eval_str_contains(string: &String, args: Vec<Values>) -> Res {
    for a in args {
        let needle = format!("{}", a);
        if string.find(&needle).is_none() {
            return Vl(Values::Bool(false));
        }
    }
    Vl(Values::Bool(true))
}

fn str_split(mut args: Vec<Values>) -> Res {
    let fst = args.swap_remove(0);
    match (fst, args.pop().unwrap()) {
        (Values::Str(s), Values::Str(s2)) => eval_str_split(&s, &s2),
        (Values::Ref(ptr, _), Values::Str(s2)) => {
            if let Values::Str(s) = &*ptr.borrow() {
                eval_str_split(s, &s2)
            } else {
                inv_arg(
                    "string::split",
                    Some(&format!("First argument must be a string. Got {:?}", ptr)),
                )
            }
        }
        fst => inv_arg(
            "string::split",
            Some(&format!("First argument must be a string. Got {:?}", fst)),
        ),
    }
}

fn eval_str_split(string: &String, delim: &String) -> Res {
    let array = string.split(delim);
    let mut res_array = Vec::<Values>::new();
    for elem in array {
        res_array.push(Values::Str(elem.to_string()));
    }
    Vl(Values::Array(Box::new(res_array)))
}

fn eval_some(mut args: Vec<Values>) -> Res {
    if args.len() == 1 {
        let vl = args.pop().unwrap();
        Vl(Values::Tuple(Box::new(vec![vl])))
    } else {
        inv_arg(
            "Some",
            Some(&format!("Expected 1 arguments. Got {:?}", args)),
        )
    }
}

fn eval_clone(mut args: Vec<Values>) -> Res {
    if args.len() == 1 {
        let arg = args.pop().unwrap();
        do_clone(&arg)
    } else {
        inv_arg("clone", Some("expected 1 argument"))
    }
}

fn do_clone(val: &Values) -> Res {
    match val {
        Values::Ref(data, mu) => match do_clone(&*data.borrow()) {
            Vl(v) => Vl(Values::Ref(Rc::new(RefCell::new(v)), *mu)),
            e => e,
        },
        Values::WeakRef(data, _) => do_clone(&*data.upgrade().unwrap().borrow()),
        Values::Object(data, cc) => {
            let Class { members, .. } = &*data.borrow();
            if let Some(Member { val: v, .. }) = members.get("__clone__") {
                if let f @ Values::RustFunc(..) = &*v.borrow() {
                    return super::eval_functions::apply_function(
                        f,
                        vec![Values::Object(data.clone(), *cc)],
                        false,
                        false,
                    );
                }
            }
            Vl(Values::Object(
                Rc::new(RefCell::new(data.borrow().clone())),
                *cc,
            ))
        }
        x => Vl(x.clone()),
    }
}
