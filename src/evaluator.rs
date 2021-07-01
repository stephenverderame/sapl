use crate::parser::Ast;
use crate::parser::Op;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
mod environment;
use environment::*;
pub use environment::{Scope, Environment};
mod std_sapl;
use std_sapl::type_of;
mod exn;
use exn::*;
mod vals;
pub use vals::*;
use Res::Vl;
use Res::Bad;
mod control_flow;
use control_flow::*;
mod eval_bop;
use eval_bop::*;
mod eval_functions;
use eval_functions::*;
mod eval_class;
use eval_class::*;

pub use std_sapl::get_std_environment;

/// Evaluates a SAPL AST
pub fn evaluate(ast: &Ast) -> Res {
    eval(ast, &mut std_sapl::get_std_environment())
}

/// Evaluates the ast to a value
pub fn eval(ast: &Ast, scope: &mut impl Environment) -> Res {
    match eval_keep_all(ast, scope) {
        Vl(Values::WeakRef(ptr, _)) => 
            Vl(ptr.upgrade().unwrap().borrow().clone()),
        e => e,
    }
}

fn eval_keep_all(ast: &Ast, scope: &mut impl Environment) -> Res {
    match ast {
        Ast::VFloat(x) => Vl(Values::Float(*x)),
        Ast::VInt(x) => Vl(Values::Int(*x)),
        Ast::VStr(x) => Vl(Values::Str(x.clone())),
        Ast::VBool(x) => Vl(Values::Bool(*x)),
        Ast::Name(x) => match name_lookup(&x, scope) {
            Some((val, _)) => Vl(val.borrow().clone()),
            _ => ukn_name(x),
        },
        Ast::Bop(left, op, right) => eval_bop(&*left, op, &*right, scope),
        Ast::If(guard, body, other) => eval_if(&*guard, &*body, other, scope),
        Ast::Seq(children) => eval_seq(children, scope),
        Ast::Let(name, ast) if are_valid_names(name, scope) => eval_let(name, &*ast, scope, false),
        Ast::Func(name, params, ast, post) if is_valid_name(name, scope) => 
            eval_func(name, params, &*ast, post, scope, false),
        Ast::Let(..) | Ast::Func(..) =>
            str_exn("Cannot name a value a type name"),
        Ast::Lambda(params, ast) => eval_lambda(params, &*ast, scope),
        Ast::FnApply(left, args) => 
            eval_fn_app(&*left, args, scope),
        Ast::Array(elems) => eval_arr(elems, scope),
        Ast::Uop(ast, op) => eval_uop(ast, op, scope),
        Ast::Tuple(elems) => eval_tuple(elems, scope),
        Ast::Map(es) => eval_map(&*es, scope),
        Ast::Try(body, var, catch) => eval_try(&*body, &var[..], &*catch, scope),
        Ast::For(vars, iter, if_expr, body) => eval_for(vars, &*iter, if_expr, &*body, scope),
        Ast::While(guard, body) => eval_while(&*guard, &*body, scope),
        Ast::Placeholder => Vl(Values::Placeholder),
        Ast::Struct(class) => eval_class_def(class, scope, false),
        Ast::Type(interface) => eval_type_def(interface, scope, false),
        Ast::Export(ast) => eval_export(&*ast, scope),
        Ast::Import(file, prefix) => eval_import(file, prefix, scope, false),
    }
}

fn are_valid_names(names: &Vec<(String, bool)>, scope: &impl Environment) -> bool {
    for (name, _) in names {
        if !is_valid_name(name, scope) { return false; }
    }
    true
}

fn is_valid_name(name: &String, scope: &impl Environment) -> bool {
    match &name[..] {
        "int" | "float" | "string" |
        "object" | "tuple" | "array" |
        "map" | "none" | "some" | "range" |
        "bool" | "number" | "ref" => false,
        _ if name.starts_with("tuple_") && 
            name[name.find('_').unwrap() + 1..name.len()].parse::<i32>().is_ok() => false,
    _ => {
            if let Some((ptr, _)) = scope.find(name) {
                if let Values::Object(..) = &*ptr.borrow() { false }
                else if let Values::Type(..) = &*ptr.borrow() { false }
                else { true }
            } else { true }
        },
    }
}

fn eval_ref(left: &Ast, op: &Op, scope: &mut impl Environment) -> Res {
    let mutable = op != &Op::Ref;
    match left {
        Ast::Name(x) => {
            if let Some((ptr, mtble)) = name_lookup(&x, scope) {
                if !mutable || mutable && mtble {
                    Vl(Values::Ref(ptr.clone(), mutable))
                } else {
                    str_exn(IMMU_ERR)
                }
            } else { ukn_name(x) }
        },
        _ => {
            match eval(left, scope) {
                Vl(Values::Ref(ptr, mt)) if !mutable || mt && mutable => {
                    Vl(Values::Ref(ptr.clone(), mutable))
                },
                Vl(Values::Ref(..)) => str_exn(IMMU_ERR),
                Vl(val) => {
                    Vl(Values::Ref(Rc::new(RefCell::new(val)), mutable))
                },
                e => e,
            }
        },
    }
}

/// Iterates over the valid sapl range start..end
/// Reverses the range if `start < end`
/// Passes the index to `func` on each iteration
fn iter_over(start: i32, end: i32, func: &mut dyn FnMut(i32) -> Option<Res>) -> Option<Res> {
    let start = start;
    let end = end;
    let iter = if start < end { 
        Box::new(start..end) as Box<dyn Iterator<Item = _>>
    } else { 
        Box::new((end.. start).rev())
    };
    for i in iter {
        match func(i) {
            None => (),
            s => return s,
        }
    }
    None
}

fn scope_add(scope: &mut impl Environment, name: &str, val: Values, 
    is_mut: bool, is_pub: bool) 
{
    let name = format!("{}{}", if is_pub {"export::"} else {""}, name);
    scope.add(name, val, is_mut);
}

fn scope_add_dir(scope: &mut impl Environment, name: &str, val: Rc<RefCell<Values>>, 
    is_mut: bool, is_pub: bool) 
{
    let name = format!("{}{}", if is_pub {"export::"} else {""}, name);
    scope.add_direct(name, val, is_mut);
}

/// Evaluates a let definition by adding `name` to `scope`
/// Returns unit on success
fn eval_let(names: &Vec<(String, bool)>, ast: &Ast, 
    scope: &mut impl Environment, public: bool) 
    -> Res 
{
    match eval(ast, scope) {
        Vl(val) if names.len() == 1 => {
            let (nm, is_mut) = names.get(0).unwrap();
            scope_add(scope, nm, val, *is_mut, public);
            Vl(Values::Unit)
        },
        Vl(Values::Range(a, b)) if names.len() == 2 => {
            let (nm1, mut1) = &names[0];
            let (nm2, mut2) = &names[1];
            scope_add(scope, nm1, *a, *mut1, public);
            scope_add(scope, nm2, *b, *mut2, public);
            Vl(Values::Unit)
        },
        Vl(Values::Tuple(es)) if names.len() == es.len() => {
            for (nm, v) in names.iter().zip(es.into_iter()) {
                let (nm, is_mut) = nm;
                scope_add(scope, nm, v, *is_mut, public);
            }
            Vl(Values::Unit)
        },
        Vl(_) => str_exn(INV_DEF),
        err => err,
    }
}

/// Evaluates the unary operator `op left`
fn eval_uop(left: &Ast, op: &Op, scope: &mut impl Environment) -> Res {
    if op == &Op::Ref || op == &Op::MutRef { return eval_ref(left, op, scope); }
    perform_uop(eval(left, scope), op, scope)
}

fn perform_uop(left: Res, op: &Op, scope: &mut impl Environment) -> Res {
    match (left, op) {
        (Vl(Values::Bool(x)), Op::Not) => Vl(Values::Bool(!x)),
        (Vl(Values::Int(x)), Op::Neg) | 
        (Vl(Values::Int(x)), Op::Not) => Vl(Values::Int(-x)),
        (Vl(Values::Float(x)), Op::Neg) | 
        (Vl(Values::Float(x)), Op::Not) => Vl(Values::Float(-x)),
        (Vl(Values::Ref(x, _)), Op::Deref) => deref(x),
        (v, Op::AsBool) => match v {
            Vl(_) | Res::Ret(_) => Vl(Values::Bool(true)),
            Res::Exn(_) | Bad(_) => Vl(Values::Bool(false)),
        },
        (Vl(v), Op::Return) => Res::Ret(v),
        (Vl(v), Op::Throw) => Res::Exn(v),
        (Vl(Values::Str(path)), Op::Include) => eval_include(path, scope),
        (Vl(v), x) => bad_op(&v, None, *x),
        (e, _) => e,
    }
}

/// Repeatedly dereferences a ptr and returns a copy of the value
fn deref(mut ptr: Rc<RefCell<Values>>) -> Res {
    while let Values::Ref(r, _) = &*ptr.clone().borrow() {
        ptr = r.clone();
    }
    Vl(ptr.borrow().clone())
}

fn eval_include(path: String, scope: &mut impl Environment) -> Res {
    use std::fs;
    match fs::File::open(&path) {
        Ok(p) => {
            crate::parse_and_eval(p, &mut Some(scope))
        },
        Err(_) => str_exn(&format!("Cannot open include file {}", path)),
    }
}

/// Evaluates an array of asts into an array of values
fn eval_arr(elems: &Vec<Box<Ast>>, scope: &mut impl Environment) -> Res {
    let mut lst = Vec::<Values>::new();
    for expr in elems {
        match eval(expr, scope) {
            Vl(val) => lst.push(val),
            e => return e,
        }
    }
    Vl(Values::Array(Box::new(lst)))
}

fn eval_tuple(elems: &Vec<Box<Ast>>, scope: &mut impl Environment) -> Res {
    let mut lst = Vec::<Values>::new();
    for expr in elems {
        match eval(expr, scope) {
            Vl(val) => lst.push(val),
            e => return e,
        }
    }
    Vl(Values::Tuple(Box::new(lst)))
}

/// Evaluates a map
fn eval_map(es: &Vec<(Ast, Ast)>, scope: &mut impl Environment) -> Res {
    let mut map = HashMap::<String, Values>::new();
    for (k, v) in es {
        match (eval(k, scope), eval(v, scope)) {
            (Vl(Values::Str(nm)), Vl(val)) => {
                map.insert(nm, val); ()
            },
            (Vl(_), _) => 
                return inv_arg("map", Some("Map keys must be strings")),
            (e, _) => return e,
        }
    }
    Vl(Values::Map(Box::new(map)))
}

fn eval_export(def: &Ast, scope: &mut impl Environment) -> Res {
    match def {
        Ast::Let(names, ast) => eval_let(&names, &*ast, scope, true),
        Ast::Func(name, args, body, pce) => 
            eval_func(&name, &args, &*body, &pce, scope, true),
        Ast::Struct(class) => eval_class_def(&class, scope, true),
        Ast::Type(class) => eval_type_def(&class, scope, true),
        Ast::Import(file, prefix) => eval_import(file, prefix, scope, true),
        e => str_exn(&format!(
            "Expected an export definition following pub. Got {:?}", e)),
    }
}

fn name_lookup(name: &str, scope: &impl Environment) -> Option<(Rc<RefCell<Values>>, bool)> {
    if let Some(p) = scope.find(name) { 
        return Some(p); 
    } 
    if !name.contains("::") {
        if let Some(p) = scope.find(&format!("export::{}", name)) {
            return Some(p)
        } 
    }  
    None

    
}

fn eval_import(file: &str, prefix: &Option<String>, scope: &mut impl Environment, public: bool) -> Res {    
    match std::fs::File::open(file) {
        Ok(file) => {
            let mut env = get_std_environment();
            match crate::parse_and_eval(file, &mut Some(&mut env)) {
                Res::Vl(_) | Res::Ret(_) => (),
                e => return e,
            };
            let v = env.find_all_in_scope("export");
            for (name, val, var) in v {
                let name = if let Some(string) = prefix {
                    format!("{}::{}", string, name.replace("export::", ""))
                } else { name };
                scope_add_dir(scope, &name, val.clone(), var, public);
            }
            Vl(Values::Unit)
        },
        _ => str_exn(&format!("Could not open import file {}", file)),
    }
}
