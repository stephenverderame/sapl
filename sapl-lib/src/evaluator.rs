use crate::parser::Ast;
use crate::parser::Op;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
mod environment;
use environment::*;
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

/// Evaluates a SAPL AST
pub fn evaluate(ast: &Ast) -> Res {
    eval(ast, &mut std_sapl::get_std_environment())
}

/// Evaluates the ast to a value
fn eval(ast: &Ast, scope: &mut impl Environment) -> Res {
    match ast {
        Ast::VFloat(x) => Vl(Values::Float(*x)),
        Ast::VInt(x) => Vl(Values::Int(*x)),
        Ast::VStr(x) => Vl(Values::Str(x.clone())),
        Ast::VBool(x) => Vl(Values::Bool(*x)),
        Ast::Name(x) => match scope.find(x) {
            Some((val, _)) => Vl(val.borrow().clone()),
            _ => ukn_name(x),
        },
        Ast::Bop(left, op, right) => eval_bop(&*left, op, &*right, scope),
        Ast::If(guard, body, other) => eval_if(&*guard, &*body, other, scope),
        Ast::Seq(children) => eval_seq(children, scope),
        Ast::Let(name, ast) => eval_let(name, &*ast, scope),
        Ast::Func(name, params, ast, post) => 
            eval_func(name, params, &*ast, post, scope),
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
    }
}

fn eval_ref(left: &Ast, op: &Op, scope: &mut impl Environment) -> Res {
    let mutable = op != &Op::Ref;
    match left {
        Ast::Name(x) => {
            if let Some((ptr, mtble)) = scope.find(&x[..]) {
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

/// Evaluates a let definition by adding `name` to `scope`
/// Returns unit on success
fn eval_let(names: &Vec<(String, bool)>, ast: &Ast, scope: &mut impl Environment) 
    -> Res 
{
    match eval(ast, scope) {
        Vl(val) if names.len() == 1 => {
            let (nm, is_mut) = names.get(0).unwrap();
            scope.add(nm.to_string(), val, *is_mut);
            Vl(Values::Unit)
        },
        Vl(Values::Range(a, b)) if names.len() == 2 => {
            let (nm1, mut1) = &names[0];
            let (nm2, mut2) = &names[1];
            scope.add(nm1.to_string(), *a, *mut1);
            scope.add(nm2.to_string(), *b, *mut2);
            Vl(Values::Unit)
        },
        Vl(Values::Tuple(es)) if names.len() == es.len() => {
            for (nm, v) in names.iter().zip(es.into_iter()) {
                let (nm, is_mut) = nm;
                scope.add(nm.to_string(), v, *is_mut);
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
    match (eval(left, scope), op) {
        (Vl(Values::Bool(x)), Op::Not) => Vl(Values::Bool(!x)),
        (Vl(Values::Int(x)), Op::Neg) | 
        (Vl(Values::Int(x)), Op::Not) => Vl(Values::Int(-x)),
        (Vl(Values::Float(x)), Op::Neg) | 
        (Vl(Values::Float(x)), Op::Not) => Vl(Values::Float(-x)),
        (Vl(Values::Ref(x, _)), Op::Deref) => Vl(x.borrow().clone()),
        (v, Op::AsBool) => match v {
            Vl(_) | Res::Ret(_) => Vl(Values::Bool(true)),
            Res::Exn(_) | Bad(_) => Vl(Values::Bool(false)),
        },
        (Vl(v), Op::Return) => Res::Ret(v),
        (Vl(v), Op::Throw) => Res::Exn(v),
        (Vl(v), x) => bad_op(&v, None, *x),
        (e, _) => e,
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

