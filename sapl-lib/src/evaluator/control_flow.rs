use super::eval;
use crate::evaluator::Values;
use crate::evaluator::Res;
use Res::Vl;
use Res::Bad;
use super::exn::*;
use crate::parser::Ast;
use super::environment::*;
use super::vals::to_booly;
use std::collections::HashMap;

/// Evaluates a try block
pub fn eval_try(body: &Ast, catch_var: &str, catch_body: &Ast, scope: &mut impl Environment) -> Res {
    scope.new_scope();
    match eval(body, scope) {
        Res::Exn(exn) => {
            scope.pop_scope();
            let mut child = ScopeProxy::new(scope);
            child.add(catch_var.to_owned(), exn, false);
            eval(catch_body, &mut child)
        },
        e => {
            scope.pop_scope();
            e
        },
    }
}

/// Evaluates a for loop
pub fn eval_for(names: &Vec<String>, iter: &Ast, if_expr: &Option<Box<Ast>>, 
    body: &Ast, scope: &mut impl Environment) -> Res
{
    let ife = if let Some(ast) = if_expr {
        Some(&**ast)
    } else { None };
    match eval(iter, scope) {
        Vl(Values::Range(start, end)) if names.len() == 1 => 
            eval_for_range(*start, *end, &names[0], ife, body, scope),
        Vl(Values::Array(vals)) =>
            eval_for_array(*vals, names, ife, body, scope),
        Vl(Values::Map(map)) => 
            eval_for_map(*map, names, ife, body, scope),
        _ => str_exn(NON_ITER),

    }
}

/// Evaluates a while loop
pub fn eval_while(guard: &Ast, body: &Ast, scope: &mut impl Environment) -> Res {
    loop {
        match eval(guard, scope) {
            Vl(val) => 
                if Values::Bool(true) == val {
                    match eval(body, &mut ScopeProxy::new(scope)) {
                        Vl(_) => (),
                        e => return e,
                    }
                } else { break },
            e => return e,
        }
    }
    Vl(Values::Unit)
}

/// Evaluates an If expression
pub fn eval_if(guard: &Ast, body: &Ast, other: &Option<Box<Ast>>, 
    scope: &mut impl Environment) -> Res 
{
    match to_booly(&eval(guard, scope)) {
        Ok(true) => eval(body, &mut ScopeProxy::new(scope)),
        Ok(false) => {
            if let Some(ast) = other {
                eval(&*ast, &mut ScopeProxy::new(scope))
            } else {
                Vl(Values::Unit)
            }
        },
        Err(e) => Bad(e),
    }
}

/// Evaluates a sequence
pub fn eval_seq(children: &Vec<Box<Ast>>, scope: &mut impl Environment) 
    -> Res 
{
    let mut last_res : Res = 
        Bad("No children in sequence".to_owned());
    for subtree in children {
        match eval(&*subtree, scope) {
            x @ Vl(_) => last_res = x,
            x => return x,
        }
    }
    last_res
}

/// Evaluates a for loop over a range
fn eval_for_range(start: Values, end: Values, name: &String, ife: Option<&Ast>,
    body: &Ast, scope: &mut impl Environment) -> Res 
{
    if let (Values::Int(start), Values::Int(end)) = (start, end) {
        let min = std::cmp::min(start, end);
        let max = std::cmp::max(start, end);
        let iter = if min == start { 
            Box::new(min..max) as Box<dyn Iterator<Item = _>>
        } else { 
            Box::new((min..max).rev())
        };

        for i in iter {
            let mut child = ScopeProxy::new(scope);
            child.add(name.to_owned(), Values::Int(i), false);

            if !filter_out_for_loop_iter(ife, &mut child) {
                match eval(body, &mut child) {
                    Vl(_) => (),
                    e => return e,
                }
            }
        }
        Vl(Values::Unit)
    } else {
        str_exn(NON_ITER)
    }
}

/// Determines if the for loop iteration will be skipped
/// Does not skip if `expr` is None or evaluating `expr` results in a value that
/// is true-ish (true when applied to `to_booly`)
fn filter_out_for_loop_iter(expr: Option<&Ast>, scope: &mut impl Environment) -> bool {
    if let Some(ast) = expr {
        match eval(ast, scope) {
            x if to_booly(&x) == Ok(true) => false,
            _ => true,
        }
    } else { false }
}

fn eval_for_array(arr: Vec<Values>, names: &Vec<String>, 
    ife: Option<&Ast>, body: &Ast, scope: &mut impl Environment) -> Res 
{
    for val in arr.into_iter() {
        let mut child_scope = ScopeProxy::new(scope);
        match (names.len(), val) {
            (x, Values::Tuple(tup)) if x == tup.len() => {
                for (name, v) in names.iter().zip(tup.into_iter()) {
                    child_scope.add(name.to_string(), v, false);
                }
            },
            (1, v) => child_scope.add(names[0].to_string(), v, false),
            _ => return str_exn(INV_DEF),
        };

        if !filter_out_for_loop_iter(ife, &mut child_scope) {
            match eval(body, &mut child_scope) {
                Vl(_) => (),
                e => return e,
            }
        }
    }
    Vl(Values::Unit)
}

/// Evaluates a for loop over a map
fn eval_for_map(map: HashMap<String, Values>, names: &Vec<String>, 
    ife: Option<&Ast>, body: &Ast, scope: &mut impl Environment) -> Res 
{
    for (key, val) in map.into_iter() {
        let mut child_scope = ScopeProxy::new(scope);
        match names.len() {
            2 => {
                child_scope.add(names[0].to_string(), 
                    Values::Str(key), false);
                child_scope.add(names[1].to_string(), 
                    val, false);
            },
            1 => child_scope.add(names[0].to_string(), 
                Values::Tuple(
                    Box::new(vec![Values::Str(key), val])), 
                false),
            _ => return str_exn(INV_DEF),
        };

        if !filter_out_for_loop_iter(ife, &mut child_scope) {
            match eval(body, &mut child_scope) {
                Vl(_) => (),
                e => return e,
            }
        }
    }
    Vl(Values::Unit)
}