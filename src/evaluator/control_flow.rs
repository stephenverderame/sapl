use super::environment::*;
use super::eval;
use super::eval_class::*;
use super::exn::*;
use super::vals::to_booly;
use crate::evaluator::Res;
use crate::evaluator::Values;
use crate::parser::Ast;
use Res::Bad;
use Res::Vl;

/// An iterator over a sapl type
struct SaplIter<'a> {
    pub it: &'a Values,
    counter: usize,
    keys: Option<std::collections::hash_map::Iter<'a, Values, Values>>,
}

impl<'a> std::iter::Iterator for SaplIter<'a> {
    type Item = Values;

    fn next(&mut self) -> Option<Values> {
        let next = match self.it {
            Values::Str(s) if self.counter < s.len() => Some(Values::Str(
                String::from_utf8(vec![s.as_bytes()[self.counter].clone()]).unwrap(),
            )),
            Values::Array(arr) if self.counter < arr.len() => Some(arr[self.counter].clone()),
            Values::Object(class, cc) => {
                let Class { members, .. } = &*class.borrow();
                if let Some(Member { val, .. }) = members.get("__next__") {
                    if let func @ Values::RustFunc(..) = &*val.borrow() {
                        match super::eval_functions::apply_function(
                            func,
                            vec![Values::Object(class.clone(), *cc)],
                            false,
                            false,
                        ) {
                            Vl(v) if v == Values::Unit => None,
                            Vl(v) => Some(v),
                            e => panic!("Object iterator returned {:?}", e),
                        }
                    } else {
                        None
                    }
                } else {
                    panic!("Object is not iterable")
                }
            }
            Values::Range(st, en) => {
                if let (Values::Int(s), Values::Int(e)) = (&**st, &**en) {
                    let v = self.counter as i32;
                    let s = *s;
                    let e = *e;
                    if v < (e - s).abs() {
                        if s > e {
                            Some(Values::Int(s - 1 - v))
                        }
                        // larger num is exclusive
                        else {
                            Some(Values::Int(s + v))
                        }
                    } else {
                        None
                    }
                } else {
                    panic!("Cannot iterate invalid range")
                }
            }
            Values::Map(map) => {
                if self.keys.is_none() {
                    self.keys = Some(map.iter());
                }
                let iter = self.keys.as_mut().unwrap();
                if let Some((k, v)) = iter.next() {
                    Some(Values::Tuple(Box::new(vec![k.clone(), v.clone()])))
                } else {
                    None
                }
            }
            Values::Str(..) | Values::Array(..) => None,
            x => panic!("Cannot iterate {:?}", x),
        };
        self.counter += 1;
        next
    }
}

impl<'a> SaplIter<'a> {
    fn new(val: &'a Values) -> SaplIter<'a> {
        SaplIter {
            it: val,
            counter: 0,
            keys: None,
        }
    }
}

/// Evaluates a try block
pub fn eval_try(
    body: &Ast,
    catch_var: &str,
    catch_body: &Ast,
    scope: &mut impl Environment,
) -> Res {
    scope.new_scope();
    match eval(body, scope) {
        Res::Exn(exn) => {
            scope.pop_scope();
            let mut child = ScopeProxy::new(scope);
            child.add(catch_var.to_owned(), exn, false);
            eval(catch_body, &mut child)
        }
        e => {
            scope.pop_scope();
            e
        }
    }
}

/// Evaluates a for loop
pub fn eval_for(
    names: &Vec<String>,
    iter: &Ast,
    if_expr: &Option<Box<Ast>>,
    body: &Ast,
    scope: &mut impl Environment,
) -> Res {
    let ife = if_expr.as_ref().map(|ast| &**ast);
    match eval(iter, scope) {
        Vl(iter) => {
            let iter = get_iter_obj(&iter);
            if let Vl(e) = iter {
                eval_for_iter(SaplIter::new(&e), names, ife, body, scope)
            } else {
                iter
            }
        }
        _ => str_exn(&format!("{} in for loop", NON_ITER)),
    }
}

/// Gets the iterator sapl object of `v`
fn get_iter_obj(v: &Values) -> Res {
    match v {
        Values::Object(class, cc) => {
            let Class { members, .. } = &*class.borrow();
            if let Some(Member { val, .. }) = members.get("__iter__") {
                if let func @ Values::RustFunc(..) = &*val.borrow() {
                    match super::eval_functions::apply_function(
                        func,
                        vec![Values::Object(class.clone(), *cc)],
                        false,
                        false,
                    ) {
                        v @ Vl(_) => v,
                        x => panic!("Could not execute iter(): {:?}", x),
                    }
                } else {
                    Vl(Values::Object(class.clone(), *cc))
                }
            } else {
                Vl(Values::Object(class.clone(), *cc))
            }
        }
        Values::Ref(val, _) => get_iter_obj(&*val.borrow()),
        Values::WeakRef(val, _) => get_iter_obj(&*val.upgrade().unwrap().borrow()),
        Values::Map(_) | Values::Range(..) | Values::Array(..) | Values::Str(..) => Vl(v.clone()),
        _ => str_exn(&format!("{} in for loop", NON_ITER)),
    }
}

fn eval_for_iter(
    iter: SaplIter,
    names: &Vec<String>,
    ife: Option<&Ast>,
    body: &Ast,
    scope: &mut impl Environment,
) -> Res {
    for val in iter {
        let mut child_scope = ScopeProxy::new(scope);
        match (names.len(), val) {
            (x, Values::Tuple(tup)) if x == tup.len() => {
                for (name, v) in names.iter().zip(tup.into_iter()) {
                    child_scope.add(name.to_string(), v, false);
                }
            }
            (1, v) => child_scope.add(names[0].to_string(), v, false),
            _ => {
                return str_exn(&format!(
                    "{} in for loop. Expected iteration over an array of tuples or one name",
                    INV_DEF
                ))
            }
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

/// Evaluates a while loop
pub fn eval_while(guard: &Ast, body: &Ast, scope: &mut impl Environment) -> Res {
    loop {
        match eval(guard, scope) {
            Vl(val) => {
                if Values::Bool(true) == val {
                    match eval(body, &mut ScopeProxy::new(scope)) {
                        Vl(_) => (),
                        e => return e,
                    }
                } else {
                    break;
                }
            }
            e => return e,
        }
    }
    Vl(Values::Unit)
}

/// Evaluates an If expression
pub fn eval_if(
    guard: &Ast,
    body: &Ast,
    other: &Option<Box<Ast>>,
    scope: &mut impl Environment,
) -> Res {
    match to_booly(&eval(guard, scope)) {
        Ok(true) => eval(body, &mut ScopeProxy::new(scope)),
        Ok(false) => {
            if let Some(ast) = other {
                eval(&*ast, &mut ScopeProxy::new(scope))
            } else {
                Vl(Values::Unit)
            }
        }
        Err(e) => Bad(e),
    }
}

/// Evaluates a sequence
pub fn eval_seq(children: &Vec<Box<Ast>>, scope: &mut impl Environment) -> Res {
    let mut last_res: Res = Bad("No children in sequence".to_owned());
    for subtree in children {
        match eval(&*subtree, scope) {
            x @ Vl(_) => last_res = x,
            x => return x,
        }
    }
    last_res
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
    } else {
        false
    }
}
