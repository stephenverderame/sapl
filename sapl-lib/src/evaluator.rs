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

#[derive(PartialEq, Debug, Clone)]
pub enum Values {
    Int(i32),
    Float(f64),
    Str(String),
    Unit,
    Bool(bool),
    Func(Vec<String>, Ast, Rc<RefCell<Scope>>, Option<Ast>),
    Array(Box<Vec<Values>>),
    Tuple(Box<Vec<Values>>),
    Range(Box<Values>, Box<Values>),
    Map(Box<HashMap<String, Values>>),
    Placeholder,
    Ref(Rc<RefCell<Values>>, bool),
    RustFunc(fn(Vec<Values>) -> Res),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Res {
    Vl(Values),
    Exn(Values),
    Bad(String),
    Ret(Values),
}
/*
#[derive(PartialEq, Debug, Clone)]
enum OpMode {
    Cpy, Ref, MRef,
}*/

use Res::Vl;
use Res::Bad;

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
        Ast::FnApply(name, args) => match scope.find(name) {
            Some((val, _)) => eval_fn_app(&val.borrow(), args, &mut ImmuScope::from(scope)),
            _ => ukn_name(name),
        },
        Ast::Array(elems) => eval_arr(elems, scope),
        Ast::Uop(ast, op) => eval_uop(ast, op, scope),
        Ast::Tuple(elems) => eval_tuple(elems, scope),
        Ast::Map(es) => eval_map(es, scope),
        Ast::Try(body, var, catch) => eval_try(&*body, &var[..], &*catch, scope),
        Ast::For(vars, iter, if_expr, body) => eval_for(vars, &*iter, if_expr, &*body, scope),
        Ast::While(guard, body) => eval_while(&*guard, &*body, scope),
        Ast::Placeholder => Vl(Values::Placeholder),
    }
}
/// Evaluates `left op right`
/// If `op` is a short circuit operator, only evaluates `left` if the short circuit path is taken
/// Otherwise evaluates both `left` and `right` and then performs the bop between the two values
fn eval_bop(left: &Ast, op: &Op, right: &Ast, scope: &mut impl Environment) -> Res {
    match op {
        &Op::Dot => return perform_dot_op(left, right, scope),
        &Op::Assign => return perform_assign_op(left, right, scope),
        &Op::Update => return perform_update_op(left, right, scope),
        &Op::Index if matches!(left, Ast::Name(_)) => return perform_name_index_op(left, right, scope),
        _ => (),
    };
    let left = eval(left, scope);
    match (left, op) {
        (x, Op::Lor) if to_booly(&x) == Ok(true) => Vl(Values::Bool(true)),
        (x, Op::Land) if to_booly(&x) == Ok(false) => Vl(Values::Bool(false)),
        (Vl(val), op) => {
            match (eval(right, scope), op) {
                (Vl(right), op @ Op::Eq) | (Vl(right), op @ Op::Neq) =>
                    perform_eq_test(val, op, right),
                (Vl(right), Op::Index) => perform_index_op(&val, right),
                (Vl(right), Op::Range) => Vl(Values::Range(Box::new(val), Box::new(right))),
                (Vl(right), Op::Concat) => perform_concat(val, right),
                (Vl(right), op) => perform_bop(val, op, right),
                (e, _) => e,
            }
        },
        (e, _) => e,
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


/// Performs the dot operator `.` on `left.right`
/// `left` can be a name, reference, named reference, or any other value
fn perform_dot_op(left: &Ast, right: &Ast, scope: &mut impl Environment) -> Res {
    if let Ast::Name(x) = left {
        match scope.find(x) {
            Some((v, nm_mut)) => {
                let mut sub_scope = ImmuScope::from(scope);
                if nm_mut {
                    do_dot_mut_ref_op(&mut v.borrow_mut(), right, scope)
                } else {
                    do_dot_ref_op(&v.borrow(), right, &mut sub_scope)
                }
            },
            None => return ukn_name(x),
        }
    } else {
        match eval(left, scope) {
            Vl(Values::Ref(r, _)) => do_dot_ref_op(&r.borrow(), right, scope),
            Vl(v) => do_dot_mv_op(v, right, scope),
            e => return e,
        }
    }
}


/// Performs the assignment `left = right`.
/// `left` must be a name
fn perform_assign_op(left: &Ast, right: &Ast, scope: &mut impl Environment) -> Res {
    let rt =
    match eval(right, scope) {
        Vl(v) => v,
        e => return e,
    };

    if let Ast::Name(x) = left {
        match scope.update(&x[..], rt) {
            true => Vl(Values::Unit),
            false => str_exn(IMMU_ERR),
        }
    } else {
        str_exn("Unimplemented assignment")
    }
}

/// Updates a reference `left <- right`
fn perform_update_op(left: &Ast, right: &Ast, scope: &mut impl Environment) -> Res {
    let rt =
    match eval(right, scope) {
        Vl(v) => v,
        e => return e,
    };

    if let Ast::Name(x) = left {
        match scope.find(&x[..]) {
            Some((ptr, _)) => {
                if let Values::Ref(rf, mtble) = &*ptr.borrow() {
                    if *mtble {
                        *(rf.borrow_mut()) = rt;
                        Vl(Values::Unit)
                    } else {
                        str_exn(IMMU_ERR)
                    }
                } else {
                    str_exn(NREF)
                }
            },
            None => ukn_name(x),
        }
    } else {
        match eval(left, scope) {
            Vl(Values::Ref(rf, true)) => { 
                *(rf.borrow_mut()) = rt; 
                Vl(Values::Unit) 
            },
            Vl(Values::Ref(_, false)) => 
                str_exn(IMMU_ERR),
            _ => str_exn(NREF),
        }        
    }
}

/// Performs the dot operator on a mutable reference
fn do_dot_mut_ref_op(mut left: &mut Values, right: &Ast, scope: &mut impl Environment) -> Res {
    match (&mut left, right) {
        (Values::Array(x),
            Ast::FnApply(name, args)) if name.eq("push_back") && !args.is_empty() => 
            eval_dot_args(args, scope, |val| -> Option<Res> { 
                (*x).push(val); 
                None 
            }),
        (Values::Array(x),
            Ast::FnApply(name, args)) if name.eq("set") && args.len() == 2 => 
                array_set(&mut *x, args, scope),
        (Values::Array(x),
            Ast::FnApply(name, args)) if name.eq("insert") && args.len() == 2 =>
                array_insert(&mut *x, args, scope),
        (Values::Array(x),
            Ast::FnApply(name, args)) if name.eq("remove") && !args.is_empty() =>
                eval_dot_args(args, scope, |val| {
                    if let Values::Int(v) = val {
                        if v >= 0 && v < x.len() as i32 {
                            x.remove(v as usize);
                        } 
                        None
                    } else {
                        Some(inv_arg("array::remove", None))
                    }
                }),
        (Values::Map(mp),
            Ast::FnApply(name, args)) if name.eq("insert") && !args.is_empty() =>
                map_insert(mp, args, scope),
        (Values::Map(mp),
                Ast::FnApply(name, args)) if name.eq("remove") && !args.is_empty() =>
                    eval_dot_args(args, scope, |val| {
                        if let Values::Str(v) = val {
                            mp.remove(&v);
                            None
                        } else {
                            Some(inv_arg("map::remove", None))
                        }
                    }),
        (Values::Ref(ptr, ptr_mut), rt) => 
            if *ptr_mut {
                do_dot_mut_ref_op(&mut ptr.borrow_mut(), rt, scope)
            } else {
                do_dot_ref_op(&*ptr.borrow(), rt, scope)
            },
        (l, r) => do_dot_ref_op(l, r, scope),
    }
}

fn eval_dot_args<F>(args: &Vec<Box<Ast>>, scope: &mut impl Environment, mut closure: F) -> Res 
    where F : FnMut(Values) -> Option<Res> 
{
    for arg in args {
        match eval(arg, scope) {
            Vl(val) => {
                match closure(val) {
                    Some(ret) => return ret,
                    None => (),
                }
            },
            e => return e,
        }
    }
    Vl(Values::Unit)
}

/// Requires `args` has 2 elements
fn array_set(array: &mut Vec<Values>, args: &Vec<Box<Ast>>, scope: &mut impl Environment) -> Res {
    match (eval(&*args[0], scope), eval(&*args[1], scope)) {
        (Vl(Values::Int(idx)), Vl(v)) => {
            if idx >= 0 && idx < array.len() as i32 {
                array[idx as usize] = v;
                Vl(Values::Unit)
            } else {
                str_exn(IDX_BNDS)
            }
        },
        _ => inv_arg("array::set", None),
    }
}

fn array_insert(array: &mut Vec<Values>, args: &Vec<Box<Ast>>, scope: &mut impl Environment) -> Res {
    match (eval(&*args[0], scope), eval(&*args[1], scope)) {
        (Vl(Values::Int(idx)), Vl(v)) => {
            if idx >= 0 && idx < array.len() as i32 {
                array.insert(idx as usize, v);
                Vl(Values::Unit)
            } else if idx >= array.len() as i32 {
                array.push(v);
                Vl(Values::Unit)
            } else {
                str_exn(IDX_BNDS)
            }
        },
        _ => inv_arg("array::insert", None),
    }
}

fn map_insert(map: &mut HashMap<String, Values>, args: &Vec<Box<Ast>>, 
    scope: &mut impl Environment) -> Res 
{
    if args.len() == 1 {
        match eval(&*args[0], scope) {
            Vl(Values::Tuple(mut x)) if x.len() == 2 => {
                match (x.swap_remove(0), x.pop().unwrap()) {
                    (Values::Str(x), val) => {
                        map.insert(x, val);
                        Vl(Values::Unit)
                    },
                    _ => inv_arg("map::insert",
                        Some("Inserting tuple must be a string value pair")),
                }
            },
            Vl(_) => inv_arg("map::insert", None),
            e => e,
        }
    }
    else if args.len() == 2 {
        match (eval(&*args[0], scope), eval(&*args[1], scope)) {
            (Vl(Values::Str(key)), Vl(val)) => {
                map.insert(key, val);
                Vl(Values::Unit)
            },
            (Vl(_), _) => inv_arg("map::insert", 
                Some("Can only insert kv pairs into a map")),
            (e, _) => e,

        }
    } else {
        inv_arg("map::insert", Some("Must insert a key/value pair!"))
    }
}


/// Performs a dot operation on a value
fn do_dot_ref_op(left: &Values, right: &Ast, scope: &mut impl Environment) 
    -> Res 
{
    match (left, right) {
        (Values::Map(map), Ast::FnApply(fn_name, args)) 
            if fn_name.eq("contains") && !args.is_empty() =>
                map_contains_all(map, args, scope),
        (Values::Array(x), Ast::FnApply(fn_name, args)) 
                if fn_name.eq("contains") && !args.is_empty() =>
                    arr_contains_all(x, args, scope),
        (Values::Map(map), Ast::Name(nm)) if map.contains_key(nm) =>
            Vl(map.get(nm).unwrap().clone()),
        (Values::Range(fst, _),
            Ast::FnApply(name, vec)) if name.eq("fst") && vec.is_empty() =>
                Vl(*fst.clone()),
        (Values::Range(_, snd),
            Ast::FnApply(name, vec)) if name.eq("snd") && vec.is_empty() =>
                Vl(*snd.clone()),
        (Values::Map(map), Ast::FnApply(fn_name, args)) if map.contains_key(fn_name) =>
            eval_fn_app(&map.get(fn_name).unwrap(), args, scope),
        (Values::Ref(ptr, ptr_mut), rt) => 
            if *ptr_mut {
                do_dot_mut_ref_op(&mut ptr.borrow_mut(), rt, scope)
            } else {
                do_dot_ref_op(&*ptr.borrow(), rt, scope)
            },
        (v, Ast::FnApply(fn_name, args)) => {
            if let Some((ptr, _)) = scope.find(&fn_name[..]) {
                if let f @ Values::Func(..) = &*ptr.borrow() {
                    let mut vc = vec![v.clone()];
                    eval_dot_args(args, scope, |val| -> Option<Res> {
                        vc.push(val);
                        None
                    });
                    return apply_function(f, vc, true);
                }
            }
            bad_op(v, None, Op::Dot)
        },
        (l, _) => bad_op(l, None, Op::Dot),
    }
}

/// Performs a dot operation on a moveable value
fn do_dot_mv_op(left: Values, right: &Ast, scope: &mut impl Environment) -> Res {
    match (left, right) {
        (Values::Range(fst, _),
            Ast::FnApply(name, vec)) if name.eq("fst") && vec.is_empty() =>
                Vl(*fst),
        (Values::Range(_, snd),
            Ast::FnApply(name, vec)) if name.eq("snd") && vec.is_empty() =>
                Vl(*snd),
        (mut l, r) => do_dot_mut_ref_op(&mut l, r, scope),
    }
}

/// Performs the concatenation operator `left @ right`
fn perform_concat(left: Values, right: Values) -> Res {
    match (left, right) {
        (Values::Array(mut x), y) => {
            (*x).push(y);
            Vl(Values::Array(x))
        },
        (Values::Map(mut x), Values::Tuple(mut y)) if y.len() == 2 => {
            if let (Values::Str(key), val) = (y.swap_remove(0), y.pop().unwrap()) {
                x.insert(key, val);
                Vl(Values::Map(x))
            } else {
                inv_arg("map @", Some("Can only concatenate maps with tuples"))
            }
        },
        (Values::Map(x), Values::Array(y)) =>
            map_concat_array(*x, *y),
        (Values::Map(mut x), Values::Map(y)) => {
            for val in y.into_iter() {
                let (k, v) = val;
                x.insert(k, v);
            };
            Vl(Values::Map(x))
        },
        (l, r) => bad_op(&l, Some(&r), Op::Concat),
    }
}

/// Inserts key-value pairs of `array` into `map`
fn map_concat_array(mut map: HashMap<String, Values>, 
    array: Vec<Values>) -> Res 
{
    for elem in array.into_iter() {
        if let Values::Tuple(mut pair) = elem {
            if pair.len() == 2 {
                if let (Values::Str(key), val) = (pair.swap_remove(0), pair.pop().unwrap()) {
                    map.insert(key, val);
                } else {
                    return inv_arg("map @", 
                        Some("Can only concatenate maps with list of k/v pairs"));
                }
            } else {
                return inv_arg("map @",
                    Some("Assoc list to concat with map must contain 2 tuples"));
            }
        } else {
            return inv_arg("map @",
                Some("Must concatenate map with a list of k/v pairs"));
        }
    };
    Vl(Values::Map(Box::new(map)))
}

/// Returns a boolean value if `map` contains all values in `args` once
/// `args` is evaluated
fn map_contains_all(map: &HashMap<String, Values>, args: &Vec<Box<Ast>>,
    scope: &mut impl Environment) -> Res
{
    for e in args {
        match eval(&*e, scope) {
            Vl(Values::Str(name)) => if map.get(&name) == None {
                return Vl(Values::Bool(false));
            },
            Vl(_) => return inv_arg("map::contains", None),
            e => return e,
        }
    };
    Vl(Values::Bool(true))
}

/// True if `arr` contains all of the vales from the evaluated asts in `args`
fn arr_contains_all(arr: &Vec<Values>, args: &Vec<Box<Ast>>,
    scope: &mut impl Environment) -> Res
{
    for e in args {
        match eval(&*e, scope) {
            Vl(x) => if arr.iter().find(|bx| {**bx == x}) == None {
                return Vl(Values::Bool(false));
            },
            e => return e,
        }
    };
    Vl(Values::Bool(true))
}

/// Indexes `left[right]` where `left` is a name
fn perform_name_index_op(left: &Ast, right: &Ast, scope: &mut impl Environment) -> Res {
    if let Ast::Name(name) = left {
        match (scope.find(name), eval(right, &mut ImmuScope::from(scope))) {
            (Some((val, _)), Vl(rt)) => perform_index_op(&val.borrow(), rt),
            (None, _) => ukn_name(name),
            (_, e) => e,
        }
    } else { panic!("Precondition violated") }
}

/// Indexes `left`[`right`]
/// `left` can be a reference
fn perform_index_op(left: &Values, right: Values) -> Res {
    match (left, right) {
        (Values::Array(x), v) => index_array(x, v),
        (Values::Map(x), Values::Str(name)) => {
            match x.get(&name) {
                Some(member) => Vl(member.clone()),
                _ => ukn_name(&name),
            }
        },
        (Values::Ref(rf, _), idx) => perform_index_op(&rf.borrow(), idx),
        _ => inv_arg("index", None),
    }
}

fn index_array(array: &Vec<Values>, indexer: Values) -> Res {
    match indexer {
        Values::Int(idx) => {
            if idx < array.len() as i32 && idx >= 0 {
                Vl(array[idx as usize].clone())
            } else {
                str_exn(IDX_BNDS)
            }
        },
        Values::Range(min, max) => {
            if let (Values::Int(min), Values::Int(max)) = (*min, *max) {
                range_of_array(array, min, max)
            } else {
                str_exn(IDX_BNDS)
            }
        },
        _ => inv_arg("Array index", None)
    }
}

fn range_of_array(array: &Vec<Values>, start: i32, end: i32) -> Res {
    if start >= 0 && end >= 0 && start <= array.len() as i32 && end <= array.len() as i32 {
        let start = start as usize;
        let end = end as usize;
        let iter = if start < end { 
            Box::new(start..end) as Box<dyn Iterator<Item = _>>
        } else { 
            Box::new((end + 1.. start + 1).rev())
        };
        let mut nw_array = Vec::<Values>::new();
        for i in iter {
            nw_array.push(array[i].clone());
        }
        Vl(Values::Array(Box::new(nw_array)))
    } else {
        str_exn(IDX_BNDS)
    }
}

/// Evaluates the binary operator `op` with arguments `vleft` and `vright`
/// by first promoting mismatch arguments to the same type and then evaluating them
fn perform_bop(vleft: Values, op: &Op, vright: Values) -> Res {
    let (a, b) = promote_args(vleft, vright, &op);
    match (a, op, b) {
        (Values::Int(x), Op::Plus, Values::Int(y)) => Vl(Values::Int(x + y)),
        (Values::Int(x), Op::Sub, Values::Int(y)) => Vl(Values::Int(x - y)),
        (Values::Int(x), Op::Mult, Values::Int(y)) => Vl(Values::Int(x * y)),
        (_, Op::Div, Values::Int(y)) if y == 0 => str_exn(DIV_Z),
        (_, Op::Div, Values::Float(y)) if y == 0.0 => str_exn(DIV_Z),
        (Values::Int(x), Op::Div, Values::Int(y)) => Vl(Values::Int(x / y)),
        (Values::Int(x), Op::Exp, Values::Int(y)) if y >= 0 => 
            Vl(Values::Int(i32::pow(x, y as u32))),
        (Values::Int(x), Op::Exp, Values::Int(y)) if y < 0 => 
            Vl(Values::Float(f64::powf(x.into(), y.into()))),
        (Values::Int(x), Op::Mod, Values::Int(y)) => Vl(Values::Int(x % y)),
        (Values::Int(x), Op::Lt, Values::Int(y)) => Vl(Values::Bool(x < y)),
        (Values::Int(x), Op::Gt, Values::Int(y)) => Vl(Values::Bool(x > y)),
        (Values::Int(x), Op::Leq, Values::Int(y)) => Vl(Values::Bool(x <= y)),
        (Values::Int(x), Op::Geq, Values::Int(y)) => Vl(Values::Bool(x >= y)),

        (Values::Float(x), Op::Plus, Values::Float(y)) => Vl(Values::Float(x + y)),
        (Values::Float(x), Op::Sub, Values::Float(y)) => Vl(Values::Float(x - y)),
        (Values::Float(x), Op::Mult, Values::Float(y)) => Vl(Values::Float(x * y)),
        (Values::Float(x), Op::Div, Values::Float(y)) => Vl(Values::Float(x / y)),
        (Values::Float(x), Op::Exp, Values::Float(y)) => 
            Vl(Values::Float(f64::powf(x, y))),
        (Values::Float(x), Op::Lt, Values::Float(y)) => Vl(Values::Bool(x < y)),
        (Values::Float(x), Op::Gt, Values::Float(y)) => Vl(Values::Bool(x > y)),
        (Values::Float(x), Op::Leq, Values::Float(y)) => Vl(Values::Bool(x <= y)),
        (Values::Float(x), Op::Geq, Values::Float(y)) => Vl(Values::Bool(x >= y)),

        (Values::Str(x), Op::Plus, Values::Str(y)) => Vl(Values::Str(x + &y)),
        (Values::Str(x), Op::Lt, Values::Str(y)) => Vl(Values::Bool(x < y)),
        (Values::Str(x), Op::Gt, Values::Str(y)) => Vl(Values::Bool(x > y)),
        (Values::Str(x), Op::Geq, Values::Str(y)) => Vl(Values::Bool(x >= y)),
        (Values::Str(x), Op::Leq, Values::Str(y)) => Vl(Values::Bool(x <= y)),

        (x, Op::Lor, y) => {
            match (to_booly(&Vl(x)), to_booly(&Vl(y))) {
                (Ok(x), Ok(y)) => Vl(Values::Bool(x || y)),
                _ => inv_arg("Op ||", None),
            }
        },
        (x, Op::Land, y) => {
            match (to_booly(&Vl(x)), to_booly(&Vl(y))) {
                (Ok(x), Ok(y)) => Vl(Values::Bool(x && y)),
                _ => inv_arg("Op &&", None),
            }
        },

        (val, Op::Pipeline, f) => eval_pipeline(val, f),

        (Values::Array(mut x), Op::Plus, Values::Array(y)) => {
            for val in y.into_iter() {
                x.push(val)
            };
            Vl(Values::Array(x))
        },
        (x, op, y) => bad_op(&x, Some(&y), *op),
    }
}

/// Determines if `left op right` where `op` is an equality operator
fn perform_eq_test(left: Values, op: &Op, right: Values) -> Res {
    match (left, op, right) {
        (Values::Int(x), Op::Eq, Values::Int(y)) => Vl(Values::Bool(x == y)),
        (Values::Bool(x), Op::Eq, Values::Bool(y)) => Vl(Values::Bool(x == y)),
        (Values::Str(x), Op::Eq, Values::Str(y)) => Vl(Values::Bool(x.eq(&y))),
        (Values::Str(x), Op::Neq, Values::Str(y)) => Vl(Values::Bool(!x.eq(&y))),
        (Values::Array(x), Op::Eq, Values::Array(y)) => Vl(Values::Bool(x == y)),
        (Values::Array(x), Op::Neq, Values::Array(y)) => Vl(Values::Bool(x != y)),
        (Values::Unit, Op::Eq, Values::Unit) => Vl(Values::Bool(true)),
        (_, Op::Eq, _) => Vl(Values::Bool(false)),
        (x, Op::Neq, y) if x != y => Vl(Values::Bool(true)),
        (_, Op::Neq, _) => Vl(Values::Bool(false)),
        (left, op, right) => bad_op(&left, Some(&right), *op),
    }
}

/// Promotes the arguments `a` and `b` to the same type if they are mismatched
/// String and anything will become two strings (for op +)
/// Float and Int will become two Floats
/// Everything else remains the same
fn promote_args(a: Values, b: Values, op: &Op) -> (Values, Values) {
    match (a, b, op) {
        (Values::Float(x), Values::Int(y), _) => 
            (Values::Float(x), Values::Float(y.into())),
        (Values::Int(x), Values::Float(y), _) => 
            (Values::Float(x.into()), Values::Float(y)),
        (Values::Str(x), Values::Int(y), Op::Plus) => 
            (Values::Str(x), Values::Str(y.to_string())),
        (Values::Str(x), Values::Float(y), Op::Plus) => 
            (Values::Str(x), Values::Str(y.to_string())),
        (Values::Int(y), Values::Str(x), Op::Plus) => 
            (Values::Str(y.to_string()), Values::Str(x)),
        (Values::Float(y), Values::Str(x), Op::Plus) => 
            (Values::Str(y.to_string()), Values::Str(x)),
        (v1, v2, _) => (v1, v2),
    }
}

/// Evaluates an If expression
fn eval_if(guard: &Ast, body: &Ast, other: &Option<Box<Ast>>, 
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

/// Converts a value `b` into the closest boole equivalent
/// Non-empty strings and arrays, true booleans, non zero ints and floats 
/// and ranges with different first and second elements
/// become true
/// Everything else becomes false
fn to_booly(b: &Res) -> Result<bool, String> {
    match b {
        Vl(Values::Bool(true)) => Ok(true),
        Vl(Values::Int(x)) if *x != 0 => Ok(true),
        Vl(Values::Str(x)) if !x.is_empty() => Ok(true),
        Vl(Values::Float(x)) if x.abs() > 0.0001 => Ok(true),
        Vl(Values::Array(x)) if !x.is_empty() => Ok(true),
        Vl(Values::Map(x)) if !x.is_empty() => Ok(true),
        Vl(Values::Range(a, b)) if a != b => Ok(true),
        Vl(Values::Func(..)) | Vl(Values::Tuple(..)) => Ok(true),        
        Vl(_) => Ok(false),
        Bad(e) => Err(e.to_string()),
       _ => Err("Return/exn value cannot be converted to bool".to_owned()),
    }
}

/// Evaluates a sequence
fn eval_seq(children: &Vec<Box<Ast>>, scope: &mut impl Environment) 
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

/// Evaluates a function definition and adds the function to `scope`
/// Captures names references in `ast` that are also in `scope` by copying them into a new
/// environment
fn eval_func(name: &String, params: &Vec<String>, ast: &Ast, postcondition: &Option<Box<Ast>>,
    scope: &mut impl Environment)
    -> Res
{
    let nw_scope = Rc::new(RefCell::new(Scope::new()));
    let post = if postcondition.is_some() {
        let ptr = postcondition.as_ref().unwrap();
        capture_into_scope(&*ptr, &mut nw_scope.borrow_mut(), scope);
        Some(*ptr.clone())
    } else { None };
    scope.add(name.to_string(), 
        Values::Func(params.clone(), ast.clone(), nw_scope.clone(), post), false);
    capture_into_scope(ast, &mut nw_scope.borrow_mut(), scope);
    Vl(Values::Unit)
}

/// Evaluates a lambda expression
/// Captures names references in `ast` that are also in `scope` by copying them into a new
/// environment
/// Copies "this" into the new environment to allow recursion
fn eval_lambda(params: &Vec<String>, ast: &Ast, scope: &mut impl Environment) 
    -> Res
{
    let nw_scope = Rc::new(RefCell::new(Scope::new()));
    nw_scope.borrow_mut().add("this".to_owned(), 
        Values::Func(params.clone(), ast.clone(), nw_scope.clone(), None), false);
    capture_into_scope(ast, &mut nw_scope.borrow_mut(), scope);
    Vl(Values::Func(params.clone(), ast.clone(), nw_scope, None))
}

/// Searches for names in `ast` and captures them by copying their corresponding value from
/// `old_scope` into `scope`
fn capture_into_scope(ast: &Ast, scope: &mut Scope, old_scope: &impl Environment) {
    match ast {
        Ast::Name(x) => {
            if let Some((val, mtble)) = old_scope.find(x) {
                scope.add(x.to_string(), val.borrow().clone(), mtble)
            }
        },
        Ast::If(guard, body, other) | Ast::For(_, guard, other, body) => {
            capture_into_scope(&*guard, scope, old_scope);
            capture_into_scope(&*body, scope, old_scope);
            if let Some(x) = other {
                capture_into_scope(&*x, scope, old_scope);
            }
        },
        Ast::Seq(children) | Ast::Array(children) | Ast::Tuple(children) => {
            for node in children {
                capture_into_scope(&*node, scope, old_scope);
            }
        },
        Ast::Let(_, ast) | Ast::Uop(ast, _) | Ast::Lambda(_, ast) => 
            capture_into_scope(&*ast, scope, old_scope),
        Ast::Func(.., ast, condition) => {
            capture_into_scope(&*ast, scope, old_scope);
            if let Some(ast) = condition {
                capture_into_scope(&*ast, scope, old_scope);
            }
        },
        Ast::FnApply(name, args) => {
            if let Some((val, mtble)) = old_scope.find(name) {
                scope.add(name.to_string(), val.borrow().clone(), mtble);
            }
            for expr in args {
                capture_into_scope(&*expr, scope, old_scope);
            }
        },
        Ast::While(one, two) | Ast::Bop(one, _, two ) 
        | Ast::Try(one, _, two) => {
            capture_into_scope(one, scope, old_scope);
            capture_into_scope(two, scope, old_scope);
        },
        Ast::Map(children) => {
            for (_, val) in children {
                capture_into_scope(val, scope, old_scope);
            }
        },
        Ast::Placeholder | Ast::VInt(_) | Ast::VStr(_)
        | Ast::VBool(_) | Ast::VFloat(_) => (),

    }
}

/// Evaluates a function application
/// Requires arguments are expressions that do no modify any values in the scope
fn eval_fn_app(func: &Values, args: &Vec<Box<Ast>>, scope: &mut impl Environment)
    -> Res
{
    match func {
        f @ Values::Func(..) => {
            let mut val_args = Vec::<Values>::new();
            let mut arg_scope = ImmuScope::from(scope);
            for expr in args {
                match eval(&*expr, &mut arg_scope) {
                    Vl(v) => val_args.push(v),
                    e => return e,
                }
            }
            apply_function(f, val_args, false)
        },
        Values::RustFunc(func) => {
            let mut params = Vec::<Values>::new();
            eval_dot_args(args, scope, |val| -> Option<Res> {
                params.push(val);
                None
            });
            func(params)
        },
        _ => str_exn(NFUNC),
    }
}

/// Evaluates the pipeline argument `left |> func`
fn eval_pipeline(left: Values, func: Values) -> Res 
{
    apply_function(&func, vec![left], true)
    // TODO Make rust functions work with pipelining by modifying apply_function
}  

/// Applies `args` to the function `func`
/// If `args` contains a placeholder, performs a partial application
/// If `allow_incomplete` is true, assumes that if `args.len()` is less than
/// the amount of parameters, the remaining parameters will be placeholders and a partial application
/// will be performed
fn apply_function(func: &Values, mut args: Vec<Values>, allow_incomplete: bool) 
    -> Res 
{
    if let Values::Func(params, ast, fn_scope, postcondition) = func {
        if allow_incomplete && args.len() < params.len() {
            args.append(&mut vec![Values::Placeholder; params.len() - args.len()]);
        } else if params.len() != args.len() { 
            return Bad(format!("Arg count mismatch formals {} vs args {} and incomplete: {}",
                params.len(), args.len(), allow_incomplete)); 
        }

        let mut sub = fn_scope.borrow().cpy();
        let mut sc = ScopeProxy::from(&mut sub);
        let mut is_partial = false;
        let mut missing_params = Vec::<String>::new();
        for (nm, arg) in params.iter().zip(args.into_iter()) {
            match arg {
                Values::Placeholder => {
                    missing_params.push(nm.to_string());
                    is_partial = true
                },
                v => sc.add(nm.to_string(), v, false),
            }
        }
        if is_partial {
            Vl(Values::Func(missing_params, ast.clone(), 
                Rc::new(RefCell::new(sc.cpy())), postcondition.clone()))
        } else {
            match eval(ast, &mut sc) {
                Res::Ret(v) | Vl(v) => 
                    check_func_post(v, postcondition, &*fn_scope.borrow()),
                e => e,
            }
        }
    } else {
        str_exn(NFUNC)
    }
}

/// Checks the function postcondition
/// Returns `result` if the postcondition is true or the postcondition contains a valid name only
/// Otherwise, returns an exception
/// When evaluating the postcondition, adds to the postcondition expression scope a name which is the type of the result
/// that stores the value of the result
fn check_func_post(result: Values, postcondition: &Option<Ast>, scope: &impl Environment) -> Res {
    match postcondition {
        None => Vl(result),
        Some(ast) => {
            let mut scope = ImmuScope::from(scope);
            match &result {
                Values::Float(_) | Values::Int(_) => {
                    scope.add("number".to_owned(), result.clone(), false);
                },
                _ => (),
            }
            scope.add(type_of(&result), result.clone(), false);
            scope.add("result".to_owned(), result.clone(), false);
            match eval(&ast, &mut scope) {
                Vl(_) if matches!(&ast, &Ast::Name(_)) => Vl(result),
                Vl(Values::Bool(true)) => Vl(result),
                /*e @ Bad(_) => e,
                e @ Res::Exn(_) => e, */
                _ => str_exn(PCE_FAIL),

            }
        },
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
fn eval_map(es: &HashMap<String, Box<Ast>>, scope: &mut impl Environment) -> Res {
    let mut map = HashMap::<String, Values>::new();
    for (k, v) in es {
        match eval(v, scope) {
            Vl(val) => map.insert(k.clone(), val),
            e => return e,
        };
    }
    Vl(Values::Map(Box::new(map)))
}

/// Evaluates a try block
fn eval_try(body: &Ast, catch_var: &str, catch_body: &Ast, scope: &mut impl Environment) -> Res {
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

fn eval_for(names: &Vec<String>, iter: &Ast, if_expr: &Option<Box<Ast>>, 
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

fn eval_for_range(start: Values, end: Values, name: &String, ife: Option<&Ast>,
    body: &Ast, scope: &mut impl Environment) -> Res 
{
    if let (Values::Int(start), Values::Int(end)) = (start, end) {
        let min = std::cmp::min(start, end);
        let max = std::cmp::max(start, end);
        let iter = if min == start { 
            Box::new(min..max) as Box<dyn Iterator<Item = _>>
        } else { 
            Box::new((min + 1 .. max + 1).rev())
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

fn eval_while(guard: &Ast, body: &Ast, scope: &mut impl Environment) -> Res {
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