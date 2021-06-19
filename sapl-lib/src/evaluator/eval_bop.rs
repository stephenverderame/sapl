use super::eval;
use crate::evaluator::Values;
use crate::evaluator::Res;
use Res::Vl;
use super::exn::*;
use crate::parser::Ast;
use super::vals::to_booly;
use std::collections::HashMap;
use crate::parser::Op;
use crate::evaluator::{Environment, ImmuScope};
use super::vals::eval_args;

/// Evaluates `left op right`
/// If `op` is a short circuit operator, only evaluates `left` if the short circuit path is taken
/// Otherwise evaluates both `left` and `right` and then performs the bop between the two values
pub fn eval_bop(left: &Ast, op: &Op, right: &Ast, scope: &mut impl Environment) -> Res {
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
            eval_args(args, scope, |val| -> Option<Res> { 
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
                eval_args(args, scope, |val| {
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
                    eval_args(args, scope, |val| {
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
            super::eval_fn_app(&map.get(fn_name).unwrap(), args, scope),
        (Values::Ref(ptr, ptr_mut), rt) => 
            if *ptr_mut {
                do_dot_mut_ref_op(&mut ptr.borrow_mut(), rt, scope)
            } else {
                do_dot_ref_op(&*ptr.borrow(), rt, scope)
            },
        (v, Ast::FnApply(fn_name, args)) => {
            if let Some((ptr, _)) = scope.find(&fn_name[..]) {
                let f = &*ptr.borrow();
                let mut vc = vec![v.clone()];
                eval_args(args, scope, |val| -> Option<Res> {
                    vc.push(val);
                    None
                });
                return super::eval_functions::apply_function(&f, vc, true);
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

/// Evaluates the pipeline argument `left |> func`
fn eval_pipeline(left: Values, func: Values) -> Res 
{
    super::eval_functions::apply_function(&func, vec![left], true)
}  