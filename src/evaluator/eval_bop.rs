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
use super::iter_over;
use super::eval_functions::apply_function;
use std::cell::RefCell;
use std::rc::Rc;
use super::eval_class::Member;
use super::eval_class::Class;

/// Evaluates `left op right`
/// If `op` is a short circuit operator, only evaluates `left` if the short circuit path is taken
/// Otherwise evaluates both `left` and `right` and then performs the bop between the two values
pub fn eval_bop(left: &Ast, op: &Op, right: &Ast, scope: &mut impl Environment) -> Res {
    match op {
        &Op::Dot => return perform_dot_op(left, right, scope),
        &Op::Assign => return perform_assign_op(left, right, scope),
        &Op::Update => return perform_update_op(left, right, scope),
        &Op::Index if matches!(left, Ast::Name(_)) => return perform_name_index_op(left, right, scope),
        &Op::As => return perform_cast(left, right, scope),
        &Op::Is => return perform_type_check(left, right, scope),
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
        lookup_on_name(x, right, scope)
    } else {
        match eval(left, scope) {
            Vl(v) => perform_dot_lookup(v, right, scope, true),
            e => return e,
        }
    }
}

/// Performs a lookup for `right` on the value of `name` bound in `scope`
fn lookup_on_name(name: &String, right: &Ast, scope: &mut impl Environment) -> Res {
    match super::name_lookup(name, scope) {
        Some((v, nm_mut)) => {
            match &*v.borrow() {
                Values::Ref(ptr, mt) => 
                    return perform_dot_lookup(Values::Ref(ptr.clone(), *mt), right, scope, nm_mut),
                _ => (),
            };
            let rf = Values::Ref(v.clone(), nm_mut);
            perform_dot_lookup(rf, right, scope, nm_mut)
        },
        None => return ukn_name(name),
    }
}

/// Looks up `right` on the value `val`
fn perform_dot_lookup(val: Values, right: &Ast, scope: &mut impl Environment, val_mut: bool) -> Res {
    match right {
        Ast::FnApply(name, args) => {
            if let Ast::Name(name) = &**name {
                do_if_some(lookup(&val, name, scope, val_mut), |func, _| -> Res {
                    let mut params = vec![val];
                    eval_args(args, scope, |val| -> Option<Res> {
                        params.push(val);
                        None
                    });
                    apply_function(func, params, false, false)
                }, name)
            } else { panic!("Unknown fn apply in dot op {:?}", name) }
        },
        Ast::Name(name) => {
            do_if_some(lookup(&val, name, scope, val_mut), |v, is_var| -> Res {
                match v {
                    f @ Values::Func(..) | f @ Values::RustFunc(..) => 
                        apply_function(f, vec![val], true, true),
                    Values::Ref(ptr, _) if matches!(&*ptr.borrow(), Values::Func(..)) 
                        || matches!(&*ptr.borrow(), Values::RustFunc(..)) => {
                            apply_function(&*ptr.borrow(), vec![val], true, true)
                    },
                    Values::Ref(data, mut_ref) =>
                        Vl(Values::Ref(data.clone(), is_var && *mut_ref)),
                    v => Vl(v.clone()), //clone creates new member vars here
                }
            }, name)
        },
        x => panic!("Dot precedence invalid. Got {:?}", x)
    }
}

/// Looks up `name` in the context of `val`
fn lookup(val: &Values, name: &String, scope: &mut impl Environment, val_mut: bool) 
    -> Option<(Rc<RefCell<Values>>, bool)> 
{
    let class_name = &super::std_sapl::type_of(val)[..];
    let class_name_no_details = if class_name.find(|c| { c == '_' }) != None {
        &class_name[0..class_name.find(|c| { c == '_' }).unwrap()]
    } else {
        class_name
    };
    let class_func_name = format!("{}::{}", class_name_no_details, name);
    if let Values::Ref(ptr, is_var) = &val {
        lookup(&*ptr.borrow(), name, scope, val_mut && *is_var)
    }
    else if let Values::Object(ptr) = &val {
        let Class {name: _, members, ..} = &**ptr;
        if let Some(Member {val, is_var, is_pub: true}) = members.get(name) {
            let is_mut = *is_var && val_mut;
            Some((Rc::new(RefCell::new(Values::Ref(val.clone(), is_mut))), is_mut))
        } else { None }
    } 
    else if let Some((ptr, is_var)) = super::name_lookup(&class_func_name, scope) {    
        Some((ptr, is_var && val_mut))
    } else if let Some((ptr, is_var)) = super::name_lookup(&name, scope) {
        Some((ptr, is_var && val_mut))
    } else {
        None
    }
}

/// Executes `func` if the lookup was successful
/// `fn_name` - the name of the function looked up (for debugging purposes)
/// Allows borrows the looked-up value immutably
fn do_if_some<T, F>(maybe: Option<(Rc<RefCell<T>>, bool)>, func: F, fn_name: &String) -> Res 
    where F : FnOnce(&T, bool) -> Res
{
    if let Some((t, is_var)) = maybe {
        func(&t.borrow(), is_var)
    } else {
        ukn_name(fn_name)
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
    } else if let Ast::Bop(dot_left, Op::Dot, dot_right) = left {
        assign_to_dot(dot_left, dot_right, rt, scope)
    } else {
        str_exn("Unimplemented assignment")
    }
}

/// Performs an assignment where the LHS is an application of the dot operator
/// Evaluates the left branch of the dot op and if the right branch is a name, lookups 
/// the name in the left branch value and sets it if it is mutable
fn assign_to_dot(dot_left: &Ast, dot_right: &Ast, set_val: Values, scope: &mut impl Environment) -> Res {
    if let Ast::Name(name) = dot_right {
        match eval(dot_left, scope) {
            Vl(v) => {
                if let Some((ptr, true)) = lookup(&v, &name, scope, true) {
                    *ptr.borrow_mut() = set_val;
                    Vl(Values::Unit)
                } else {
                    str_exn(&format!("{} is either an invalid member of {:?} or immutable", name, v))
                }
            },
            e => return e,
        }
    } else {
        str_exn(&format!("Expected a name following the dot operator. Got {:?}", dot_right))
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
        match super::name_lookup(&x, scope) {
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

/// Performs the concatenation operator `left @ right`
fn perform_concat(left: Values, right: Values) -> Res {
    match (left, right) {
        (Values::Array(mut x), y) => {
            (*x).push(y);
            Vl(Values::Array(x))
        },
        (Values::Map(mut x), Values::Tuple(mut y)) if y.len() == 2 => {
            let first = y.swap_remove(0);
            if let (Values::Str(key), val) = (first, y.pop().unwrap()) {
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
                let first = pair.swap_remove(0);
                if let (Values::Str(key), val) = (first, pair.pop().unwrap()) {
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


/// Indexes `left[right]` where `left` is a name
fn perform_name_index_op(left: &Ast, right: &Ast, scope: &mut impl Environment) -> Res {
    if let Ast::Name(name) = left {
        match (super::name_lookup(name, scope), eval(right, &mut ImmuScope::from(scope))) {
            (Some((val, _)), Vl(rt)) => perform_index_op(&val.borrow(), rt),
            (None, _) => ukn_name(name),
            (_, e) => e,
        }
    } else { panic!("Precondition broken perform_name_index_op") }
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
        (Values::Str(string), v) => index_string(string, v),
        (x, idx) => inv_arg("index", Some(&format!(
            "Expected an indexable expression. Got: {:?}[{:?}]", x, idx
        ))),
    }
}

fn index_array(array: &Vec<Values>, indexer: Values) -> Res {
    match indexer {
        Values::Int(idx) => {
            if idx < array.len() as i32 && idx >= 0 {
                Vl(array[idx as usize].clone())
            } else {
                str_exn(&format!("{}: {} is out of array bounds {}..{}", IDX_BNDS, idx, 0, array.len()))
            }
        },
        Values::Range(min, max) => do_if_valid_range(&min, &max, &|min, max| -> Res {
            range_of_array(array, min, max)
        }),
        x => inv_arg("Array index.", Some(&format!("{:?}", x)))
    }
}

fn index_string(string: &String, indexer: Values) -> Res {
    match indexer {
        Values::Int(idx) => {
            let bytes = string.as_bytes().to_vec();
            if idx < bytes.len() as i32 && idx >= 0 {
                Vl(Values::Str(bytes[idx as usize].to_string()))
            } else {
                str_exn(&format!("Index {} out of bounds of {}..{} for string", idx, 0, bytes.len()))
            }
        },
        Values::Range(min, max) => do_if_valid_range(&min, &max, &|min, max| -> Res {
            range_of_string(string, min, max)
        }),
        x => inv_arg("String index", Some(&format!("{:?}", x))),

    }
}

fn do_if_valid_range(start: &Values, end: &Values, func: &dyn Fn(i32, i32) -> Res) -> Res {
    if let (Values::Int(min), Values::Int(max)) = (start, end) {
        func(*min, *max)
    } else {
        str_exn(&format!("{}: Index array with invalid range", IDX_BNDS))
    }
}

fn range_of_string(string: &String, start: i32, end: i32) -> Res {
    let array = string.as_bytes().to_vec();
    if start >= 0 && end >= 0 && start <= array.len() as i32 && end <= array.len() as i32 {
        let mut nw = Vec::<u8>::new();
        iter_over(start, end, &mut |idx: i32| -> Option<Res> {
            nw.push(array[idx as usize].clone());
            None
        });
        Vl(Values::Str(String::from_utf8(nw).unwrap()))
    } else {
        str_exn(&format!("{} when taking the range {}..{} of array", IDX_BNDS, start, end))
    }
}

fn range_of_array(array: &Vec<Values>, start: i32, end: i32) -> Res {
    if start >= 0 && end >= 0 && start <= array.len() as i32 && end <= array.len() as i32 {
        let mut nw_array = Vec::<Values>::new();
        iter_over(start, end, &mut |idx: i32| -> Option<Res> {
            nw_array.push(array[idx as usize].clone());
            None
        });
        Vl(Values::Array(Box::new(nw_array)))
    } else {
        str_exn(&format!("{} when taking the range {}..{} of array", IDX_BNDS, start, end))
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
    super::eval_functions::apply_function(&func, vec![left], true, false)
}  

fn perform_cast(left: &Ast, as_type: &Ast, scope: &mut impl Environment) -> Res {
    if let Ast::Name(as_type) = as_type {
        let vl = match eval(left, scope) {
            Vl(v) => v,
            e => return e,
        };
        super::vals::type_conversion(vl, as_type)
    } else {
        inv_arg("as", 
            Some("Invalid type conversion. Expecting a type name as the right param"))
    }

}

/// Checks if the value of `left` is the type specefied as a name in `is_type`
fn perform_type_check(left: &Ast, is_type: &Ast, scope: &mut impl Environment) -> Res {
    if let Ast::Name(is_type) = is_type {
        match (super::eval(left, scope), &is_type[..]) {
            (Vl(x), typ) if super::std_sapl::type_of(&x).eq(typ) => Vl(Values::Bool(true)),
            (Vl(Values::Tuple(_)), "tuple") =>  Vl(Values::Bool(true)),
            (Vl(Values::Unit), "None") =>  Vl(Values::Bool(true)),
            (Vl(x), "Some") if x != Values::Unit =>  Vl(Values::Bool(true)),
            (Vl(_), _) =>  Vl(Values::Bool(false)),
            (e, _) => e,
        }
    } else {
        inv_arg("is", Some(&format!(
            "Expected a type name. Got {:?}", is_type
        )))
    }
}