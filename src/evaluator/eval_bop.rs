use super::eval;
use super::eval_class::Class;
use super::eval_class::Member;
use super::eval_functions::apply_function;
use super::exn::*;
use super::iter_over;
use super::vals::eval_args;
use super::vals::to_booly;
use super::vals::CallingContext;
use crate::evaluator::Res;
use crate::evaluator::Values;
use crate::evaluator::{Environment, ImmuScope};
use crate::parser::Ast;
use crate::parser::Op;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use Res::Vl;

/// Evaluates `left op right`
/// If `op` is a short circuit operator, only evaluates `left` if the short circuit path is taken
/// Otherwise evaluates both `left` and `right` and then performs the bop between the two values
pub fn eval_bop(left: &Ast, op: &Op, right: &Ast, scope: &mut impl Environment) -> Res {
    match op {
        &Op::Dot => return perform_dot_op(left, right, scope),
        &Op::Assign => return perform_assign_op(left, right, scope),
        &Op::Update => return perform_update_op(left, right, scope),
        &Op::Index if matches!(left, Ast::Name(_)) => {
            return perform_name_index_op(left, right, scope)
        }
        &Op::As => return perform_cast(left, right, scope),
        &Op::Is => return perform_type_check(left, right, scope),
        _ => (),
    };
    match (super::eval_keep_all(left, scope), op) {
        (x, Op::Lor) if to_booly(&x) == Ok(true) => Vl(Values::Bool(true)),
        (x, Op::Land) if to_booly(&x) == Ok(false) => Vl(Values::Bool(false)),
        (Vl(v), op) => eval_bop_vals(v, op, eval(right, scope), scope),
        (e, _) => e,
    }
}

fn eval_bop_vals(left: Values, op: &Op, right: Res, scope: &mut impl Environment) -> Res {
    match (right, op) {
        (Vl(right), Op::Index) => perform_index_op(&left, right),
        (right, _) if matches!(left, Values::WeakRef(..)) => {
            if let Values::WeakRef(ptr, _) = left {
                eval_bop_vals(ptr.upgrade().unwrap().borrow().clone(), op, right, scope)
            } else {
                panic!("matches! failed")
            }
        }
        (Vl(right), op @ Op::Eq) | (Vl(right), op @ Op::Neq) => perform_eq_test(&left, op, &right),
        (Vl(right), Op::Range) => Vl(Values::Range(Box::new(left), Box::new(right))),
        (Vl(right), Op::Concat) => perform_concat(left, right),
        (Vl(right), op) => perform_bop(left, op, right),
        (e, _) => e,
    }
}

/// Performs the dot operator `.` on `left.right`
/// `left` can be a name, reference, named reference, or any other value
fn perform_dot_op(left: &Ast, right: &Ast, scope: &mut impl Environment) -> Res {
    if let Ast::Name(x) = left {
        lookup_on_name(x, right, scope)
    } else {
        match super::eval_keep_all(left, scope) {
            Vl(v) => perform_dot_lookup(v, right, scope, true), //here
            e => return e,
        }
    }
}

/// Performs a lookup for `right` on the value of `name` bound in `scope`
fn lookup_on_name(name: &String, right: &Ast, scope: &mut impl Environment) -> Res {
    match super::name_lookup(name, scope) {
        Some((v, nm_mut)) => {
            match &*v.borrow() {
                Values::Ref(ptr, mt) => {
                    return perform_dot_lookup(Values::Ref(ptr.clone(), *mt), right, scope, nm_mut)
                }
                Values::WeakRef(data, mu) => {
                    return perform_dot_lookup(
                        Values::Ref(data.upgrade().unwrap(), *mu),
                        right,
                        scope,
                        nm_mut,
                    )
                }
                _ => (),
            };
            let rf = Values::Ref(v.clone(), nm_mut);
            perform_dot_lookup(rf, right, scope, nm_mut)
        }
        None => return ukn_name(name),
    }
}

/// Looks up `right` on the value `val`
fn perform_dot_lookup(
    val: Values,
    right: &Ast,
    scope: &mut impl Environment,
    val_mut: bool,
) -> Res {
    match right {
        Ast::FnApply(name, args) => {
            if let Ast::Name(name) = &**name {
                do_if_some(
                    lookup(&val, name, scope, val_mut),
                    |func, _| -> Res {
                        let mut params = vec![val];
                        eval_args(args, scope, |val| -> Option<Res> {
                            params.push(val);
                            None
                        });
                        apply_function(&*func.borrow(), params, false, false)
                    },
                    name,
                )
            } else {
                panic!("Unknown fn apply in dot op {:?}", name)
            }
        }
        Ast::Name(name) => do_if_some(
            lookup(&val, name, scope, val_mut),
            |v, is_var| -> Res {
                match &*v.borrow() {
                    f @ Values::Func(..) | f @ Values::RustFunc(..) => {
                        apply_function(f, vec![val], true, true)
                    }
                    Values::Ref(ptr, _)
                        if matches!(&*ptr.borrow(), Values::Func(..))
                            || matches!(&*ptr.borrow(), Values::RustFunc(..)) =>
                    {
                        apply_function(&*ptr.borrow(), vec![val], true, true)
                    }
                    Values::Ref(data, mut_ref) => Vl(Values::Ref(data.clone(), is_var && *mut_ref)),
                    _ => Vl(Values::WeakRef(Rc::downgrade(&v), is_var)),
                }
            },
            name,
        ),
        x => panic!("Dot precedence invalid. Got {:?}", x),
    }
}

/// Looks up `name` in the context of `val`
fn lookup(
    val: &Values,
    name: &String,
    scope: &mut impl Environment,
    val_mut: bool,
) -> Result<(Rc<RefCell<Values>>, bool), String> {
    let class_func_name = get_name_w_type(val, name);
    if let Values::Ref(ptr, is_var) = &val {
        return lookup(&*ptr.borrow(), name, scope, val_mut && *is_var);
    } else if let Values::WeakRef(ptr, is_var) = &val {
        return lookup(
            &*ptr.upgrade().unwrap().borrow(),
            name,
            scope,
            val_mut && *is_var,
        );
    }
    if let Values::Object(ptr, ctx) = &val {
        if let Ok(v) = class_lookup(&ptr.borrow(), *ctx, name, val_mut, scope) {
            return Ok(v);
        }
    }
    if let Some((ptr, is_var)) = super::name_lookup(&class_func_name, scope) {
        Ok((ptr, is_var && val_mut))
    } else if let Some((ptr, is_var)) = super::name_lookup(&name, scope) {
        Ok((ptr, is_var && val_mut))
    } else {
        Err(format!("Could not find name: {}", name))
    }
}

/// Looks up `name` object `class`
fn class_lookup(
    class: &Class,
    mut ctx: CallingContext,
    name: &String,
    val_mut: bool,
    scope: &impl Environment,
) -> Result<(Rc<RefCell<Values>>, bool), String> {
    let Class {
        name: c_name,
        members,
        ..
    } = class;
    ctx = calling_context_if_self(ctx, scope, class);
    if let Some(Member {
        val,
        is_var,
        is_pub,
    }) = members.get(name)
    {
        if !is_pub && ctx == CallingContext::Public {
            Err(format!(
                "Attempt to read private value {} from public context",
                name
            ))
        } else {
            let is_mut = *is_var && val_mut || ctx == CallingContext::Constructor;
            Ok((val.clone(), is_mut))
        }
    } else {
        Err(format!("Unable to find {} in class {}", name, c_name))
    }
}

/// If the name self is in scope with class name equal to `class.name` then
/// returns the calling context to be `SelfCall`
/// If `class` contains the name of `self` in the friends list, also returns
/// `SelfCall`
/// otherwise returns `ctx`
fn calling_context_if_self(
    ctx: CallingContext,
    scope: &impl Environment,
    class: &Class,
) -> CallingContext {
    if let Some((ptr, _)) = scope.find("self") {
        if let Values::Object(ptr, _) = &*ptr.borrow() {
            let Class { name, .. } = &*ptr.borrow();
            if name.eq(&class.name) && ctx == CallingContext::Public {
                return CallingContext::SelfCall;
            } else if class
                .friends
                .iter()
                .find(|e| -> bool { (*e).eq(name) })
                .is_some()
            {
                return CallingContext::SelfCall;
            }
        }
    }
    ctx
}

/// Gets the qualified type name of `name` with `ctx_val` as a lookup context
fn get_name_w_type(ctx_val: &Values, name: &String) -> String {
    let class_name = &super::std_sapl::type_of(ctx_val)[..];
    let class_name_no_details = if class_name.find(|c| c == '_') != None {
        &class_name[0..class_name.find(|c| c == '_').unwrap()]
    } else {
        class_name
    };
    format!("{}::{}", class_name_no_details, name)
}

/// Executes `func` if the lookup was successful
/// `fn_name` - the name of the function looked up (for debugging purposes)
/// Allows borrows the looked-up value immutably
fn do_if_some<T, F>(maybe: Result<(Rc<RefCell<T>>, bool), String>, func: F, fn_name: &String) -> Res
where
    F: FnOnce(Rc<RefCell<T>>, bool) -> Res,
{
    match maybe {
        Ok((t, is_var)) => func(t, is_var),
        Err(msg) => str_exn(&format!(
            "Error looking up bound name: '{}' in function {}",
            msg, fn_name
        )),
    }
}

/// Performs the assignment `left = right`.
/// `left` must be a name
fn perform_assign_op(left: &Ast, right: &Ast, scope: &mut impl Environment) -> Res {
    let rt = match eval(right, scope) {
        Vl(v) => v,
        e => return e,
    };

    if let Ast::Name(x) = left {
        match scope.update(&x[..], rt) {
            true => Vl(Values::Unit),
            false => str_exn(&format!("{}: {}", IMMU_ERR, x)),
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
fn assign_to_dot(
    dot_left: &Ast,
    dot_right: &Ast,
    set_val: Values,
    scope: &mut impl Environment,
) -> Res {
    if let Ast::Name(name) = dot_right {
        let mu = is_dot_mut(dot_left, scope);
        match super::eval_keep_all(dot_left, scope) {
            Vl(v) => {
                if let Ok((ptr, true)) = lookup(&v, &name, scope, mu) {
                    *ptr.borrow_mut() = set_val;
                    Vl(Values::Unit)
                } else {
                    str_exn(&format!(
                        "{} is either an invalid member of {:?} or immutable",
                        name, v
                    ))
                }
            }
            e => return e,
        }
    } else {
        str_exn(&format!(
            "Expected a name following the dot operator. Got {:?}",
            dot_right
        ))
    }
}

/// Determines if entire series of names that make up LHS are mutable
fn is_dot_mut(left: &Ast, scope: &impl Environment) -> bool {
    let mut ast = left;
    while let Ast::Bop(left, _, _) = ast {
        if let Ast::Name(x) = &**left {
            if let Some((_, false)) = scope.find(x) {
                return false;
            }
        }
        ast = &**left;
    }
    if let Ast::Name(x) = ast {
        if let Some((_, false)) = scope.find(x) {
            return false;
        }
    }
    true
}

/// Updates a reference `left <- right`
fn perform_update_op(left: &Ast, right: &Ast, scope: &mut impl Environment) -> Res {
    let rt = match eval(right, scope) {
        Vl(v) => v,
        e => return e,
    };

    if let Ast::Name(x) = left {
        match super::name_lookup(&x, scope) {
            Some((ptr, _)) => {
                if let Values::Ref(rf, mtble) = &*ptr.borrow() {
                    update_ref(rf.clone(), *mtble, rt)
                } else {
                    str_exn(NREF)
                }
            }
            None => ukn_name(x),
        }
    } else {
        match eval(left, scope) {
            Vl(Values::Ref(rf, mtble)) => update_ref(rf, mtble, rt),
            _ => str_exn(NREF),
        }
    }
}

/// Updates a reference by repeatedly applying <-
fn update_ref(mut reference: Rc<RefCell<Values>>, is_mut: bool, val: Values) -> Res {
    if !is_mut {
        return str_exn(&format!(
            "{} when updating ref {:?}",
            IMMU_ERR,
            &*reference.borrow()
        ));
    }
    while let Values::Ref(ptr, true) = &*reference.clone().borrow() {
        reference = ptr.clone();
    }
    match &mut *reference.borrow_mut() {
        Values::Ref(_, false) => str_exn(&format!(
            "{} when updating ref {:?}",
            IMMU_ERR,
            &*reference.borrow()
        )),
        ptr => {
            *ptr = val;
            Vl(Values::Unit)
        }
    }
}

/// Performs the concatenation operator `left @ right`
fn perform_concat(left: Values, right: Values) -> Res {
    match (left, right) {
        (Values::Array(mut x), y) => {
            (*x).push(y);
            Vl(Values::Array(x))
        }
        (Values::Map(mut x), Values::Tuple(mut y)) if y.len() == 2 => {
            let first = y.swap_remove(0);
            if let (key, val) = (first, y.pop().unwrap()) {
                x.insert(key, val);
                Vl(Values::Map(x))
            } else {
                inv_arg("map @", Some("Can only concatenate maps with tuples"))
            }
        }
        (Values::Map(x), Values::Array(y)) => map_concat_array(x, *y),
        (Values::Map(mut x), Values::Map(y)) => {
            for val in y.into_iter() {
                let (k, v) = val;
                x.insert(k, v);
            }
            Vl(Values::Map(x))
        }
        (l, r) => bad_op(&l, Some(&r), Op::Concat),
    }
}

/// Inserts key-value pairs of `array` into `map`
fn map_concat_array(mut map: HashMap<Values, Values>, array: Vec<Values>) -> Res {
    for elem in array.into_iter() {
        if let Values::Tuple(mut pair) = elem {
            if pair.len() == 2 {
                let first = pair.swap_remove(0);
                if let (key, val) = (first, pair.pop().unwrap()) {
                    map.insert(key, val);
                } else {
                    return inv_arg(
                        "map @",
                        Some("Can only concatenate maps with list of k/v pairs"),
                    );
                }
            } else {
                return inv_arg(
                    "map @",
                    Some("Assoc list to concat with map must contain 2 tuples"),
                );
            }
        } else {
            return inv_arg(
                "map @",
                Some("Must concatenate map with a list of k/v pairs"),
            );
        }
    }
    Vl(Values::Map(map))
}

/// Indexes `left[right]` where `left` is a name
fn perform_name_index_op(left: &Ast, right: &Ast, scope: &mut impl Environment) -> Res {
    if let Ast::Name(name) = left {
        match (
            super::name_lookup(name, scope),
            eval(right, &mut ImmuScope::from(scope)),
        ) {
            (Some((val, _)), Vl(rt)) => perform_index_op(&*val.borrow(), rt),
            (None, _) => ukn_name(name),
            (_, e) => e,
        }
    } else {
        panic!("Precondition broken perform_name_index_op")
    }
}

/// Indexes `left`[`right`]
/// `left` can be a reference
fn perform_index_op(left: &Values, right: Values) -> Res {
    match (left, right) {
        (Values::Array(x), v) => index_array(x, v),
        (Values::Map(x), key) => match x.get(&key) {
            Some(member) => Vl(member.clone()),
            _ => ukn_name(&format!("{:?}", &key)),
        },
        (Values::Ref(rf, _), idx) => perform_index_op(&*rf.borrow(), idx),
        (Values::WeakRef(rf, _), idx) => perform_index_op(&*rf.upgrade().unwrap().borrow(), idx),
        (Values::Str(string), v) => index_string(string, v),
        (obj @ Values::Object(..), idx) => index_obj(obj, idx),
        (x, idx) => inv_arg(
            "index",
            Some(&format!(
                "Expected an indexable expression. Got: {:?}[{:?}]",
                x, idx
            )),
        ),
    }
}

fn index_array(array: &Vec<Values>, indexer: Values) -> Res {
    match indexer {
        Values::Int(idx) => {
            if idx < array.len() as i32 && idx >= 0 {
                Vl(array[idx as usize].clone())
            } else {
                str_exn(&format!(
                    "{}: {} is out of array bounds {}..{}",
                    IDX_BNDS,
                    idx,
                    0,
                    array.len()
                ))
            }
        }
        Values::Range(min, max) => do_if_valid_range(&min, &max, &|min, max| -> Res {
            range_of_array(array, min, max)
        }),
        x => inv_arg("Array index.", Some(&format!("{:?}", x))),
    }
}

fn index_obj(val: &Values, index: Values) -> Res {
    if let Values::Object(class, cc) = val {
        if let Some(Member { val, .. }) = class.borrow().members.get("__index__") {
            if let func @ Values::RustFunc(..) = &*val.borrow() {
                return super::eval_functions::apply_function(
                    &func,
                    vec![Values::Object(class.clone(), *cc), index],
                    false,
                    false,
                );
            }
        }
    }
    str_exn(&format!("Object {:?} has no function __index__ ", val))
}

fn index_string(string: &String, indexer: Values) -> Res {
    match indexer {
        Values::Int(idx) => {
            let bytes = string.as_bytes().to_vec();
            if idx < bytes.len() as i32 && idx >= 0 {
                Vl(Values::Str(bytes[idx as usize].to_string()))
            } else {
                str_exn(&format!(
                    "Index {} out of bounds of {}..{} for string",
                    idx,
                    0,
                    bytes.len()
                ))
            }
        }
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
        str_exn(&format!(
            "{} when taking the range {}..{} of array",
            IDX_BNDS, start, end
        ))
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
        str_exn(&format!(
            "{} when taking the range {}..{} of array",
            IDX_BNDS, start, end
        ))
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
        (Values::Int(x), Op::Exp, Values::Int(y)) if y >= 0 => {
            Vl(Values::Int(i32::pow(x, y as u32)))
        }
        (Values::Int(x), Op::Exp, Values::Int(y)) if y < 0 => {
            Vl(Values::Float(f64::powf(x.into(), y.into())))
        }
        (Values::Int(x), Op::Mod, Values::Int(y)) => Vl(Values::Int(x % y)),
        (Values::Int(x), Op::Lt, Values::Int(y)) => Vl(Values::Bool(x < y)),
        (Values::Int(x), Op::Gt, Values::Int(y)) => Vl(Values::Bool(x > y)),
        (Values::Int(x), Op::Leq, Values::Int(y)) => Vl(Values::Bool(x <= y)),
        (Values::Int(x), Op::Geq, Values::Int(y)) => Vl(Values::Bool(x >= y)),

        (Values::Float(x), Op::Plus, Values::Float(y)) => Vl(Values::Float(x + y)),
        (Values::Float(x), Op::Sub, Values::Float(y)) => Vl(Values::Float(x - y)),
        (Values::Float(x), Op::Mult, Values::Float(y)) => Vl(Values::Float(x * y)),
        (Values::Float(x), Op::Div, Values::Float(y)) => Vl(Values::Float(x / y)),
        (Values::Float(x), Op::Exp, Values::Float(y)) => Vl(Values::Float(f64::powf(x, y))),
        (Values::Float(x), Op::Lt, Values::Float(y)) => Vl(Values::Bool(x < y)),
        (Values::Float(x), Op::Gt, Values::Float(y)) => Vl(Values::Bool(x > y)),
        (Values::Float(x), Op::Leq, Values::Float(y)) => Vl(Values::Bool(x <= y)),
        (Values::Float(x), Op::Geq, Values::Float(y)) => Vl(Values::Bool(x >= y)),

        (Values::Str(x), Op::Plus, Values::Str(y)) => Vl(Values::Str(x + &y)),
        (Values::Str(x), Op::Lt, Values::Str(y)) => Vl(Values::Bool(x < y)),
        (Values::Str(x), Op::Gt, Values::Str(y)) => Vl(Values::Bool(x > y)),
        (Values::Str(x), Op::Geq, Values::Str(y)) => Vl(Values::Bool(x >= y)),
        (Values::Str(x), Op::Leq, Values::Str(y)) => Vl(Values::Bool(x <= y)),

        (x, Op::Lor, y) => match (to_booly(&Vl(x)), to_booly(&Vl(y))) {
            (Ok(x), Ok(y)) => Vl(Values::Bool(x || y)),
            _ => inv_arg("Op ||", None),
        },
        (x, Op::Land, y) => match (to_booly(&Vl(x)), to_booly(&Vl(y))) {
            (Ok(x), Ok(y)) => Vl(Values::Bool(x && y)),
            _ => inv_arg("Op &&", None),
        },

        (val, Op::Pipeline, f) => eval_pipeline(val, f),

        (Values::Array(mut x), Op::Plus, Values::Array(y)) => {
            for val in y.into_iter() {
                x.push(val)
            }
            Vl(Values::Array(x))
        }
        (x, op, y) => bad_op(&x, Some(&y), *op),
    }
}

/// Determines if `left op right` where `op` is an equality operator
fn perform_eq_test(left: &Values, op: &Op, right: &Values) -> Res {
    match (left, op, right) {
        (Values::Int(x), Op::Eq, Values::Int(y)) => Vl(Values::Bool(x == y)),
        (Values::Bool(x), Op::Eq, Values::Bool(y)) => Vl(Values::Bool(x == y)),
        (Values::Str(x), Op::Eq, Values::Str(y)) => Vl(Values::Bool(x.eq(y))),
        (Values::Array(x), Op::Eq, Values::Array(y)) => Vl(Values::Bool(x == y)),
        (Values::Map(x), Op::Eq, Values::Map(y)) => Vl(Values::Bool(x == y)),
        (Values::Tuple(x), Op::Eq, Values::Tuple(y)) => Vl(Values::Bool(x == y)),
        (Values::Unit, Op::Eq, Values::Unit) => Vl(Values::Bool(true)),
        (Values::Object(a, _), Op::Eq, Values::Object(b, _)) => Vl(Values::Bool(a == b)),
        (
            Values::Func(params1, body1, scope1, pce1),
            Op::Eq,
            Values::Func(params2, body2, scope2, pce2),
        ) => Vl(Values::Bool(
            params1 == params2 && body1 == body2 && scope1 == scope2 && pce1 == pce2,
        )),
        (Values::Ref(ptr, _), op, right) => perform_eq_test(&*ptr.borrow(), op, right),
        (left, op, Values::Ref(ptr, _)) => perform_eq_test(left, op, &*ptr.borrow()),
        (Values::WeakRef(ptr, _), op, right) => {
            perform_eq_test(&*ptr.upgrade().unwrap().borrow(), op, right)
        }
        (left, op, Values::WeakRef(ptr, _)) => {
            perform_eq_test(left, op, &*ptr.upgrade().unwrap().borrow())
        }
        (_, Op::Eq, _) => Vl(Values::Bool(false)),
        (x, Op::Neq, y) => match perform_eq_test(x, &Op::Eq, y) {
            Vl(Values::Bool(x)) => Vl(Values::Bool(!x)),
            e => e,
        },
        (left, op, right) => bad_op(&left, Some(&right), *op),
    }
}

/// Promotes the arguments `a` and `b` to the same type if they are mismatched
/// String and anything will become two strings (for op +)
/// Float and Int will become two Floats
/// Everything else remains the same
fn promote_args(a: Values, b: Values, op: &Op) -> (Values, Values) {
    match (a, b, op) {
        (Values::Float(x), Values::Int(y), _) => (Values::Float(x), Values::Float(y.into())),
        (Values::Int(x), Values::Float(y), _) => (Values::Float(x.into()), Values::Float(y)),
        (Values::Str(x), Values::Int(y), Op::Plus) => (Values::Str(x), Values::Str(y.to_string())),
        (Values::Str(x), Values::Float(y), Op::Plus) => {
            (Values::Str(x), Values::Str(y.to_string()))
        }
        (Values::Int(y), Values::Str(x), Op::Plus) => (Values::Str(y.to_string()), Values::Str(x)),
        (Values::Float(y), Values::Str(x), Op::Plus) => {
            (Values::Str(y.to_string()), Values::Str(x))
        }
        (v1, v2, _) => (v1, v2),
    }
}

/// Evaluates the pipeline argument `left |> func`
fn eval_pipeline(left: Values, func: Values) -> Res {
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
        inv_arg(
            "as",
            Some("Invalid type conversion. Expecting a type name as the right param"),
        )
    }
}

/// Checks if the value of `left` is the type specefied as a name in `is_type`
fn perform_type_check(left: &Ast, is_type: &Ast, scope: &mut impl Environment) -> Res {
    if let Ast::Name(is_type) = is_type {
        match (super::eval(left, scope), &is_type[..]) {
            (Vl(x), typ) if super::std_sapl::type_of(&x).eq(typ) => Vl(Values::Bool(true)),
            (Vl(Values::Tuple(_)), "tuple") => Vl(Values::Bool(true)),
            (Vl(Values::Unit), "none") => Vl(Values::Bool(true)),
            (Vl(x), "some") if x != Values::Unit => Vl(Values::Bool(true)),
            (Vl(Values::Int(_)), "number") | (Vl(Values::Float(_)), "number") => {
                Vl(Values::Bool(true))
            }
            (Vl(Values::Object(obj, _)), typ) => {
                let Class { parents, .. } = &*obj.borrow();
                if typ == "object" || parents.iter().find(|e| *e == typ).is_some() {
                    Vl(Values::Bool(true))
                } else {
                    Vl(Values::Bool(false))
                }
            }
            (Vl(_), _) => Vl(Values::Bool(false)),
            (e, _) => e,
        }
    } else {
        inv_arg(
            "is",
            Some(&format!("Expected a type name. Got {:?}", is_type)),
        )
    }
}
