use crate::parser::Ast;
use crate::parser::Op;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

mod environment;
use environment::*;

#[derive(PartialEq, Debug, Clone)]
pub enum Values {
    Int(i32),
    Float(f64),
    Str(String),
    Unit,
    Bool(bool),
    Func(Vec<String>, Ast, Rc<RefCell<Scope>>, Option<Ast>),
    Array(Vec<Rc<RefCell<Values>>>),
    Tuple(Vec<Box<Values>>),
    Range(Box<Values>, Box<Values>),
    Map(HashMap<String, Rc<RefCell<Values>>>),
    Placeholder,
    Ref(Rc<RefCell<Values>>, bool),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Res {
    Vl(Values),
    Exn(Values),
    Bad(String),
    Ret(Values),
}

#[derive(PartialEq, Debug, Clone)]
enum OpMode {
    Cpy, Ref, MRef,
}

use Res::Vl;
use Res::Bad;

static UNKNOWN_NAME: &'static str = "Unknown name exception";
static PCE_FAIL: &'static str = "Function post-condition violated";
static INV_UOP: &'static str = "Invalid value or operation";
static IMMU_ERR: &'static str = "Cannot mutate an immutable value";
static NREF: &'static str = "Applying reference operator to non-reference";
static INV_ARG: &'static str = "Invalid argument to function";
static DIV_Z: &'static str = "Divide by zero exception";
static IDX_BNDS: &'static str = "Index out of bounds";
static NFUNC: &'static str = "Not a function";
static INV_DEF: &'static str = "Invalid definition or structured binding";
static NON_ITER: &'static str = "Attempt to iterate over non-iterable";

fn str_exn(msg: &str) -> Res {
    Res::Exn(Values::Str(msg.to_owned()))
}

fn ukn_name(name: &String) -> Res {
    Res::Exn(Values::Str(format!("{}: {}", UNKNOWN_NAME, name)))
}
fn bad_op(left: &Values, right: Option<&Values>, op: Op) -> Res {
    let err_msg = if right == None {
        format!("{:?} {:?} {:?}", left, op, right)
    } else {
        format!("{:?} {:?}", op, left)
    };
    Res::Exn(Values::Str(format!("{}: {}", INV_UOP, err_msg)))
}

fn inv_arg(func: &str, info: Option<&str>) -> Res{
    if info == None {
        Res::Exn(Values::Str(format!("{} {}", INV_ARG, func)))
    } else {
        Res::Exn(Values::Str(format!("{} {} : {}", INV_ARG, func, info.unwrap())))
    }
}


/// Evaluates a SAPL AST
pub fn evaluate(ast: &Ast) -> Res {
    eval(ast, &mut Scope::new())
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
        Ast::FnApply(name, args) if is_rust_func(&name[..]) => 
            eval_rust_func(&name[..], args, scope),
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
                x.push(Rc::new(RefCell::new(val))); 
                None 
            }),
        (Values::Array(x),
            Ast::FnApply(name, args)) if name.eq("get") && args.len() == 1 => 
                eval_dot_args(args, scope, |val| {
                    Some(index_array(x, val, OpMode::MRef))
                }),
        (Values::Map(mp),
            Ast::FnApply(name, args)) if name.eq("insert") && !args.is_empty() =>
                map_insert(mp, args, scope),
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

fn map_insert(map: &mut HashMap<String, Rc<RefCell<Values>>>, args: &Vec<Box<Ast>>, 
    scope: &mut impl Environment) -> Res 
{
    if args.len() == 1 {
        match eval(&*args[0], scope) {
            Vl(Values::Tuple(mut x)) if x.len() == 2 => {
                match (*x.swap_remove(0), *x.pop().unwrap()) {
                    (Values::Str(x), val) => {
                        map.insert(x, Rc::new(RefCell::new(val)));
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
                map.insert(key, Rc::new(RefCell::new(val)));
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
        (Values::Array(x), 
            Ast::FnApply(name, vec)) if vec.is_empty() && name.eq("size") => 
                Vl(Values::Int(x.len() as i32)),
        (Values::Map(map), Ast::FnApply(fn_name, args)) 
            if fn_name.eq("contains") && !args.is_empty() =>
                map_contains_all(map, args, scope),
        (Values::Array(x), Ast::FnApply(fn_name, args)) 
                if fn_name.eq("contains") && !args.is_empty() =>
                    arr_contains_all(x, args, scope),
        (Values::Map(map), Ast::Name(nm)) if map.contains_key(nm) =>
            Vl(map.get(nm).unwrap().borrow().clone()),
        (Values::Range(fst, _),
            Ast::FnApply(name, vec)) if name.eq("fst") && vec.is_empty() =>
                Vl(*fst.clone()),
        (Values::Range(_, snd),
            Ast::FnApply(name, vec)) if name.eq("snd") && vec.is_empty() =>
                Vl(*snd.clone()),
        (Values::Map(map), Ast::FnApply(fn_name, args)) if map.contains_key(fn_name) =>
            eval_fn_app(&map.get(fn_name).unwrap().borrow(), args, scope),
        (Values::Array(x),
            Ast::FnApply(name, args)) if name.eq("get") && args.len() == 1 => 
                eval_dot_args(args, scope, |val| { Some(index_array(x, val, OpMode::Ref)) }),
        (Values::Ref(ptr, ptr_mut), rt) => 
            if *ptr_mut {
                do_dot_mut_ref_op(&mut ptr.borrow_mut(), rt, scope)
            } else {
                do_dot_ref_op(&*ptr.borrow(), rt, scope)
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
            x.push(Rc::new(RefCell::new(y)));
            Vl(Values::Array(x))
        },
        (Values::Map(mut x), Values::Tuple(mut y)) if y.len() == 2 => {
            if let (Values::Str(key), val) = (*y.swap_remove(0), y.pop().unwrap()) {
                x.insert(key, Rc::new(RefCell::new(*val)));
                Vl(Values::Map(x))
            } else {
                inv_arg("map @", Some("Can only concatenate maps with tuples"))
            }
        },
        (Values::Map(x), Values::Array(y)) =>
            map_concat_array(x, y),
        (Values::Map(mut x), Values::Map(y)) => {
            for val in y.into_iter() {
                let (k, v) = val;
                x.insert(k, Rc::new(RefCell::new(v.borrow().clone())));
            };
            Vl(Values::Map(x))
        },
        (l, r) => bad_op(&l, Some(&r), Op::Concat),
    }
}

/// Inserts key-value pairs of `array` into `map`
fn map_concat_array(mut map: HashMap<String, Rc<RefCell<Values>>>, 
    array: Vec<Rc<RefCell<Values>>>) -> Res 
{
    for elem in array.into_iter() {
        if let Values::Tuple(mut pair) = elem.borrow().clone() {
            if pair.len() == 2 {
                if let (Values::Str(key), val) = (*pair.swap_remove(0), pair.pop().unwrap()) {
                    map.insert(key, Rc::new(RefCell::new(*val)));
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
    Vl(Values::Map(map))
}

/// Returns a boolean value if `map` contains all values in `args` once
/// `args` is evaluated
fn map_contains_all(map: &HashMap<String, Rc<RefCell<Values>>>, args: &Vec<Box<Ast>>,
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
fn arr_contains_all(arr: &Vec<Rc<RefCell<Values>>>, args: &Vec<Box<Ast>>,
    scope: &mut impl Environment) -> Res
{
    for e in args {
        match eval(&*e, scope) {
            Vl(x) => if arr.iter().find(|bx| {&*bx.borrow() == &x}) == None {
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
        (Values::Array(x), v) => index_array(x, v, OpMode::Cpy),
        (Values::Map(x), Values::Str(name)) => {
            match x.get(&name) {
                Some(member) => Vl(member.borrow().clone()),
                _ => ukn_name(&name),
            }
        },
        (Values::Ref(rf, _), idx) => perform_index_op(&rf.borrow(), idx),
        _ => inv_arg("index", None),
    }
}

fn index_array(array: &Vec<Rc<RefCell<Values>>>, indexer: Values, mode: OpMode) -> Res {
    match indexer {
        Values::Int(idx) => {
            if idx < array.len() as i32 && idx >= 0 {
                if mode == OpMode::Cpy 
                    { Vl((*(array[idx as usize])).borrow().clone()) }
                else
                    { Vl(Values::Ref(array[idx as usize].clone(), mode == OpMode::MRef)) }
            } else {
                str_exn(IDX_BNDS)
            }
        },
        Values::Range(min, max) => {
            if let (Values::Int(min), Values::Int(max)) = (*min, *max) {
                range_of_array(array, min, max, mode)
            } else {
                str_exn(IDX_BNDS)
            }
        },
        _ => inv_arg("Array index", None)
    }
}

fn range_of_array(array: &Vec<Rc<RefCell<Values>>>, start: i32, end: i32, mode: OpMode) -> Res {
    if start >= 0 && end >= 0 && start <= array.len() as i32 && end <= array.len() as i32 {
        let start = start as usize;
        let end = end as usize;
        let iter = if start < end { 
            Box::new(start..end) as Box<dyn Iterator<Item = _>>
        } else { 
            Box::new((end + 1.. start + 1).rev())
        };
        let mut nw_array = Vec::<Rc<RefCell<Values>>>::new();
        for i in iter {
            let elem = if mode == OpMode::Cpy 
                { Rc::new(RefCell::new(array[i].borrow().clone())) }
            else
                { array[i].clone() };
            nw_array.push(elem);
        }
        Vl(Values::Array(nw_array))
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
                scope.add(nm.to_string(), *v, *is_mut);
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
        Ast::While(one, two) | Ast::Bop(one, _, two ) => {
            capture_into_scope(one, scope, old_scope);
            capture_into_scope(two, scope, old_scope);
        }
        _ => (),

    }
}

/// Evaluates a function application
/// Requires arguments are expressions that do no modify any values in the scope
fn eval_fn_app(func: &Values, args: &Vec<Box<Ast>>, scope: &mut impl Environment)
    -> Res
{
    if let f @ Values::Func(..) = func {
        let mut val_args = Vec::<Values>::new();
        let mut arg_scope = ImmuScope::from(scope);
        for expr in args {
            match eval(&*expr, &mut arg_scope) {
                Vl(v) => val_args.push(v),
                e => return e,
            }
        }
        apply_function(f, val_args, false)
    } else {
        str_exn(NFUNC)
    }

}

/// Evaluates the pipeline argument `left |> func`
fn eval_pipeline(left: Values, func: Values) -> Res 
{
    apply_function(&func, vec![left], true)
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
    let mut lst = Vec::<Rc<RefCell<Values>>>::new();
    for expr in elems {
        match eval(expr, scope) {
            Vl(val) => lst.push(Rc::new(RefCell::new(val))),
            e => return e,
        }
    }
    Vl(Values::Array(lst))
}

fn eval_tuple(elems: &Vec<Box<Ast>>, scope: &mut impl Environment) -> Res {
    let mut lst = Vec::<Box<Values>>::new();
    for expr in elems {
        match eval(expr, scope) {
            Vl(val) => lst.push(Box::new(val)),
            e => return e,
        }
    }
    Vl(Values::Tuple(lst))
}

/// Evaluates a map
fn eval_map(es: &HashMap<String, Box<Ast>>, scope: &mut impl Environment) -> Res {
    let mut map = HashMap::<String, Rc<RefCell<Values>>>::new();
    for (k, v) in es {
        match eval(v, scope) {
            Vl(val) => map.insert(k.clone(), Rc::new(RefCell::new(val))),
            e => return e,
        };
    }
    Vl(Values::Map(map))
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

/// Is true if `f_name` is the name of a hardcoded sapl function
fn is_rust_func(f_name: &str) -> bool {
    match f_name {
        "typeof" | "assert" => true,
        _ => false,
    }
}

/// Evaluates a hardcoded function
fn eval_rust_func(f_name: &str, args: &Vec<Box<Ast>>, scope: &mut impl Environment) -> Res {
    match f_name {
        "typeof" => eval_type(args, scope),
        "assert" => eval_assert(args, scope),
        _ => str_exn(NFUNC),
    }
}

/// Evaluates typeof()
fn eval_type(args: &Vec<Box<Ast>>, scope: &mut impl Environment) -> Res {
    if args.len() == 1 {
        match eval(&*args[0], scope) {
           Vl(v) => Vl(Values::Str(type_of(&v))),
           Res::Ret(v) => Vl(Values::Str(format!("return of {}", type_of(&v)))),
           Res::Exn(v) => Vl(Values::Str(format!("exn of {}", type_of(&v)))),
           b => b,
        }
    } else {
        inv_arg("typeof", Some("expect 1 argument"))
    }
}

/// Evaluates assert()
fn eval_assert(args: &Vec<Box<Ast>>, scope: &mut impl Environment) -> Res {
    if args.len() == 2 {
        if let Vl(Values::Str(msg)) = eval(&*args[1], scope) {
            match eval(&*args[0], scope) {
                Vl(Values::Bool(true)) => Vl(Values::Unit),
                Vl(Values::Bool(false)) => Bad(format!("Assertation error: '{}'", msg)),
                _ => inv_arg("assert", Some("expects a boolean")),
            }
        } else {
            inv_arg("assert", Some("expects an error message"))
        }
    } else if args.len() == 1 {
        match eval(&*args[0], scope) {
            Vl(Values::Bool(true)) => Vl(Values::Unit),
            Vl(Values::Bool(false)) => Bad(format!("Assertation error")),
            _ => inv_arg("assert", Some("expects a boolean")),
        }
    } else {
        inv_arg("assert", Some("invalid arg count"))
    }
}

/// Gets the string representation of the type of `v`
fn type_of(v: &Values) -> String {
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
        Values::Ref(..) => "ref".to_owned(),
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
            eval_for_array(vals, names, ife, body, scope),
        Vl(Values::Map(map)) => 
            eval_for_map(map, names, ife, body, scope),
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

fn eval_for_array(arr: Vec<Rc<RefCell<Values>>>, names: &Vec<String>, 
    ife: Option<&Ast>, body: &Ast, scope: &mut impl Environment) -> Res 
{
    for val in arr.into_iter() {
        let val = val.borrow().clone();
        let mut child_scope = ScopeProxy::new(scope);
        match (names.len(), val) {
            (x, Values::Tuple(tup)) if x == tup.len() => {
                for (name, v) in names.iter().zip(tup.into_iter()) {
                    child_scope.add(name.to_string(), *v, false);
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

fn eval_for_map(map: HashMap<String, Rc<RefCell<Values>>>, names: &Vec<String>, 
    ife: Option<&Ast>, body: &Ast, scope: &mut impl Environment) -> Res 
{
    for (key, val) in map.into_iter() {
        let mut child_scope = ScopeProxy::new(scope);
        match names.len() {
            2 => {
                child_scope.add(names[0].to_string(), 
                    Values::Str(key), false);
                child_scope.add(names[1].to_string(), 
                    val.borrow().clone(), false);
            },
            1 => child_scope.add(names[0].to_string(), 
                Values::Tuple(
                    vec![Box::new(Values::Str(key)), 
                    Box::new(val.borrow().clone())]), 
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