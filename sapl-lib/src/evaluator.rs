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
    Func(Vec<String>, Ast, Rc<RefCell<Scope>>),
    Array(Vec<Box<Values>>),
    Tuple(Vec<Box<Values>>),
    Range(Box<Values>, Box<Values>),
    Map(HashMap<String, Box<Values>>),
    Placeholder
}

#[derive(PartialEq, Debug, Clone)]
pub enum Res {
    Vl(Values),
    Exn(Values),
    Bad(String),
    Ret(Values),
}

use Res::Vl;
use Res::Bad;

fn str_exn(msg: &str) -> Res {
    Res::Exn(Values::Str(msg.to_owned()))
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
            Ok(val) => Vl(val.clone()),
            Err(e) => Bad(e),
        },
        Ast::Bop(left, op, right) => eval_bop(&*left, op, &*right, scope),
        Ast::If(guard, body, other) => eval_if(&*guard, &*body, other, scope),
        Ast::Seq(children) => eval_seq(children, scope),
        Ast::Let(name, ast) => eval_let(name, &*ast, scope),
        Ast::Func(name, params, ast) => eval_func(name, params, &*ast, scope),
        Ast::Lambda(params, ast) => eval_lambda(params, &*ast, scope),
        Ast::FnApply(name, args) => match scope.find(name) {
            Ok(val) => eval_fn_app(val, args, &mut ImmuScope::from(scope)),
            Err(e) => Bad(e),
        },
        Ast::Array(elems) => eval_arr(elems, scope, false),
        Ast::Uop(ast, op) => eval_uop(ast, op, scope),
        Ast::Tuple(elems) => eval_arr(elems, scope, true),
        Ast::Map(es) => eval_map(es, scope),
        Ast::Placeholder => Vl(Values::Placeholder),
    }
}
/// Evaluates `left op right`
/// If `op` is a short circuit operator, only evaluates `left` if the short circuit path is taken
/// Otherwise evaluates both `left` and `right` and then performs the bop between the two values
fn eval_bop(left: &Ast, op: &Op, right: &Ast, scope: &mut impl Environment) -> Res {
    if op == &Op::Dot {
        return perform_dot_op(left, right, scope);
    }
    let left = eval(left, scope);
    match (left, op) {
        (Vl(Values::Bool(true)), Op::Lor) => Vl(Values::Bool(true)),
        (Vl(Values::Bool(false)), Op::Land) => Vl(Values::Bool(false)),
        (Vl(val), op) => {
            match (eval(right, scope), op) {
                (Vl(right), op @ Op::Eq) | (Vl(right), op @ Op::Neq) =>
                    perform_eq_test(val, op, right),
                (Vl(right), Op::Index) => perform_index_op(val, right),
                (Vl(right), Op::Range) => Vl(Values::Range(Box::new(val), Box::new(right))),
                (Vl(right), Op::Concat) => perform_concat(val, right),
                (Vl(right), op) => perform_bop(val, op, right),
                (e, _) => e,
            }
        },
        (e, _) => e,
    }
}

fn eval_uop(left: &Ast, op: &Op, scope: &mut impl Environment) -> Res {
    match (eval(left, scope), op) {
        (Vl(Values::Bool(x)), Op::Neg) => Vl(Values::Bool(!x)),
        (Vl(Values::Int(x)), Op::Neg) => Vl(Values::Int(-x)),
        (Vl(Values::Float(x)), Op::Neg) => Vl(Values::Float(-x)),
        (Vl(v), Op::Neg) => str_exn(&format!("{:?} cannot be negated", v)[..]),
        (v, Op::AsBool) => match to_booly(v) {
            Ok(b) => Vl(Values::Bool(b)),
            Err(e) => Bad(e),
        },
        (Vl(v), Op::Return) => Res::Ret(v),
        (Vl(v), Op::Throw) => Res::Exn(v),
        (Vl(_), x) => str_exn(&format!("{:?} is not a Uop", x)[..]),
        (e, _) => e,
    }
}

/// Performs the dot operator `.` on `left.right`
fn perform_dot_op(left: &Ast, right: &Ast, scope: &mut impl Environment) -> Res {
    if let Ast::Name(x) = left {
        match scope.find(x) {
            Ok(v) => do_dot_ref_op(v, right, &mut ImmuScope::from(scope)),
            Err(e) => return Bad(e),
        }
    } else {
        match eval(left, scope) {
            Vl(v) => do_dot_mv_op(v, right, scope),
            e => return e,
        }
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
        (Values::Map(map), Ast::Name(nm)) if map.contains_key(nm) =>
            Vl((**(map.get(nm).unwrap())).clone()),
        (Values::Range(fst, _),
            Ast::FnApply(name, vec)) if name.eq("fst") && vec.is_empty() =>
                Vl(*fst.clone()),
        (Values::Range(_, snd),
            Ast::FnApply(name, vec)) if name.eq("snd") && vec.is_empty() =>
                Vl(*snd.clone()),
        (Values::Map(map), Ast::FnApply(fn_name, args)) if map.contains_key(fn_name) =>
            eval_fn_app(map.get(fn_name).unwrap(), args, scope),
        (l, r) => str_exn(&format!("Unrecognized member {:?} in {:?}", r, l)[..]),
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
        (l, r) => do_dot_ref_op(&l, r, scope),
    }
}

fn perform_concat(left: Values, right: Values) -> Res {
    match (left, right) {
        (Values::Array(mut x), y) => {
            x.push(Box::new(y));
            Vl(Values::Array(x))
        },
        (Values::Map(mut x), Values::Tuple(mut y)) if y.len() == 2 => {
            if let (Values::Str(key), val) = (*y.swap_remove(0), y.pop().unwrap()) {
                x.insert(key, val);
                Vl(Values::Map(x))
            } else {
                str_exn("Can only concatenate maps with tuples")
            }
        },
        (Values::Map(mut x), Values::Array(y)) => {
            for elem in y.into_iter() {
                if let Values::Tuple(mut pair) = *elem {
                    if pair.len() == 2 {
                        if let (Values::Str(key), val) = (*pair.swap_remove(0), pair.pop().unwrap()) {
                            x.insert(key, val);
                        } else {
                            return str_exn("Can only concatenate maps with list of k/v pairs");
                        }
                    } else {
                        return str_exn("Assoc list to concat with map must contain 2 tuples");
                    }
                } else {
                    return str_exn("Must concatenate map with a list of k/v pairs");
                }
            };
            Vl(Values::Map(x))
        },
        (Values::Map(mut x), Values::Map(y)) => {
            for val in y.into_iter() {
                let (k, v) = val;
                x.insert(k, v);
            };
            Vl(Values::Map(x))
        },
        (l, r) => str_exn(&format!("Invalid concatenation of {:?} onto {:?}", r, l)[..]),
    }
}

/// Returns a boolean value if `map` contains all values in `args` once
/// `args` is evaluated
fn map_contains_all(map: &HashMap<String, Box<Values>>, args: &Vec<Box<Ast>>,
    scope: &mut impl Environment) -> Res
{
    for e in args {
        match eval(&*e, scope) {
            Vl(Values::Str(name)) => if map.get(&name) == None {
                return Vl(Values::Bool(false));
            },
            Vl(_) => return Res::Exn(Values::Str("Map contains(): non string key".to_owned())),
            e => return e,
        }
    };
    Vl(Values::Bool(true))
}

/// Indexes `right` from `left`
fn perform_index_op(left: Values, right: Values) -> Res {
    match (left, right) {
        (Values::Array(mut x), Values::Int(idx)) => {
            if idx < x.len() as i32 && idx >= 0 {
                Vl(*(x.swap_remove(idx as usize)))
            } else {
                Res::Exn(Values::Str(format!(
                    "Index out of bounds error {:?} has no index {:?}", x, idx)))
            }
        },
        (Values::Array(x), Values::Range(min, max)) => {
            if let (Values::Int(min), Values::Int(max)) = (*min, *max) {
                if min <= max && min > 0 && max < x.len() as i32 {
                    return Vl(
                        Values::Array(
                            x.get(min as usize..max as usize).unwrap().to_vec()
                        )
                    );
                }
            }
            Res::Exn(Values::Str("Invalid range for indexer".to_owned()))
        },
        (Values::Map(x), Values::Str(name)) => {
            match x.get(&name) {
                Some(member) => Vl(*member.clone()),
                _ => Res::Exn(Values::Str(format!("Map has no member {}", name))),
            }
        },
        _ => str_exn("Unrecognized indexer"),
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
        (_, Op::Div, Values::Int(y)) if y == 0 => 
            Res::Exn(Values::Str("Divide by zero exception".to_owned())),
        (_, Op::Div, Values::Float(y)) if y == 0.0 => 
            Res::Exn(Values::Str("Divide by zero exception".to_owned())),
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

        (Values::Bool(x), Op::Lor, Values::Bool(y)) => Vl(Values::Bool(x || y)),
        (Values::Bool(x), Op::Land, Values::Bool(y)) => Vl(Values::Bool(x && y)),

        (val, Op::Pipeline, f) => eval_pipeline(val, f),

        (Values::Array(mut x), Op::Plus, Values::Array(y)) => {
            for val in y.into_iter() {
                x.push(val)
            };
            Vl(Values::Array(x))
        },
        (x, op, y) => 
            Res::Exn(Values::Str(
                format!("'{:?} {:?} {:?}' is an invalid Bop or invalid arguments", x, op, y)))
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
        _ => Bad("Not an equality test".to_owned()),
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
    match to_booly(eval(guard, scope)) {
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
fn to_booly(b: Res) -> Result<bool, String> {
    match b {
        Vl(Values::Bool(true)) => Ok(true),
        Vl(Values::Int(x)) if x != 0 => Ok(true),
        Vl(Values::Str(x)) if !x.is_empty() => Ok(true),
        Vl(Values::Float(x)) if x.abs() > 0.0001 => Ok(true),
        Vl(Values::Array(x)) if !x.is_empty() => Ok(true),
        Vl(Values::Range(a, b)) if a != b => Ok(true),
        Vl(_) => Ok(false),
        Bad(e) => Err(e),
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
fn eval_let(names: &Vec<String>, ast: &Ast, scope: &mut impl Environment) 
    -> Res 
{
    match eval(ast, scope) {
        Vl(val) if names.len() == 1 => {
            let nm = names.get(0).unwrap();
            scope.add(nm.to_string(), val, false);
            Vl(Values::Unit)
        },
        Vl(Values::Range(a, b)) if names.len() == 2 => {
            scope.add(names.get(0).unwrap().to_string(), *a, false);
            scope.add(names.get(1).unwrap().to_string(), *b, false);
            Vl(Values::Unit)
        },
        Vl(Values::Tuple(es)) if names.len() == es.len() => {
            for (nm, v) in names.iter().zip(es.into_iter()) {
                scope.add(nm.to_string(), *v, false);
            }
            Vl(Values::Unit)
        },
        Vl(_) => Bad("Invalid structured binding in let definition".to_owned()),
        err => err,
    }
}

/// Evaluates a function definition and adds the function to `scope`
/// Captures names references in `ast` that are also in `scope` by copying them into a new
/// environment
fn eval_func(name: &String, params: &Vec<String>, ast: &Ast, scope: &mut impl Environment)
    -> Res
{
    let nw_scope = Rc::new(RefCell::new(Scope::new()));
    scope.add(name.to_string(), 
        Values::Func(params.clone(), ast.clone(), nw_scope.clone()), false);
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
        Values::Func(params.clone(), ast.clone(), nw_scope.clone()), false);
    capture_into_scope(ast, &mut nw_scope.borrow_mut(), scope);
    Vl(Values::Func(params.clone(), ast.clone(), nw_scope))
}

/// Searches for names in `ast` and captures them by copying their corresponding value from
/// `old_scope` into `scope`
fn capture_into_scope(ast: &Ast, scope: &mut Scope, old_scope: &impl Environment) {
    match ast {
        Ast::Name(x) => {
            if let Ok(val) = old_scope.find(x) {
                scope.add(x.to_string(), val.clone(), false)
            }
        },
        Ast::If(guard, body, other) => {
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
        Ast::Bop(left, _, right) => {
            capture_into_scope(&*left, scope, old_scope);
            capture_into_scope(&*right, scope, old_scope);
        },
        Ast::Let(_, ast) => capture_into_scope(&*ast, scope, old_scope),
        Ast::Func(.., ast) | Ast::Lambda(_, ast) => capture_into_scope(&*ast, scope, old_scope),
        Ast::FnApply(name, args) => {
            if let Ok(val) = old_scope.find(name) {
                scope.add(name.to_string(), val.clone(), false);
            }
            for expr in args {
                capture_into_scope(&*expr, scope, old_scope);
            }
        },
        Ast::Uop(ast, _) => capture_into_scope(&*ast, scope, old_scope),
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
        str_exn("Variable is not a function")
    }

}

/// Evaluates the pipeline argument `left |> func`
fn eval_pipeline(left: Values, func: Values) 
    -> Res 
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
    if let Values::Func(params, ast, fn_scope) = func {
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
                Rc::new(RefCell::new(sc.cpy()))))
        } else {
            match eval(ast, &mut sc) {
                Res::Ret(v) => Vl(v),
                e => e,
            }
        }
    } else {
        str_exn("Not a function being applied")
    }
}

/// Evaluates an array of asts into an array of values
fn eval_arr(elems: &Vec<Box<Ast>>, scope: &mut impl Environment, tuple: bool) -> Res {
    let mut lst = Vec::<Box<Values>>::new();
    for expr in elems {
        match eval(expr, scope) {
            Vl(val) => lst.push(Box::new(val)),
            e => return e,
        }
    }
    if tuple {
        Vl(Values::Tuple(lst))
    }
    else {
        Vl(Values::Array(lst))
    }
}

/// Evaluates a map
fn eval_map(es: &HashMap<String, Box<Ast>>, scope: &mut impl Environment) -> Res {
    let mut map = HashMap::<String, Box<Values>>::new();
    for (k, v) in es {
        match eval(v, scope) {
            Vl(val) => map.insert(k.clone(), Box::new(val)),
            e => return e,
        };
    }
    Vl(Values::Map(map))
}