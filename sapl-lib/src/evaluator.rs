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


/// Evaluates a SAPL AST
pub fn evaluate(ast: &Ast) -> Result<Values, String> {
    eval(ast, &mut Scope::new())
}

/// Evaluates the ast to a value
fn eval(ast: &Ast, scope: &mut impl Environment) -> Result<Values, String> {
    match ast {
        Ast::VFloat(x) => Ok(Values::Float(*x)),
        Ast::VInt(x) => Ok(Values::Int(*x)),
        Ast::VStr(x) => Ok(Values::Str(x.clone())),
        Ast::VBool(x) => Ok(Values::Bool(*x)),
        Ast::Name(x) => match scope.find(x) {
            Ok(val) => Ok(val.clone()),
            Err(e) => Err(e),
        },
        Ast::Bop(left, op, right) => eval_bop(&*left, op, &*right, scope),
        Ast::If(guard, body, other) => eval_if(&*guard, &*body, other, scope),
        Ast::Seq(children) => eval_seq(children, scope),
        Ast::Let(name, ast) => eval_let(name, &*ast, scope),
        Ast::Func(name, params, ast) => eval_func(name, params, &*ast, scope),
        Ast::Lambda(params, ast) => eval_lambda(params, &*ast, scope),
        Ast::FnApply(name, args) => match scope.find(name) {
            Ok(val) => eval_fn_app(val, args, &mut ImmuScope::from(scope)),
            Err(e) => Err(e),
        },
        Ast::Array(elems) => eval_arr(elems, scope, false),
        Ast::Uop(ast, op) => eval_uop(ast, op, scope),
        Ast::Tuple(elems) => eval_arr(elems, scope, true),
        Ast::Map(es) => eval_map(es, scope),
        Ast::Placeholder => Ok(Values::Placeholder),
    }
}
/// Evaluates `left op right`
/// If `op` is a short circuit operator, only evaluates `left` if the short circuit path is taken
/// Otherwise evaluates both `left` and `right` and then performs the bop between the two values
fn eval_bop(left: &Ast, op: &Op, right: &Ast, scope: &mut impl Environment) -> Result<Values, String> {
    let left = eval(left, scope);
    match (left, op) {
        (Ok(Values::Bool(true)), Op::Lor) => Ok(Values::Bool(true)),
        (Ok(Values::Bool(false)), Op::Land) => Ok(Values::Bool(false)),
        (Ok(val), Op::Dot) => perform_dot_op(val, right, scope),
        (Ok(val), op) => {
            match (eval(right, scope), op) {
                (Ok(right), op @ Op::Eq) | (Ok(right), op @ Op::Neq) =>
                    perform_eq_test(val, op, right),
                (Ok(right), Op::Index) => perform_index_op(val, right),
                (Ok(right), Op::Range) => Ok(Values::Range(Box::new(val), Box::new(right))),
                (Ok(right), op) => perform_bop(val, op, right),
                (e, _) => e,
            }
        },
        (e, _) => e,
    }
}

fn eval_uop(left: &Ast, op: &Op, scope: &mut impl Environment) -> Result<Values, String> {
    match (eval(left, scope), op) {
        (Ok(Values::Bool(x)), Op::Neg) => Ok(Values::Bool(!x)),
        (Ok(Values::Int(x)), Op::Neg) => Ok(Values::Int(-x)),
        (Ok(Values::Float(x)), Op::Neg) => Ok(Values::Float(-x)),
        (Ok(v), Op::Neg) => Err(format!("{:?} cannot be negated", v)),
        (v, Op::AsBool) => match to_booly(v) {
            Ok(b) => Ok(Values::Bool(b)),
            Err(e) => Err(e),
        },
        (Ok(_), x) => Err(format!("{:?} is not a Uop", x)),
        (e, _) => e,
    }
}

/// Performs the dot operator `.` on `left.right`
fn perform_dot_op(left: Values, right: &Ast, scope: &mut impl Environment) -> Result<Values, String> {
    match (left, right) {
        (Values::Array(x), 
            Ast::FnApply(name, vec)) if vec.is_empty() && name.eq("size") => 
                Ok(Values::Int(x.len() as i32)),
        (Values::Array(mut x),
            Ast::FnApply(name, elems)) if name.eq("push_back") => {
                for e in elems {
                    match eval(&*e, scope) {
                        Ok(val) => x.push(Box::new(val)),
                        e => return e,
                    }
                }
                Ok(Values::Array(x))
            },
        (Values::Range(fst, _),
            Ast::FnApply(name, vec)) if name.eq("fst") && vec.is_empty() =>
                Ok(*fst),
        (Values::Range(_, snd),
            Ast::FnApply(name, vec)) if name.eq("snd") && vec.is_empty() =>
                Ok(*snd),
        (Values::Map(map), Ast::FnApply(fn_name, args)) if fn_name.eq("insert") =>
            map_insert(map, args, scope),
        (Values::Map(map), Ast::FnApply(fn_name, args)) if fn_name.eq("contains") && !args.is_empty() => {
            for e in args {
                match eval(&*e, scope) {
                    Ok(Values::Str(name)) => if map.get(&name) == None {
                        return Ok(Values::Bool(false));
                    },
                    Ok(_) => return Err("Map contains(): non string key".to_owned()),
                    e => return e,
                }
            };
            Ok(Values::Bool(true))
        },
        (Values::Map(map), Ast::Name(nm)) if map.contains_key(nm) =>
            Ok((**(map.get(nm).unwrap())).clone()),
        (Values::Map(map), Ast::FnApply(fn_name, args)) if map.contains_key(fn_name) =>
            eval_fn_app(map.get(fn_name).unwrap(), args, scope),
        (l, r) => Err(format!("Unrecognized member {:?} in {:?}", r, l)),
    }
}

/// Inserts a key-value pair into a map
/// Accepts string key and value arguments or a 2-tuple string, value
fn map_insert(mut map: HashMap<String, Box<Values>>, args: &Vec<Box<Ast>>, 
    scope: &mut impl Environment) 
    -> Result<Values, String> 
{
    if args.len() == 1 {
        if let Ok(Values::Tuple(mut x)) = eval(&*args[0], scope) {
            if x.len() == 2 {
                match (*x.swap_remove(0), *x.pop().unwrap()) {
                    (Values::Str(name), x) => {
                        map.insert(name, Box::new(x));
                        Ok(Values::Map(map))
                    },
                    _ => Err("Map key must be a string".to_owned()),
                }
            } else {
                Err("Invalid tuple for map insert".to_owned())
            }
        } else {
            Err("Invalid tuple for map insert".to_owned())
        }
    } else if args.len() == 2 {
        match (eval(&*args[0], scope), eval(&*args[1], scope)) {
            (Ok(Values::Str(x)), Ok(y)) => {
                map.insert(x, Box::new(y));
                Ok(Values::Map(map))
            },
            (Ok(_), Ok(_)) => Err("Map key must be a string".to_owned()),
            (Err(e), _) | (_, Err(e)) => Err(e),
        }
    } else {
        Err("Invalid arguments for map insert".to_owned())
    }
}

/// Indexes `right` from `left`
fn perform_index_op(left: Values, right: Values) -> Result<Values, String> {
    match (left, right) {
        (Values::Array(mut x), Values::Int(idx)) => {
            if idx < x.len() as i32 && idx >= 0 {
                Ok(*(x.swap_remove(idx as usize)))
            } else {
                Err(format!(
                    "Index out of bounds error {:?} has no index {:?}", x, idx))
            }
        },
        (Values::Array(x), Values::Range(min, max)) => {
            if let (Values::Int(min), Values::Int(max)) = (*min, *max) {
                if min <= max && min > 0 && max < x.len() as i32 {
                    return Ok(
                        Values::Array(
                            x.get(min as usize..max as usize).unwrap().to_vec()
                        )
                    );
                }
            }
            Err("Invalid range for indexer".to_owned())
        },
        (Values::Map(x), Values::Str(name)) => {
            match x.get(&name) {
                Some(member) => Ok(*member.clone()),
                _ => Err(format!("Map has no member {}", name)),
            }
        },
        _ => Err(format!("Unrecognized indexer")),
    }
}

/// Evaluates the binary operator `op` with arguments `vleft` and `vright`
/// by first promoting mismatch arguments to the same type and then evaluating them
fn perform_bop(vleft: Values, op: &Op, vright: Values) -> Result<Values, String> {
    let (a, b) = promote_args(vleft, vright, &op);
    match (a, op, b) {
        (Values::Int(x), Op::Plus, Values::Int(y)) => Ok(Values::Int(x + y)),
        (Values::Int(x), Op::Sub, Values::Int(y)) => Ok(Values::Int(x - y)),
        (Values::Int(x), Op::Mult, Values::Int(y)) => Ok(Values::Int(x * y)),
        (Values::Int(x), Op::Div, Values::Int(y)) => Ok(Values::Int(x / y)),
        (Values::Int(x), Op::Exp, Values::Int(y)) if y >= 0 => 
            Ok(Values::Int(i32::pow(x, y as u32))),
        (Values::Int(x), Op::Exp, Values::Int(y)) if y < 0 => 
            Ok(Values::Float(f64::powf(x.into(), y.into()))),
        (Values::Int(x), Op::Mod, Values::Int(y)) => Ok(Values::Int(x % y)),
        (Values::Int(x), Op::Lt, Values::Int(y)) => Ok(Values::Bool(x < y)),
        (Values::Int(x), Op::Gt, Values::Int(y)) => Ok(Values::Bool(x > y)),
        (Values::Int(x), Op::Leq, Values::Int(y)) => Ok(Values::Bool(x <= y)),
        (Values::Int(x), Op::Geq, Values::Int(y)) => Ok(Values::Bool(x >= y)),

        (Values::Float(x), Op::Plus, Values::Float(y)) => Ok(Values::Float(x + y)),
        (Values::Float(x), Op::Sub, Values::Float(y)) => Ok(Values::Float(x - y)),
        (Values::Float(x), Op::Mult, Values::Float(y)) => Ok(Values::Float(x * y)),
        (Values::Float(x), Op::Div, Values::Float(y)) => Ok(Values::Float(x / y)),
        (Values::Float(x), Op::Exp, Values::Float(y)) => 
            Ok(Values::Float(f64::powf(x, y))),
        (Values::Float(x), Op::Lt, Values::Float(y)) => Ok(Values::Bool(x < y)),
        (Values::Float(x), Op::Gt, Values::Float(y)) => Ok(Values::Bool(x > y)),
        (Values::Float(x), Op::Leq, Values::Float(y)) => Ok(Values::Bool(x <= y)),
        (Values::Float(x), Op::Geq, Values::Float(y)) => Ok(Values::Bool(x >= y)),

        (Values::Str(x), Op::Plus, Values::Str(y)) => Ok(Values::Str(x + &y)),
        (Values::Str(x), Op::Lt, Values::Str(y)) => Ok(Values::Bool(x < y)),
        (Values::Str(x), Op::Gt, Values::Str(y)) => Ok(Values::Bool(x > y)),
        (Values::Str(x), Op::Geq, Values::Str(y)) => Ok(Values::Bool(x >= y)),
        (Values::Str(x), Op::Leq, Values::Str(y)) => Ok(Values::Bool(x <= y)),

        (Values::Bool(x), Op::Lor, Values::Bool(y)) => Ok(Values::Bool(x || y)),
        (Values::Bool(x), Op::Land, Values::Bool(y)) => Ok(Values::Bool(x && y)),
        (val, Op::Pipeline, f) => eval_pipeline(val, f),
        (x, op, y) => 
            Err(format!("'{:?} {:?} {:?}' is an invalid Bop or invalid arguments", x, op, y))
    }
}

/// Determines if `left op right` where `op` is an equality operator
fn perform_eq_test(left: Values, op: &Op, right: Values) -> Result<Values, String> {
    match (left, op, right) {
        (Values::Int(x), Op::Eq, Values::Int(y)) => Ok(Values::Bool(x == y)),
        (Values::Bool(x), Op::Eq, Values::Bool(y)) => Ok(Values::Bool(x == y)),
        (Values::Str(x), Op::Eq, Values::Str(y)) => Ok(Values::Bool(x.eq(&y))),
        (Values::Str(x), Op::Neq, Values::Str(y)) => Ok(Values::Bool(!x.eq(&y))),
        (Values::Array(x), Op::Eq, Values::Array(y)) => Ok(Values::Bool(x == y)),
        (Values::Array(x), Op::Neq, Values::Array(y)) => Ok(Values::Bool(x != y)),
        (Values::Unit, Op::Eq, Values::Unit) => Ok(Values::Bool(true)),
        (_, Op::Eq, _) => Ok(Values::Bool(false)),
        (x, Op::Neq, y) if x != y => Ok(Values::Bool(true)),
        (_, Op::Neq, _) => Ok(Values::Bool(false)),
        _ => Err("Not an equality test".to_owned()),
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
    scope: &mut impl Environment) -> Result<Values, String> 
{
    match to_booly(eval(guard, scope)) {
        Ok(true) => eval(body, &mut ScopeProxy::new(scope)),
        Ok(false) => {
            if let Some(ast) = other {
                eval(&*ast, &mut ScopeProxy::new(scope))
            } else {
                Ok(Values::Unit)
            }
        },
        Err(e) => Err(e),
    }
}

/// Converts a value `b` into the closest boole equivalent
/// Non-empty strings and arrays, true booleans, non zero ints and floats 
/// and ranges with different first and second elements
/// become true
/// Everything else becomes false
fn to_booly(b: Result<Values, String>) -> Result<bool, String> {
    match b {
        Ok(Values::Bool(true)) => Ok(true),
        Ok(Values::Int(x)) if x != 0 => Ok(true),
        Ok(Values::Str(x)) if !x.is_empty() => Ok(true),
        Ok(Values::Float(x)) if x.abs() > 0.0001 => Ok(true),
        Ok(Values::Array(x)) if !x.is_empty() => Ok(true),
        Ok(Values::Range(a, b)) if a != b => Ok(true),
        Ok(_) => Ok(false),
        Err(e) => Err(e),
    }
}

/// Evaluates a sequence
fn eval_seq(children: &Vec<Box<Ast>>, scope: &mut impl Environment) 
    -> Result<Values, String> 
{
    let mut last_res : Result<Values, String> = 
        Err("No children in sequence".to_owned());
    for subtree in children {
        match eval(&*subtree, scope) {
            er @ Err(_) => return er,
            x => last_res = x,
        }
    }
    last_res
}

/// Evaluates a let definition by adding `name` to `scope`
/// Returns unit on success
fn eval_let(names: &Vec<String>, ast: &Ast, scope: &mut impl Environment) 
    -> Result<Values, String> 
{
    match eval(ast, scope) {
        Ok(val) if names.len() == 1 => {
            let nm = names.get(0).unwrap();
            scope.add(nm.to_string(), val, false);
            Ok(Values::Unit)
        },
        Ok(Values::Range(a, b)) if names.len() == 2 => {
            scope.add(names.get(0).unwrap().to_string(), *a, false);
            scope.add(names.get(1).unwrap().to_string(), *b, false);
            Ok(Values::Unit)
        },
        Ok(Values::Tuple(es)) if names.len() == es.len() => {
            for (nm, v) in names.iter().zip(es.into_iter()) {
                scope.add(nm.to_string(), *v, false);
            }
            Ok(Values::Unit)
        },
        Ok(_) => Err("Invalid structured binding in let definition".to_owned()),
        err => err,
    }
}

/// Evaluates a function definition and adds the function to `scope`
/// Captures names references in `ast` that are also in `scope` by copying them into a new
/// environment
fn eval_func(name: &String, params: &Vec<String>, ast: &Ast, scope: &mut impl Environment)
    -> Result<Values, String>
{
    let nw_scope = Rc::new(RefCell::new(Scope::new()));
    scope.add(name.to_string(), 
        Values::Func(params.clone(), ast.clone(), nw_scope.clone()), false);
    capture_into_scope(ast, &mut nw_scope.borrow_mut(), scope);
    Ok(Values::Unit)
}

/// Evaluates a lambda expression
/// Captures names references in `ast` that are also in `scope` by copying them into a new
/// environment
/// Copies "this" into the new environment to allow recursion
fn eval_lambda(params: &Vec<String>, ast: &Ast, scope: &mut impl Environment) 
    -> Result<Values, String>
{
    let nw_scope = Rc::new(RefCell::new(Scope::new()));
    nw_scope.borrow_mut().add("this".to_owned(), 
        Values::Func(params.clone(), ast.clone(), nw_scope.clone()), false);
    capture_into_scope(ast, &mut nw_scope.borrow_mut(), scope);
    Ok(Values::Func(params.clone(), ast.clone(), nw_scope))
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
        Ast::Seq(children) => {
            for node in children {
                capture_into_scope(&*node, scope, old_scope);
            }
        },
        Ast::Bop(left, _, right) => {
            capture_into_scope(&*left, scope, old_scope);
            capture_into_scope(&*right, scope, old_scope);
        },
        Ast::Let(_, ast) => capture_into_scope(&*ast, scope, old_scope),
        Ast::Func(.., ast) => capture_into_scope(&*ast, scope, old_scope),
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
    -> Result<Values, String>
{
    if let f @ Values::Func(..) = func {
        let mut val_args = Vec::<Values>::new();
        let mut arg_scope = ImmuScope::from(scope);
        for expr in args {
            match eval(&*expr, &mut arg_scope) {
                Ok(v) => val_args.push(v),
                e => return e,
            }
        }
        apply_function(f, val_args, false)
    } else {
        Err("Variable is not a function".to_owned())
    }

}

/// Evaluates the pipeline argument `left |> func`
fn eval_pipeline(left: Values, func: Values) 
    -> Result<Values, String> 
{
    apply_function(&func, vec![left], true)
}  

/// Applies `args` to the function `func`
/// If `args` contains a placeholder, performs a partial application
/// If `allow_incomplete` is true, assumes that if `args.len()` is less than
/// the amount of parameters, the remaining parameters will be placeholders and a partial application
/// will be performed
fn apply_function(func: &Values, mut args: Vec<Values>, allow_incomplete: bool) 
    -> Result<Values, String> 
{
    if let Values::Func(params, ast, fn_scope) = func {
        if allow_incomplete && args.len() < params.len() {
            args.append(&mut vec![Values::Placeholder; params.len() - args.len()]);
        } else if params.len() != args.len() { 
            return Err(format!("Arg count mismatch formals {} vs args {} and incomplete: {}",
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
            Ok(Values::Func(missing_params, ast.clone(), 
                Rc::new(RefCell::new(sc.cpy()))))
        } else {
            eval(ast, &mut sc)
        }
    } else {
        Err("Not a function being applied".to_owned())
    }
}

/// Evaluates an array of asts into an array of values
fn eval_arr(elems: &Vec<Box<Ast>>, scope: &mut impl Environment, tuple: bool) -> Result<Values, String> {
    let mut lst = Vec::<Box<Values>>::new();
    for expr in elems {
        match eval(expr, scope) {
            Ok(val) => lst.push(Box::new(val)),
            e => return e,
        }
    }
    if tuple {
        Ok(Values::Tuple(lst))
    }
    else {
        Ok(Values::Array(lst))
    }
}

/// Evaluates a map
fn eval_map(es: &HashMap<String, Box<Ast>>, scope: &mut impl Environment) -> Result<Values, String> {
    let mut map = HashMap::<String, Box<Values>>::new();
    for (k, v) in es {
        match eval(v, scope) {
            Ok(val) => map.insert(k.clone(), Box::new(val)),
            e => return e,
        };
    }
    Ok(Values::Map(map))
}