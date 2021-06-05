use crate::parser::Ast;
use crate::parser::Op;
use std::rc::Rc;
use std::cell::RefCell;

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
        Ast::FnApply(name, args) => eval_fn_app(name, args, scope),
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
        (Ok(val), op) => {
            match (eval(right, scope), op) {
                (Ok(right), op @ Op::Eq) | (Ok(right), op @ Op::Neq) =>
                    perform_eq_test(val, op, right),
                (Ok(right), op) => perform_bop(val, op, right),
                (e, _) => e,
            }
        },
        (e, _) => e,
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
    match eval(guard, scope) {
        Ok(Values::Bool(true)) => eval(body, &mut ScopeProxy::new(scope)),
        Ok(Values::Str(x)) if !x.is_empty() => eval(body, &mut ScopeProxy::new(scope)),
        Ok(Values::Int(x)) if x != 0 => eval(body, &mut ScopeProxy::new(scope)),
        Ok(Values::Bool(false)) | Ok(Values::Str(_))
        | Ok(Values::Int(_)) => {
            if let Some(ast) = other {
                eval(&*ast, &mut ScopeProxy::new(scope))
            } else {
                Ok(Values::Unit)
            }
        },
        Ok(_) => 
            Err("Condition must operate on Boolean, Integer, or String".to_owned()),
        x => x,
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
fn eval_let(name: &String, ast: &Ast, scope: &mut impl Environment) 
    -> Result<Values, String> 
{
    match eval(ast, scope) {
        Ok(val) => {
            scope.add(name.to_string(), val, false);
            Ok(Values::Unit)
        },
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
        _ => (),

    }
}

/// Evaluates a function application
/// Requires arguments are expressions that do no modify any values in the scope
fn eval_fn_app(name: &String, args: &Vec<Box<Ast>>, scope: &mut impl Environment)
    -> Result<Values, String>
{
    if let Ok(f @ Values::Func(..)) = scope.find(name) {
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