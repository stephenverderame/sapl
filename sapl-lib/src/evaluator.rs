use crate::parser::Ast;
use crate::parser::Op;

mod environment;
use environment::*;

#[derive(PartialEq, Debug, Clone)]
pub enum Values {
    Int(i32),
    Float(f64),
    Str(String),
    Unit,
    Bool(bool),
}


pub fn evaluate(ast: Ast) -> Result<Values, String> {
    eval(ast, &mut Scope::new())
}

/// Evaluates the ast to a value
fn eval(ast: Ast, scope: &mut impl Environment) -> Result<Values, String> {
    match ast {
        Ast::VFloat(x) => Ok(Values::Float(x)),
        Ast::VInt(x) => Ok(Values::Int(x)),
        Ast::VStr(x) => Ok(Values::Str(x)),
        Ast::VBool(x) => Ok(Values::Bool(x)),
        Ast::Name(x) => scope.find(x),
        Ast::Bop(left, op, right) => eval_bop(*left, op, *right, scope),
        Ast::If(guard, body, other) => eval_if(*guard, *body, other, scope),
        Ast::Seq(children) => eval_seq(children, scope),
        Ast::Let(name, ast) => eval_let(name.clone(), *ast, scope),
    }
}

/// Evaluates each subtree `left` and `right` and if they are both valid
/// Performs the operation `left op right`
fn eval_bop(left: Ast, op: Op, right: Ast, scope: &mut impl Environment) -> Result<Values, String> {
    let left = eval(left, scope);
    let right = eval(right, scope);
    match (left, right) {
        (Ok(vleft), Ok(vright)) => perform_bop(vleft, op, vright),
        (Err(e), _) | (_, Err(e)) => Err(e),      
    }
}

/// Evaluates the binary operator `op` with arguments `vleft` and `vright`
fn perform_bop(vleft: Values, op: Op, vright: Values) -> Result<Values, String> {
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
        (Values::Int(x), Op::Eq, Values::Int(y)) => Ok(Values::Bool(x == y)),
        (Values::Int(x), Op::Leq, Values::Int(y)) => Ok(Values::Bool(x <= y)),
        (Values::Int(x), Op::Geq, Values::Int(y)) => Ok(Values::Bool(x >= y)),
        (Values::Int(x), Op::Neq, Values::Int(y)) => Ok(Values::Bool(x != y)),

        (Values::Float(x), Op::Plus, Values::Float(y)) => Ok(Values::Float(x + y)),
        (Values::Float(x), Op::Sub, Values::Float(y)) => Ok(Values::Float(x - y)),
        (Values::Float(x), Op::Mult, Values::Float(y)) => Ok(Values::Float(x * y)),
        (Values::Float(x), Op::Div, Values::Float(y)) => Ok(Values::Float(x / y)),
        (Values::Float(x), Op::Exp, Values::Float(y)) => 
            Ok(Values::Float(f64::powf(x, y))),
        (Values::Float(x), Op::Lt, Values::Float(y)) => Ok(Values::Bool(x < y)),
        (Values::Float(x), Op::Gt, Values::Float(y)) => Ok(Values::Bool(x > y)),
        (Values::Float(x), Op::Eq, Values::Float(y)) => Ok(Values::Bool(x == y)),
        (Values::Float(x), Op::Leq, Values::Float(y)) => Ok(Values::Bool(x <= y)),
        (Values::Float(x), Op::Geq, Values::Float(y)) => Ok(Values::Bool(x >= y)),
        (Values::Float(x), Op::Neq, Values::Float(y)) => Ok(Values::Bool(x != y)),

        (Values::Str(x), Op::Plus, Values::Str(y)) => Ok(Values::Str(x + &y)),
        (Values::Str(x), Op::Eq, Values::Str(y)) => Ok(Values::Bool(x.eq(&y))),
        (Values::Str(x), Op::Neq, Values::Str(y)) => Ok(Values::Bool(!x.eq(&y))),
        (Values::Str(x), Op::Lt, Values::Str(y)) => Ok(Values::Bool(x < y)),
        (Values::Str(x), Op::Gt, Values::Str(y)) => Ok(Values::Bool(x > y)),
        (Values::Str(x), Op::Geq, Values::Str(y)) => Ok(Values::Bool(x >= y)),
        (Values::Str(x), Op::Leq, Values::Str(y)) => Ok(Values::Bool(x <= y)),

        (Values::Bool(x), Op::Lor, Values::Bool(y)) => Ok(Values::Bool(x || y)),
        (Values::Bool(x), Op::Land, Values::Bool(y)) => Ok(Values::Bool(x && y)),
        (a, Op::Neq, b) if a != b => Ok(Values::Bool(true)),
        (x, op, y) => 
            Err(format!("'{:?} {:?} {:?}' is an invalid Bop or invalid arguments", x, op, y))
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

fn eval_if(guard: Ast, body: Ast, other: Option<Box<Ast>>, 
    scope: &mut impl Environment) -> Result<Values, String> 
{
    match eval(guard, scope) {
        Ok(Values::Bool(true)) => eval(body, &mut ScopeProxy::new(scope)),
        Ok(Values::Str(x)) if !x.is_empty() => eval(body, &mut ScopeProxy::new(scope)),
        Ok(Values::Int(x)) if x != 0 => eval(body, &mut ScopeProxy::new(scope)),
        Ok(Values::Bool(false)) | Ok(Values::Str(_))
        | Ok(Values::Int(_)) => {
            if let Some(ast) = other {
                eval(*ast, &mut ScopeProxy::new(scope))
            } else {
                Ok(Values::Unit)
            }
        },
        Ok(_) => 
            Err("Condition must operate on Boolean, Integer, or String".to_owned()),
        x => x,
    }
}

fn eval_seq(children: Vec<Box<Ast>>, scope: &mut impl Environment) 
    -> Result<Values, String> 
{
    let mut last_res : Result<Values, String> = 
        Err("No children in sequence".to_owned());
    for subtree in children {
        match eval(*subtree, scope) {
            er @ Err(_) => return er,
            x => last_res = x,
        }
        println!("After seq: {:?}", scope.find("x".to_owned()));
    }
    last_res
}

fn eval_let(name: String, ast: Ast, scope: &mut impl Environment) 
    -> Result<Values, String> 
{
    match eval(ast, scope) {
        Ok(val) => {
            scope.add(name, val, false);
            Ok(Values::Unit)
        },
        err => err,
    }
}