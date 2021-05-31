use crate::parser::Ast;
use crate::parser::Op;

#[derive(PartialEq, Debug)]
pub enum Values {
    Int(i32),
    Float(f64),
    Str(String),
    Unit
}

/// Evaluates the ast to a value
pub fn eval(ast: Ast) ->  Result<Values, String> {
    match ast {
        Ast::VFloat(x) => Ok(Values::Float(x)),
        Ast::VInt(x) => Ok(Values::Int(x)),
        Ast::VStr(x) => Ok(Values::Str(x)),
        Ast::Bop(left, op, right) => eval_bop(*left, op, *right),
    }
}

/// Evaluates each subtree `left` and `right` and if they are both valid
/// Performs the operation `left op right`
fn eval_bop(left: Ast, op: Op, right: Ast) -> Result<Values, String> {
    let left = eval(left);
    let right = eval(right);
    match (left, right) {
        (Ok(vleft), Ok(vright)) => perform_bop(vleft, op, vright),
        (Err(e), _) | (_, Err(e)) => Err(e),      
    }
}

/// Evaluates the binary operator `op` with arguments `vleft` and `vright`
fn perform_bop(vleft: Values, op: Op, vright: Values) -> Result<Values, String> {
    let (a, b) = promote_args(vleft, vright);
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
        (Values::Float(x), Op::Plus, Values::Float(y)) => Ok(Values::Float(x + y)),
        (Values::Float(x), Op::Sub, Values::Float(y)) => Ok(Values::Float(x - y)),
        (Values::Float(x), Op::Mult, Values::Float(y)) => Ok(Values::Float(x * y)),
        (Values::Float(x), Op::Div, Values::Float(y)) => Ok(Values::Float(x / y)),
        (Values::Float(x), Op::Exp, Values::Float(y)) => 
            Ok(Values::Float(f64::powf(x, y))),
        (Values::Str(x), Op::Plus, Values::Str(y)) => Ok(Values::Str(x + &y)),
        (x, op, y) => 
            Err(format!("'{:?} {:?} {:?}' is an invalid Bop or invalid arguments", x, op, y))
    }
}

/// Promotes the arguments `a` and `b` to the same type if they are mismatched
/// String and anything will become two strings
/// Float and Int will become two Floats
/// Everything else remains the same
fn promote_args(a: Values, b: Values) -> (Values, Values) {
    match (a, b) {
        (Values::Float(x), Values::Int(y)) => 
            (Values::Float(x), Values::Float(y.into())),
        (Values::Int(x), Values::Float(y)) => 
            (Values::Float(x.into()), Values::Float(y)),
        (Values::Str(x), Values::Int(y)) => 
            (Values::Str(x), Values::Str(y.to_string())),
        (Values::Str(x), Values::Float(y)) => 
            (Values::Str(x), Values::Str(y.to_string())),
        (Values::Int(y), Values::Str(x)) => 
            (Values::Str(y.to_string()), Values::Str(x)),
        (Values::Float(y), Values::Str(x)) => 
            (Values::Str(y.to_string()), Values::Str(x)),
        p => p,
    }
}