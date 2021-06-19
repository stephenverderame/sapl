use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use crate::parser::Ast;
use std::collections::HashMap;
use super::environment::{Scope, Environment};

#[derive(Clone)]
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
    RustFunc(Rc<dyn Fn(Vec<Values>) -> Res>, usize),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Res {
    Vl(Values),
    Exn(Values),
    Bad(String),
    Ret(Values),
}

use Res::*;

impl PartialEq for Values {
    fn eq(&self, other: &Values) -> bool {
        use Values::*;
        match (self, other) {
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Bool(a), Bool(b)) => a == b,
            (Str(a), Str(b)) => a == b,
            (Array(a), Array(b)) => a == b,
            (Map(a), Map(b)) => a == b,
            (Ref(a, c), Ref(b, d)) => a == b && c == d,
            (Tuple(a), Tuple(b)) => a == b,
            (Func(a, b, c, d), Func(x, y, z, w)) => 
                a == x && b == y && c == z && d == w,
            (Range(a, b), Range(c, d)) => a == c && b == d,
            (Unit, Unit) => true,
            (Placeholder, Placeholder) => true,
            _ => false,
        }
    }
}

impl std::fmt::Debug for Values {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Values::*;
        match self {
            RustFunc(..) | Func(..) => write!(f, "<function>"),
            Int(x) => write!(f, "{}", x),
            Str(x) => write!(f, "{}", x),
            Bool(x) => write!(f, "{}", x),
            Float(x) => write!(f, "{}", x),
            Array(x) | Tuple(x) => write!(f, "{:?}", x),
            Map(x) => write!(f, "{:?}", x),
            Ref(x, _) => write!(f, "{:?}", x),
            Placeholder => write!(f, "<placeholder>"),
            Unit => write!(f, "<unit>"),
            Range(a, b) => write!(f, "{:?}..{:?}", a, b),          
        }
    }
}

/// Converts a value `b` into the closest boole equivalent
/// Non-empty strings and arrays, true booleans, non zero ints and floats 
/// and ranges with different first and second elements
/// become true
/// Everything else becomes false
pub fn to_booly(b: &Res) -> Result<bool, String> {
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

/// Evaluates a list of arguments `args` from Asts
/// Passes each evaluated argument to `closure`
/// Returns the return of `closure` if `closure` returns `Some`
/// If `closure` returns `None`, keeps looking through `args`
/// If any argument evaluates to a non value, returns that non-value
pub fn eval_args<F>(args: &Vec<Box<Ast>>, scope: &mut impl Environment, mut closure: F) -> Res 
    where F : FnMut(Values) -> Option<Res> 
{
    for arg in args {
        match super::eval(arg, scope) {
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