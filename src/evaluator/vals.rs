use super::environment::{Environment, Scope};
use super::eval_class::Class;
use crate::parser::Ast;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::{Rc, Weak};

#[derive(Clone, PartialEq, Copy)]
pub enum CallingContext {
    Constructor,
    SelfCall,
    Public,
}

#[derive(Clone)]
pub enum Values {
    Int(i32),
    Float(f64),
    Str(String),
    Unit,
    Bool(bool),
    Func(
        Vec<(String, bool, Option<Ast>)>,
        Ast,
        Rc<RefCell<Scope>>,
        Option<Ast>,
    ),
    Array(Box<Vec<Values>>),
    Tuple(Box<Vec<Values>>),
    Range(Box<Values>, Box<Values>),
    Map(HashMap<Values, Values>),
    Placeholder,
    Ref(Rc<RefCell<Values>>, bool),
    WeakRef(Weak<RefCell<Values>>, bool), //Invariant: weak ref must always be valid
    RustFunc(Rc<dyn Fn(Vec<Values>) -> Res>, usize),
    Object(Rc<RefCell<Class>>, CallingContext),
    Type(Rc<Class>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Res {
    Vl(Values),
    Exn(Values),
    Bad(String),
    Ret(Values),
}

use Res::*;

fn map_eq(a: &HashMap<Values, Values>, b: &HashMap<Values, Values>) -> bool {
    if a.len() != b.len() {
        return false;
    }
    for (k, v) in a {
        if !b.contains_key(k) {
            return false;
        }
        if b[k] != *v {
            return false;
        }
    }
    true
}

impl PartialEq for Values {
    fn eq(&self, other: &Values) -> bool {
        use Values::*;
        match (self, other) {
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Bool(a), Bool(b)) => a == b,
            (Str(a), Str(b)) => a == b,
            (Array(a), Array(b)) => a == b,
            (Map(a), Map(b)) => map_eq(a, b),
            (Ref(a, c), Ref(b, d)) => a == b && c == d,
            (Tuple(a), Tuple(b)) => a == b,
            (Func(a, b, c, d), Func(x, y, z, w)) => a == x && b == y && c == z && d == w,
            (Range(a, b), Range(c, d)) => a == c && b == d,
            (Unit, Unit) => true,
            (Placeholder, Placeholder) => true,
            (Object(a, _), Object(b, _)) => a == b,
            (Type(a), Type(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Values {}

impl std::fmt::Debug for Values {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Values::*;
        match self {
            RustFunc(_, args) => write!(f, "<function> : {}", args),
            Func(params, ..) => write!(f, "<function> : {}", params.len()),
            Int(x) => write!(f, "{}", x),
            Str(x) => write!(f, "{}", x),
            Bool(x) => write!(f, "{}", x),
            Float(x) => write!(f, "{}", x),
            Array(x) | Tuple(x) => write!(f, "{:?}", x),
            Map(x) => write!(f, "{:?}", x),
            Ref(x, _) => write!(f, "ref {:?}", &*x.borrow()),
            WeakRef(x, _) => write!(f, "weak {:?}", &*x.upgrade().unwrap().borrow()),
            Placeholder => write!(f, "<placeholder>"),
            Unit => write!(f, "<unit>"),
            Range(a, b) => write!(f, "{:?}..{:?}", a, b),
            Object(a, _) => write!(f, "Object {{ {:?} }}", &*a.borrow()),
            Type(a) => write!(f, "Type {{ {:?} }}", a),
        }
    }
}

impl std::fmt::Display for Values {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Values::*;
        match self {
            RustFunc(_, args) => write!(f, "<function> : {}", args),
            Func(params, ..) => write!(f, "<function> : {}", params.len()),
            Int(x) => write!(f, "{}", x),
            Str(x) => write!(f, "{}", x),
            Bool(x) => write!(f, "{}", x),
            Float(x) => write!(f, "{}", x),
            Array(x) | Tuple(x) => write!(f, "{:?}", &*x),
            Map(x) => write!(f, "{:?}", &*x),
            Ref(x, _) => write!(f, "ref {}", &*x.borrow()),
            WeakRef(x, _) => write!(f, "weak {:?}", &*x.upgrade().unwrap().borrow()),
            Placeholder | Unit => Ok(()),
            Range(a, b) => write!(f, "{}..{}", &*a, &*b),
            Object(a, _) => write!(f, "Object {{ {:?} }}", &*a.borrow()),
            Type(a) => write!(f, "Type {{ {:?} }}", a),
        }
    }
}

impl std::hash::Hash for Values {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
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
        Vl(Values::Func(..))
        | Vl(Values::Tuple(..))
        | Vl(Values::Object(..))
        | Vl(Values::Type(_)) => Ok(true),
        Vl(_) => Ok(false),
        Bad(e) => Err(e.to_string()),
        e => Err(format!(
            "Return/exn value cannot be converted to bool: got {:?}",
            e
        )),
    }
}

pub fn type_conversion(val: Values, as_type: &String) -> Res {
    use super::exn::*;
    use std::convert::TryInto;
    match (val, &as_type[..]) {
        (Values::Func(..), _) | (Values::RustFunc(..), _) => str_exn("Cannot cast function types"),
        (x, y) if super::std_sapl::type_of(&x) == y => Vl(x),
        (Values::Str(x), "bool") if x == "true" => Vl(Values::Bool(true)),
        (Values::Str(x), "bool") if x == "false" => Vl(Values::Bool(false)),
        (x, "string") => Vl(Values::Str(format!("{:?}", x))),
        (x, "bool") => Vl(Values::Bool(match to_booly(&Vl(x)) {
            Ok(v) => v,
            Err(msg) => return Res::Bad(msg),
        })),
        (Values::Int(a), "float") => Vl(Values::Float(a as f64)),
        (Values::Float(a), "int") => Vl(Values::Int(a as i32)),
        (Values::Bool(a), "int") => Vl(Values::Int(if a { 1 } else { 0 })),
        (Values::Bool(a), "float") => Vl(Values::Float(if a { 1.0 } else { 0.0 })),
        (Values::Tuple(x), "array") => Vl(Values::Array(x)),
        (Values::Array(x), "tuple") => Vl(Values::Tuple(x)),
        (Values::Str(x), "int") => match x.parse::<i32>() {
            Ok(num) => Vl(Values::Int(num)),
            _ => str_exn("Cannot parse str to int"),
        },
        (Values::Str(x), "float") => match x.parse::<f64>() {
            Ok(num) => Vl(Values::Float(num)),
            _ => str_exn("Cannot parse str to int"),
        },
        (Values::Map(map), "array") => {
            let mut array = Vec::<Values>::new();
            for (k, v) in map.into_iter() {
                let tup = Values::Tuple(Box::new(vec![k, v]));
                array.push(tup);
            }
            Vl(Values::Array(Box::new(array)))
        }
        (Values::Array(arr), "map") => {
            let mut map = HashMap::<Values, Values>::new();
            for i in 0..arr.len() {
                map.insert(Values::Int(i.try_into().unwrap()), arr[i].clone());
            }
            Vl(Values::Map(map))
        }
        (vl, typ) => str_exn(&format!("Unknown type conversion from {:?} to {:?}", vl, typ)[..]),
    }
}

/// Evaluates a list of arguments `args` from Asts
/// Passes each evaluated argument to `closure`
/// Returns the return of `closure` if `closure` returns `Some`
/// If `closure` returns `None`, keeps looking through `args`
/// If any argument evaluates to a non value, returns that non-value
pub fn eval_args<F>(args: &Vec<Box<Ast>>, scope: &mut impl Environment, mut closure: F) -> Res
where
    F: FnMut(Values) -> Option<Res>,
{
    for arg in args {
        match super::eval(arg, scope) {
            Vl(val) => match closure(val) {
                Some(ret) => return ret,
                None => (),
            },
            e => return e,
        }
    }
    Vl(Values::Unit)
}
