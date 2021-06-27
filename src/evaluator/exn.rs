use crate::evaluator::Res;
use crate::evaluator::Values;
use crate::parser::Op;

pub const UNKNOWN_NAME: &'static str = "Unknown name exception";
pub const PCE_FAIL: &'static str = "Function post-condition violated";
pub const INV_UOP: &'static str = "Invalid value or operation";
pub const IMMU_ERR: &'static str = "Cannot mutate an immutable value";
pub const NREF: &'static str = "Applying reference operator to non-reference";
pub const INV_ARG: &'static str = "Invalid argument to function";
pub const DIV_Z: &'static str = "Divide by zero exception";
pub const IDX_BNDS: &'static str = "Index out of bounds";
pub const NFUNC: &'static str = "Not a function";
pub const INV_DEF: &'static str = "Invalid definition or structured binding";
pub const NON_ITER: &'static str = "Attempt to iterate over non-iterable";

pub fn str_exn(msg: &str) -> Res {
    Res::Exn(Values::Str(msg.to_owned()))
}

pub fn ukn_name(name: &String) -> Res {
    Res::Exn(Values::Str(format!("{}: {}", UNKNOWN_NAME, name)))
}

pub fn bad_op(left: &Values, right: Option<&Values>, op: Op) -> Res {
    let err_msg = if right == None {
        format!("{:?} {:?} {:?}", left, op, right)
    } else {
        format!("{:?} {:?}", op, left)
    };
    Res::Exn(Values::Str(format!("{}: {}", INV_UOP, err_msg)))
}

pub fn inv_arg(func: &str, info: Option<&str>) -> Res{
    if info == None {
        Res::Exn(Values::Str(format!("{} {}", INV_ARG, func)))
    } else {
        Res::Exn(Values::Str(format!("{} {} : {}", INV_ARG, func, info.unwrap())))
    }
}