use crate::lexer::Tokens;
use std::collections::VecDeque;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Op {
    Plus, Mult, Div, Mod, Sub, Exp,
    Land, Lor, And, Or, Lt, Gt, Eq,
    Neq, Leq, Geq,
}

#[derive(PartialEq, Debug)]
pub enum Ast {
    VInt(i32),
    VFloat(f64),
    VStr(String),
    VBool(bool),
    Bop(Box<Ast>, Op, Box<Ast>),
}

/// Parses a stream of tokens `stream` into an Abstract Syntax Tree
pub fn parse(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.front() {
        Some(Tokens::Integer(_)) | Some(Tokens::Float(_))
        | Some(Tokens::TString(_)) | Some(Tokens::LParen)
        | Some(Tokens::Bool(_))
            => parse_expr(stream),
        _ => Err("unknown parse".to_owned()),
    }
}

/// True if `tok` is a binary operator
/// False otherwise
fn tok_is_bop(tok: &Tokens) -> bool {
    match *tok {
        Tokens::OpMod | Tokens::OpDiv | Tokens::OpPlus
        | Tokens::OpMinus | Tokens::OpOr | Tokens::OpExp
        | Tokens::OpLand | Tokens::OpLor | Tokens::OpMult 
        | Tokens::OpLeq | Tokens::OpEq | Tokens::OpLt 
        | Tokens::OpNeq | Tokens::OpGeq | Tokens::OpGt
        => true,
        _ => false,
    }
}

/// True if `tok` is a value token
fn tok_is_val(tok: &Tokens) -> bool {
    match tok {
        Tokens::Integer(_) | Tokens::Bool(_)
        | Tokens::TString(_) | Tokens::Float(_) =>
            true,
        _ => false,
    }
}
/// Converts `tok` to an AST value
/// Requires `tok` is an integer, float, or string token
fn tok_to_val(tok: Tokens) -> Result<Ast, String> {
    match tok {
        Tokens::Integer(x) => Ok(Ast::VInt(x)),
        Tokens::Float(x) => Ok(Ast::VFloat(x)),
        Tokens::TString(x) => Ok(Ast::VStr(x)),
        Tokens::Bool(x) => Ok(Ast::VBool(x)),
        _ => Err(format!("{:?} is not a value", tok)),
    }
}

/// Parses a binary operator
/// `left` is the left child of the operator or None
/// Requires `op` is a binary operator token
fn parse_bop(left: Ast, parent: Option<i32>, stream: &mut VecDeque<Tokens>) 
    -> Result<Ast, String> 
{
    let mut res : Result<Ast, String> = Ok(left);
    while let Some(op) = tok_to_op(stream.front().unwrap_or(&Tokens::RParen)) {
        if let Err(_) = res { break; }
        if let Some(p) = parent {
            if p >= precedence(op) {
                // parent must be lower than new op
                break;
            }
        }
        stream.pop_front().unwrap();
        let right = parse_bop_right(stream, precedence(op));
        res = make_bop(res, op, right);
    }
    res
    

}

/// Constructs a bop `left op right` if both `left` and `right` are `Ok`
/// Otherwise returns one of the two errors
fn make_bop(left: Result<Ast, String>, op: Op, right: Result<Ast, String>)
    -> Result<Ast, String>
{
    match (left, right) {
        (Ok(a), Ok(b)) => 
            Ok(Ast::Bop(Box::new(a), op, Box::new(b))),
        (Err(e), _) | (_, Err(e)) =>
            Err(e),
    }
}

/// Parses the right productions of a BOP (either `L` or `(E)`)
fn parse_bop_right(stream: &mut VecDeque<Tokens>, cur_pres: i32) -> Result<Ast, String> {
    match stream.front() {
        Some(Tokens::LParen) => parse_expr(stream),
        Some(x) if tok_is_val(&x) =>
            parse_bop(
                tok_to_val(stream.pop_front().unwrap()).unwrap(), 
                Some(cur_pres), stream
            ),
        _ => Err("Bop needs right branch".to_owned()),
    }
}
/// Gets the precedence of `op`
/// Larger number indicates higher priority
fn precedence(op: Op) -> i32 {
    match op {
        Op::Exp => 6,
        Op::Mult | Op::Mod | Op::Div | Op::And => 5,
        Op::Plus | Op::Sub | Op::Or => 4,
        Op::Eq | Op::Neq | Op::Leq | Op::Geq | Op::Lt | Op::Gt => 3,
        Op::Land => 2,
        Op::Lor => 1,
    }
}

/// Converts `tok` to an operator
/// Requires `tok` is a bop token
fn tok_to_op(tok: &Tokens) -> Option<Op> {
    match *tok {
        Tokens::OpMult => Some(Op::Mult),
        Tokens::OpDiv => Some(Op::Div),
        Tokens::OpPlus => Some(Op::Plus),
        Tokens::OpMod => Some(Op::Mod),
        Tokens::OpMinus => Some(Op::Sub),
        Tokens::OpExp => Some(Op::Exp),
        Tokens::OpLor => Some(Op::Lor),
        Tokens::OpLand => Some(Op::Land),
        Tokens::OpGeq => Some(Op::Geq),
        Tokens::OpGt => Some(Op::Gt),
        Tokens::OpLt => Some(Op::Lt),
        Tokens::OpLeq => Some(Op::Leq),
        Tokens::OpEq => Some(Op::Eq),
        Tokens::OpNeq => Some(Op::Neq),
        _ => None,
    }
}

/// Parses an arithmetic expression
fn parse_expr(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let left = parse_expr_left(stream);

    match stream.front() {
        Some(x) if tok_is_bop(x) =>
            parse_bop(left.unwrap(), None, stream),
        Some(x) if *x == Tokens::RParen =>
            left,
        None => left,
        x => Err(format!("Unknown token {:?} in expr with left as {:?}", x, left)),
    }
}

/// Parses the (E) or L productions of an expr
fn parse_expr_left(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.pop_front() {
        Some(Tokens::LParen) => parse_paren_expr(stream),
        Some(tok) => tok_to_val(tok),
        _ => Err("Expr missing left branch".to_owned()),
    }
}

/// Parses the (E) productions
/// Requires `tok` is `LParen`
fn parse_paren_expr(stream: &mut VecDeque<Tokens>) -> Result<Ast, String>
{
    let expr = parse_expr(stream);
    if let Ok(ast) = expr {
        match stream.pop_front() {
            Some(Tokens::RParen) => Ok(ast),
            _ => Err("Missing closing parenthesis".to_owned()),
        }
    } else {expr}
}
