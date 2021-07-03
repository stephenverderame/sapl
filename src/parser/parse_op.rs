use std::collections::VecDeque;
use crate::lexer::Tokens;
use super::parse_expr;
use super::Ast;
use super::Op;
use super::consume;
use super::parse_func;

/// True if `tok` is a binary operator
/// False otherwise
pub fn tok_is_bop(tok: &Tokens) -> bool {
    match *tok {
        Tokens::OpMod | Tokens::OpDiv | Tokens::OpPlus
        | Tokens::OpMinus | Tokens::OpOr | Tokens::OpExp
        | Tokens::OpLand | Tokens::OpLor | Tokens::OpMult 
        | Tokens::OpLeq | Tokens::OpEq | Tokens::OpLt 
        | Tokens::OpNeq | Tokens::OpGeq | Tokens::OpGt
        | Tokens::OpPipeline | Tokens::OpDot 
        | Tokens::OpRange | Tokens::OpConcat
        | Tokens::OpAssign | Tokens::Leftarrow
        | Tokens::As | Tokens::Is
        => true,
        _ => false,
    }
}

/// True if `tok` is a value token
pub fn tok_is_val(tok: &Tokens) -> bool {
    match tok {
        Tokens::Integer(_) | Tokens::Bool(_)
        | Tokens::TString(_) | Tokens::Float(_) 
        | Tokens::None => 
            true,
        _ => false,
    }
}

/// Parses a binary operator
/// `left` is the left child of the operator or None
/// Requires `op` is a binary operator token
pub fn parse_bop(left: Ast, parent: Option<i32>, stream: &mut VecDeque<Tokens>) 
    -> Result<Ast, String> 
{
    let mut res : Result<Ast, String> = Ok(left);
    while let Some(op) = 
        tok_to_op(stream.front().unwrap_or(&Tokens::RParen), false) 
    {
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

/// True if `tok` is a post value unary operator
/// Ie. the index operator `lst[i]`
pub fn tok_is_post_uop(tok: &Tokens) -> bool {
    match *tok {
        Tokens::LBracket | Tokens::OpQ 
        | Tokens::LParen => true,
        _ => false,
    }
}

/// True if `tok` is a pre value unary operator
/// Ie. the negation operator `~val`
pub fn tok_is_pre_uop(tok: &Tokens) -> bool {
    match *tok {
        Tokens::OpNegate | Tokens::Return 
        | Tokens::Throw | Tokens::OpAnd |
        Tokens::OpMinus | Tokens::OpMult 
        | Tokens::OpLand | Tokens::Include => true,
        _ => false,
    }
}

/// Parses a post value uop
/// Requires that the uop is currently the first element in the stream
pub fn parse_post_uop(left: Ast, stream: &mut VecDeque<Tokens>)
    -> Result<Ast, String>
{
    let op =
    match stream.pop_front() {
        Some(Tokens::LBracket) => parse_index_op(left, stream),
        Some(Tokens::OpQ) => Ok(Ast::Uop(Box::new(left), Op::AsBool)),
        Some(Tokens::LParen) => parse_func::parse_fn_apply(left, stream),
        e => Err(format!("Unexpected post UOP token {:?}", e)),
    };
    if op.is_ok() && stream.front() != None && 
            tok_is_post_uop(&stream.front().unwrap()) 
    {
        parse_post_uop(op.unwrap(), stream)
    } else { op }
}

/// Parses a pre value uop
/// Requires the uop is the first element in the stream
pub fn parse_pre_uop(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let op = tok_to_op(stream.front().unwrap(), true).unwrap();
    let prec = precedence(op);
    consume(stream);
    let right = 
    match stream.front() {
        Some(x) if tok_is_pre_uop(x) => parse_expr(stream, Some(prec)),
        Some(x) if tok_is_val(x) => parse_expr(stream, Some(prec)),
        Some(x) if super::tok_is_expr(&x) => parse_expr(stream, Some(prec)),
        x => return Err(format!("Unexpected {:?} after Uop", x)),
    };
    match right {
        Ok(right) => Ok(Ast::Uop(Box::new(right), op)),
        e => e,
    }

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
        Some(t) if super::tok_is_expr(t) => parse_expr(stream, Some(cur_pres)),
        e => Err(format!("Expected right branch of binary op. Got {:?}", e)),
    }
}
/// Gets the precedence of `op`
/// Larger number indicates higher priority
fn precedence(op: Op) -> i32 {
    match op {
        Op::Dot => 13,
        Op::Ref | Op::Deref | Op::MutRef | Op::Include => 12,
        Op::Index | Op::Neg | Op::AsBool | Op::Not => 11,
        Op::As | Op::Is => 10,
        Op::Exp => 9,
        Op::Mult | Op::Mod | Op::Div | Op::And => 8,
        Op::Plus | Op::Sub | Op::Or => 7,
        Op::Eq | Op::Neq | Op::Leq | Op::Geq | Op::Lt | Op::Gt => 6,
        Op::Land => 5,
        Op::Lor => 4,       
        Op::Range | Op::Concat => 3,
        Op::Pipeline => 2,
        Op::Return | Op::Throw => 1,
        Op::Assign | Op::Update => 0,
    }
}

/// Converts `tok` to an operator
/// Requires `tok` is a bop token
/// `uop` - true when the operator is seen as a uop
fn tok_to_op(tok: &Tokens, uop: bool) -> Option<Op> {
    match (tok, uop) {
        (&Tokens::OpMult, false) => Some(Op::Mult),
        (&Tokens::OpMult, true) => Some(Op::Deref),
        (&Tokens::OpDiv, _) => Some(Op::Div),
        (&Tokens::OpPlus, _) => Some(Op::Plus),
        (&Tokens::OpMod, _) => Some(Op::Mod),
        (&Tokens::OpMinus, false) => Some(Op::Sub),
        (&Tokens::OpMinus, true) => Some(Op::Neg),
        (&Tokens::OpExp, _) => Some(Op::Exp),
        (&Tokens::OpLor, _) => Some(Op::Lor),
        (&Tokens::OpLand, false) => Some(Op::Land),
        (&Tokens::OpLand, true) => Some(Op::MutRef),
        (&Tokens::OpAnd, false) => Some(Op::And),
        (&Tokens::OpAnd, true) => Some(Op::Ref),
        (&Tokens::OpGeq, _) => Some(Op::Geq),
        (&Tokens::OpGt, _) => Some(Op::Gt),
        (&Tokens::OpLt, _) => Some(Op::Lt),
        (&Tokens::OpLeq, _) => Some(Op::Leq),
        (&Tokens::OpEq, _) => Some(Op::Eq),
        (&Tokens::OpNeq, _) => Some(Op::Neq),
        (&Tokens::OpPipeline, _) => Some(Op::Pipeline),
        (&Tokens::OpDot, _) => Some(Op::Dot),
        (&Tokens::OpNegate, _) => Some(Op::Not),
        (&Tokens::OpRange, _) => Some(Op::Range),
        (&Tokens::Return, _) => Some(Op::Return),
        (&Tokens::OpConcat, _) => Some(Op::Concat),
        (&Tokens::Throw, _) => Some(Op::Throw),
        (&Tokens::OpAssign, _) => Some(Op::Assign),
        (&Tokens::Leftarrow, _) => Some(Op::Update),
        (&Tokens::As, _) => Some(Op::As),
        (&Tokens::Is, _) => Some(Op::Is),
        (&Tokens::Include, _) => Some(Op::Include),
        _ => None,
    }
}

/// Parses the uop post operator `[]`. Requires the first bracket has been consumed
fn parse_index_op(left: Ast, stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let idx = 
    match parse_expr(stream, None) {
        Ok(idx) => idx,
        e => return e,
    };
    crate::expect!(stream, Tokens::RBracket, "index operator");
    Ok(Ast::Bop(Box::new(left), Op::Index, Box::new(idx)))

}