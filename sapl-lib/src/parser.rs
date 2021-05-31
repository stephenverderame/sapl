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
    match stream.pop_front() {
        Some(x @ Tokens::Integer(_)) | Some(x @ Tokens::Float(_))
        | Some(x @ Tokens::TString(_)) | Some(x @ Tokens::LParen)
            => parse_expr(x, stream),
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
fn parse_bop(left: Ast, op: Tokens, stream: &mut VecDeque<Tokens>) 
    -> Result<Ast, String> 
{
    let ast_op = tok_to_op(&op);
    match parse_bop_right(stream) {
        Ok(ast) => 
            parse_bop_continuation(left, ast_op, ast, stream),
        err => err,
    }
    

}

/// Parses the right productions of a BOP (either `L` or `(E)`)
fn parse_bop_right(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.pop_front() {
        Some(Tokens::LParen) => parse_expr(Tokens::LParen, stream),
        Some(x) => tok_to_val(x),
        _ => Err("Bop needs right branch".to_owned()),
    }
}

/// Checks if the BOP continues (more bops) and if so extends the Ast up or down
/// depending on the precedence between `op` and the next op
/// If Bop ends, returns `Bop(left, op, right)`
fn parse_bop_continuation(left: Ast, op: Op, right: Ast, stream: &mut VecDeque<Tokens>) 
    -> Result<Ast, String> 
{
    match stream.front() {
        Some(x) if tok_is_bop(x) => {
            let tok2 = stream.pop_front().unwrap(); // cannot panic
            let op2 = tok_to_op(&tok2);
            if is_lower_precedence(op, op2) {
                let new_right = parse_bop(right, tok2, stream);
                if let Ok(bop) = new_right {
                    Ok(Ast::Bop(Box::new(left), op, Box::new(bop)))
                } else {new_right}
            } else {
                parse_bop(Ast::Bop(Box::new(left), op, Box::new(right)), 
                    tok2, stream)
            }
        },
        _ => Ok(Ast::Bop(Box::new(left), op, Box::new(right)))
    }
}

/// True if `a` has a higher precedence than `b`
/// If true, then `b` should be a child of `a`
fn is_lower_precedence(a: Op, b: Op) -> bool {
    let precedence = |op| -> i32 {
        match op {
            Op::Exp => 6,
            Op::Mult | Op::Mod | Op::Div | Op::And => 5,
            Op::Plus | Op::Sub | Op::Or => 4,
            Op::Eq | Op::Neq | Op::Leq | Op::Geq | Op::Lt | Op::Gt => 3,
            Op::Land => 2,
            Op::Lor => 1,
        }
    };
    precedence(a) < precedence(b)
}

/// Converts `tok` to an operator
/// Requires `tok` is a bop token
fn tok_to_op(tok: &Tokens) -> Op {
    match *tok {
        Tokens::OpMult => Op::Mult,
        Tokens::OpDiv => Op::Div,
        Tokens::OpPlus => Op::Plus,
        Tokens::OpMod => Op::Mod,
        Tokens::OpMinus => Op::Sub,
        Tokens::OpExp => Op::Exp,
        Tokens::OpLor => Op::Lor,
        Tokens::OpLand => Op::Land,
        Tokens::OpGeq => Op::Geq,
        Tokens::OpGt => Op::Gt,
        Tokens::OpLt => Op::Lt,
        Tokens::OpLeq => Op::Leq,
        Tokens::OpEq => Op::Eq,
        Tokens::OpNeq => Op::Neq,
        _ => panic!("Op unimplemented")
    }
}

/// Parses an arithmetic expression
fn parse_expr(tok: Tokens, stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let left = parse_expr_left(tok, stream);

    match stream.front() {
        Some(x) if tok_is_bop(x) =>
            parse_bop(left.unwrap(), stream.pop_front().unwrap(), stream),
        Some(x) if *x == Tokens::RParen =>
            left,
        None => left,
        x => Err(format!("Unknown token {:?} in expr with left as {:?}", x, left)),
    }
}

/// Parses the (E) or L productions of an expr
fn parse_expr_left(tok: Tokens, stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match tok {
        Tokens::LParen => parse_paren_expr(stream),
        _ => tok_to_val(tok),
    }
}

/// Parses the (E) productions
/// Requires `tok` is `LParen`
fn parse_paren_expr(stream: &mut VecDeque<Tokens>) -> Result<Ast, String>
{
    let expr = parse_expr(stream.pop_front().unwrap(), stream);
    if let Ok(ast) = expr {
        match stream.pop_front() {
            Some(Tokens::RParen) => Ok(ast),
            _ => Err("Missing closing parenthesis".to_owned()),
        }
    } else {expr}
}
