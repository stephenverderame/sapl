use crate::lexer::Tokens;
use std::collections::VecDeque;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Op {
    Plus, Mult, Div, Mod, Sub
}

#[derive(PartialEq, Debug)]
pub enum Ast {
    VInt(i32),
    VFloat(f64),
    VStr(String),
    Bop(Box<Ast>, Op, Box<Ast>),
}

pub fn parse(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.pop_front() {
        Some(x @ Tokens::Integer(_)) | Some(x @ Tokens::Float(_))
            => parse_expr(x, stream),
        x => Err("unknown expr".to_owned()),
    }
}

fn tok_is_bop(tok: &Tokens) -> bool {
    match *tok {
        Tokens::OpMod | Tokens::OpDiv | Tokens::OpPlus
        | Tokens::OpMinus | Tokens::OpOr | Tokens::OpExp
        | Tokens::OpLand | Tokens::OpLor | Tokens::OpMult 
        => true,
        _ => false,
    }
}
/// Converts `tok` to an AST value
/// Requires `tok` is an integer, float, or string token
fn tok_to_val(tok: Tokens) -> Ast {
    match tok {
        Tokens::Integer(x) => Ast::VInt(x),
        Tokens::Float(x) => Ast::VFloat(x),
        Tokens::TString(x) => Ast::VStr(x),
        _ => panic!("{:?} is not a value", tok)
    }
}

/// Parses a binary operator
/// `left` is the left child of the operator or None
/// Requires `op` is a binary operator token
fn parse_bop(left: Ast, op: Tokens, stream: &mut VecDeque<Tokens>) 
    -> Result<Ast, String> 
{
    let ast_op = tok_to_op(&op);
    let right = tok_to_val(stream.pop_front()
        .expect("Bop needs right branch"));
    match stream.front() {
        Some(x) if tok_is_bop(x) => {
            let tok2 = stream.pop_front().unwrap();
            let op2 = tok_to_op(&tok2);
            if is_higher_precedence(ast_op, op2) {
                Ok(Ast::Bop(Box::new(left), ast_op, 
                    Box::new(parse_bop(right, tok2, stream)
                    .expect("Bop needs valid right branch"))))
            } else {
                parse_bop(Ast::Bop(Box::new(left), ast_op, Box::new(right)), 
                    tok2, stream)
            }
        },
        _ => Ok(Ast::Bop(Box::new(left), ast_op, Box::new(right)))
    }

}

/// True if `a` has a higher precedence than `b`
/// If true, then `b` should be a child of `a`
fn is_higher_precedence(a: Op, b: Op) -> bool {
    let precedence = |op| -> i32 {
        match op {
            Op::Mult | Op::Mod | Op::Mod | Op::Div => 1,
            Op::Plus | Op::Sub => 0,
        }
    };
    precedence(a) > precedence(b)
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
        _ => panic!("Op unimplemented")
    }
}

/// Parses an arithmetic expression
fn parse_expr(tok: Tokens, stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.pop_front() {
        Some(x) if tok_is_bop(&x) => parse_bop(tok_to_val(tok), x, stream),
        x => Err("Unknown expr".to_owned()),
    }
}