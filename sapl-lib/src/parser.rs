use crate::lexer::Tokens;
use std::collections::VecDeque;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Op {
    Plus, Mult, Div, Mod, Sub, Exp,
    Land, Lor, And, Or,
}

#[derive(PartialEq, Debug)]
pub enum Ast {
    VInt(i32),
    VFloat(f64),
    VStr(String),
    Bop(Box<Ast>, Op, Box<Ast>),
}

/// Parses a stream of tokens `stream` into an Abstract Syntax Tree
pub fn parse(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.pop_front() {
        Some(x @ Tokens::Integer(_)) | Some(x @ Tokens::Float(_))
        | Some(x @ Tokens::TString(_)) | Some(x @ Tokens::LParen)
            => parse_expr(x, stream),
        x => Err("unknown parse".to_owned()),
    }
}

/// True if `tok` is a binary operator
/// False otherwise
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
fn tok_to_val(tok: Tokens) -> Result<Ast, String> {
    match tok {
        Tokens::Integer(x) => Ok(Ast::VInt(x)),
        Tokens::Float(x) => Ok(Ast::VFloat(x)),
        Tokens::TString(x) => Ok(Ast::VStr(x)),
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
    let right = match stream.pop_front() {
        Some(Tokens::LParen) => parse_expr(Tokens::LParen, stream).unwrap(),
        Some(x) => tok_to_val(x).unwrap(),
        _ => panic!("Bop needs right branch!"),
    };
    match stream.front() {
        Some(x) if tok_is_bop(x) => {
            let tok2 = stream.pop_front().unwrap();
            let op2 = tok_to_op(&tok2);
            if is_lower_precedence(ast_op, op2) {
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
fn is_lower_precedence(a: Op, b: Op) -> bool {
    let precedence = |op| -> i32 {
        match op {
            Op::Exp => 6,
            Op::Mult | Op::Mod | Op::Div | Op::And => 5,
            Op::Plus | Op::Sub | Op::Or => 4,
            Op::Land => 1,
            Op::Lor => 0,
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
        _ => panic!("Op unimplemented")
    }
}

/// Parses an arithmetic expression
fn parse_expr(tok: Tokens, stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let left_tree = 
    if let Tokens::LParen = tok {
        // (E) production
        let expr = parse_expr(stream.pop_front().unwrap(), stream);
        if let Ok(ast) = expr {
            match stream.pop_front() {
                Some(Tokens::RParen) => Some(Ok(ast)),
                _ => return Err("Missing closing parenthesis".to_owned()),
            }
        } else {Some(expr)}
    } else {None};

    let left = if left_tree.is_some() { left_tree.unwrap() }
    else { tok_to_val(tok) };

    match stream.pop_front() {
        Some(x) if tok_is_bop(&x) =>
            parse_bop(left.unwrap(), x, stream),
            // L op E production
        None => left, // L production
        _ => Err("Unknown expr".to_owned()),
    }
}