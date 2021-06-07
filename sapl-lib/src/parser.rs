use crate::lexer::Tokens;
use std::collections::VecDeque;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Op {
    Plus, Mult, Div, Mod, Sub, Exp,
    Land, Lor, And, Or, Lt, Gt, Eq,
    Neq, Leq, Geq, Range,
    Pipeline, Dot, Neg,
    AsBool,
    Index,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Ast {
    VInt(i32),
    VFloat(f64),
    VStr(String),
    VBool(bool),
    Bop(Box<Ast>, Op, Box<Ast>),
    If(Box<Ast>, Box<Ast>, Option<Box<Ast>>),
    Seq(Vec<Box<Ast>>),
    Let(String, Box<Ast>),
    Name(String),
    Func(String, Vec<String>, Box<Ast>),
    FnApply(String, Vec<Box<Ast>>),
    List(Vec<Box<Ast>>),
    Placeholder,
    Uop(Box<Ast>, Op),
}

/// Parses a stream of tokens `stream` into an Abstract Syntax Tree
pub fn parse(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    parse_seq(parse_single(stream), stream)
}

/// Parses a single expression or definition
fn parse_single(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.front() {
        Some(x) if tok_is_expr(&x) => 
            parse_expr(stream, None),
        Some(x) if tok_is_defn(&x) =>
            parse_defn(stream),
        x => Err(format!("Expected expression, got {:?}", x)),
    }
}

/// Parses a sequence following `ast`
/// If `ast` is not part of a sequence, returns `ast`
/// If any ast in the sequence fails to parse, returns that error
fn parse_seq(ast: Result<Ast, String>, stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    if ast.is_err() || stream.front() == None { return ast; }
    let mut vec = vec![Box::new(ast.unwrap())];
    let mut last = vec.last().unwrap();
    while can_elide_seq(last) || stream.front() == Some(&Tokens::Seq) {
        let is_seq_token = stream.front() == Some(&Tokens::Seq);
        if is_seq_token { consume(stream); }   
        match parse_single(stream) {
            Ok(ast) => {
                vec.push(Box::new(ast));
                last = vec.last().unwrap();
            },
            e if is_seq_token => return e,
            _ => break,
        }
    }
    if vec.len() == 1 {
        Ok(*(vec.pop().unwrap()))
    } else {
        Ok(Ast::Seq(vec))
    }
}

/// True if `ast` can be in a sequence without a `;` following it
fn can_elide_seq(ast: &Ast) -> bool {
    match ast {
        Ast::If(..) |
        Ast::Func(..) => true,
        _ => false,
    }
}

/// True if `tok` can start an expression
fn tok_is_expr(tok: &Tokens) -> bool {
    match *tok {
        Tokens::Integer(_) | Tokens::Float(_)
        | Tokens::Bool(_) | Tokens::TString(_)
        | Tokens::LParen | Tokens::If 
        | Tokens::Name(_) | Tokens::OpQ 
        | Tokens::LBracket | Tokens::OpNegate =>
        true,
        _ => false,
    }
}

fn tok_is_defn(tok: &Tokens) -> bool {
    match *tok {
        Tokens::Let | Tokens::Fun => true,
        _ => false,
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
        | Tokens::OpPipeline | Tokens::OpDot 
        | Tokens::OpRange
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
        // a name may or may not be a value, so it must be parsed via
        // parse expr
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
        Tokens::Name(x) => Ok(Ast::Name(x)),
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
         Some(_) => parse_expr(stream, Some(cur_pres)),
        _ => Err("Bop needs right branch".to_owned()),
    }
}
/// Gets the precedence of `op`
/// Larger number indicates higher priority
fn precedence(op: Op) -> i32 {
    match op {
        Op::Index | Op::Neg | Op::AsBool => 11,
        Op::Exp => 10,
        Op::Mult | Op::Mod | Op::Div | Op::And => 9,
        Op::Plus | Op::Sub | Op::Or => 8,
        Op::Eq | Op::Neq | Op::Leq | Op::Geq | Op::Lt | Op::Gt => 7,
        Op::Land => 6,
        Op::Lor => 5,
        Op::Dot => 4,
        Op::Pipeline => 3,
        Op::Range => 2,
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
        Tokens::OpPipeline => Some(Op::Pipeline),
        Tokens::OpDot => Some(Op::Dot),
        Tokens::OpNegate => Some(Op::Neg),
        Tokens::OpRange => Some(Op::Range),
        _ => None,
    }
}

/// Parses an arithmetic expression 
/// If it cannot parse the next token sequence, an error is returned and the
/// stream is unchanged
/// `parent_precedence` is the precedence of the parent Bop or `None` if this is not a sub-expression of
/// a bop
fn parse_expr(stream: &mut VecDeque<Tokens>, parent_precedence: Option<i32>) -> Result<Ast, String> {
    let left = parse_expr_left(stream);
    if left.is_err() { return left; }
    match stream.front() {
        Some(x) if tok_is_bop(x) =>
            parse_bop(left.unwrap(), parent_precedence, stream),
        Some(x) if tok_is_post_uop(x) =>
            match parse_post_uop(left.unwrap(), stream) {
                Ok(ast) => parse_bop(ast, parent_precedence, stream),
                e => e,
            }
        Some(_) =>
        //if *x == Tokens::RParen =>
            left,
        None => left,
        //x => Err(format!("Unknown token {:?} in expr with left as {:?}", x, left)),
    }
}

/// Parses the (E) or L productions of an expr
/// If the next token is not a left expression, returns an error
/// without modifying stream
fn parse_expr_left(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.front() {
        Some(Tokens::LParen) => parse_paren_expr(consume(stream)),
        Some(Tokens::If) => parse_conditional(consume(stream)),
        Some(Tokens::Name(_)) => parse_name(stream),
        Some(Tokens::OpQ) => {
            consume(stream);
            Ok(Ast::Placeholder)
        },
        Some(Tokens::LBracket) => parse_list(consume(stream)),
        Some(tok) if tok_is_pre_uop(tok) => parse_pre_uop(stream),
        Some(tok) if tok_is_val(tok) => tok_to_val(stream.pop_front().unwrap()),
        t => Err(format!("Unexpected {:?} in expr left branch", t)),
    }
}

/// Parses the (E) productions
/// Requires `(` has already been consumed
fn parse_paren_expr(stream: &mut VecDeque<Tokens>) -> Result<Ast, String>
{
    let expr = parse_expr(stream, None);
    if let Ok(ast) = expr {
        match stream.pop_front() {
            Some(Tokens::RParen) => Ok(ast),
            _ => Err("Missing closing parenthesis".to_owned()),
        }
    } else {expr}
}

/// Parses an `If`
/// Requires the `If` token has already been consumed
fn parse_conditional(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let guard = parse_expr(stream, None);
    match (guard, stream.pop_front()) {
        (Ok(guard), Some(tok @ Tokens::Colon)) |
        (Ok(guard), Some(tok @ Tokens::LBrace)) => 
            parse_if_body(guard, tok, stream),
        (Ok(_), t) => Err(format!("Got {:?} instead of colon or bracket after if", t)),
        (e @ Err(_), _) => e,
    } 
}

/// Parses the next token on the stream as a name
fn parse_name(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    if let Some(Tokens::Name(name)) = stream.pop_front() {
        match stream.front() {
            Some(Tokens::LParen) => parse_fn_apply(name, consume(stream)),
            _ => Ok(Ast::Name(name)),
        }
    } else { Err("Missing name".to_owned()) }
}

/// Parses a function application of the function `func`
/// Requires `func(` has been consumed from the stream
fn parse_fn_apply(func: String, stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {

    let mut args = Vec::<Box<Ast>>::new();
    while let Ok(ast) = parse_expr(stream, None) {
        args.push(Box::new(ast));
        if stream.front() != Some(&Tokens::Comma) {
            break;
        }
        stream.pop_front();
    }
    if Some(Tokens::RParen) != stream.pop_front() {
        Err("Missing closing parenthesis in function app".to_owned())
    } else {
        Ok(Ast::FnApply(func, args))
    }
}

/// Parses the body of an if expression
/// `guard` is the guard expression
/// `opening` is the opening token of the body. Either `:` or `{`
/// `stream` is the token stream
fn parse_if_body(guard: Ast, opening: Tokens, stream: &mut VecDeque<Tokens>) 
    -> Result<Ast, String> 
{
    let body = parse(stream);
    if opening == Tokens::LBrace 
        && stream.pop_front() != Some(Tokens::RBrace) 
    {
        return Err("Missing closing } after bracketed if".to_owned());
    }
    if body.is_err() { return body; }
    let body = body.unwrap();
    match stream.front() {
        Some(Tokens::Else) => {
            stream.pop_front();
            let braces = if let Some(Tokens::LBrace) = stream.front() { 
                stream.pop_front(); 
                true
            } else { false };
            let other = parse_expr(stream, None);
            if braces && stream.pop_front() != Some(Tokens::RBrace) {
                return Err("Missing } after else".to_owned());
            }
            if other.is_ok() {
                Ok(Ast::If(Box::new(guard), Box::new(body), 
                    Some(Box::new(other.unwrap()))))
            } else { other }
        },
        _ => Ok(Ast::If(Box::new(guard), Box::new(body), None)),
    }
}

/// Parses the next token sequence as a definition
/// If the next token is not a definition, the stream is unchanged and
/// an error is returned
fn parse_defn(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.front() {
        Some(Tokens::Let) => parse_let(consume(stream)),
        Some(Tokens::Fun) => parse_func(consume(stream)),
        _ => Err("Not a defn".to_owned()),
    }
}

/// Parses a let definition
/// Requires `let` has already been consumed
fn parse_let(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    if let Some(Tokens::Name(nm)) = stream.pop_front() {
        if stream.pop_front() != Some(Tokens::OpAssign) { 
            return Err("Missing '=' in let defn".to_owned())
        }
        let val = parse_expr(stream, None);
        if val.is_err() { return val; }
        Ok(Ast::Let(nm, Box::new(val.unwrap())))
    } else {
        Err("Missing valid identifier name in let".to_owned())
    }
}

/// Parses a function definition
/// Requires `fun` has already been consumed
fn parse_func(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    if let Some(Tokens::Name(fn_name)) = stream.pop_front() {
        let mut params = Vec::<String>::new();
        while let Some(Tokens::Name(_)) = stream.front() {
            if let Some(Tokens::Name(param)) = stream.pop_front() {
                params.push(param);
            } else { panic!("WTF"); }
        }
        if Some(Tokens::LBrace) != stream.pop_front() {
            return Err("Missing brace".to_owned())
        }
        let expr = parse(stream);
        if Some(Tokens::RBrace) != stream.pop_front() {
            Err("Missing closing brace".to_owned())
        } else if expr.is_err() {
            expr
        } else {
            Ok(Ast::Func(fn_name, params, Box::new(expr.unwrap())))
        }

    } else {
        Err("Missing function name".to_owned())
    }
}

/// Parses a series of tokens within a `[]` as a list
/// Requires the first `[` has been consumed
fn parse_list(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let mut lst = Vec::<Box<Ast>>::new();
    loop {
        match parse_expr(stream, None) {
            Ok(expr) => lst.push(Box::new(expr)),
            _ => break,
        }
        if stream.front() == Some(&Tokens::Comma) {
            consume(stream);
        } else {
            break;
        }
    }
    if stream.pop_front() != Some(Tokens::RBracket) {
        Err("List missing ending bracket".to_owned())
    } else {
        Ok(Ast::List(lst))
    }
}

/// True if `tok` is a post value unary operator
/// Ie. the index operator `lst[i]`
fn tok_is_post_uop(tok: &Tokens) -> bool {
    match *tok {
        Tokens::LBracket | Tokens::OpQ => true,
        _ => false,
    }
}

/// True if `tok` is a pre value unary operator
/// Ie. the negation operator `~val`
fn tok_is_pre_uop(tok: &Tokens) -> bool {
    match *tok {
        Tokens::OpNegate => true,
        _ => false,
    }
}

/// Parses a post value uop
/// Requires that the uop is currently the first element in the stream
fn parse_post_uop(left: Ast, stream: &mut VecDeque<Tokens>)
    -> Result<Ast, String>
{
    match stream.pop_front() {
        Some(Tokens::LBracket) => parse_index_op(left, stream),
        Some(Tokens::OpQ) => Ok(Ast::Uop(Box::new(left), Op::AsBool)),
        e => Err(format!("Unexpected post UOP token {:?}", e)),
    }
}

/// Parses a pre value uop
/// Requires the uop is the first element in the stream
fn parse_pre_uop(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let op = tok_to_op(stream.front().unwrap()).unwrap();
    consume(stream);
    let right = 
    match stream.front() {
        Some(Tokens::LParen) | Some(Tokens::Name(_)) => parse_expr(stream, None),
        Some(x) if tok_is_val(x) => parse_expr(stream, None),
        x => return Err(format!("Unexpected {:?} after Uop", x)),
    };
    match right {
        Ok(right) => Ok(Ast::Uop(Box::new(right), op)),
        e => e,
    }

}

/// Parses the uop post operator `[]`. Requires the first bracket has been consumed
fn parse_index_op(left: Ast, stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let idx = 
    match parse_expr(stream, None) {
        Ok(idx) => idx,
        e => return e,
    };
    if stream.pop_front() != Some(Tokens::RBracket) {
        Err("Missing right bracket in index operation".to_owned())
    } else {
        Ok(Ast::Bop(Box::new(left), Op::Index, Box::new(idx)))
    }
}

/// Unconditionally pops the front of `stream` and returns `stream`
fn consume(stream: &mut VecDeque<Tokens>) -> &mut VecDeque<Tokens> {
    stream.pop_front();
    stream
}
