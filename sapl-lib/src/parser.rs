use crate::lexer::Tokens;
use std::collections::VecDeque;
use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Op {
    Plus, Mult, Div, Mod, Sub, Exp,
    Land, Lor, And, Or, Lt, Gt, Eq,
    Neq, Leq, Geq, Range, Not,
    Pipeline, Dot, Neg, Concat,
    AsBool,
    Index,
    Return, Throw,
    Assign,
    Ref,
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
    Let(Vec<(String, bool)>, Box<Ast>),
    Name(String),
    Func(String, Vec<String>, Box<Ast>, Option<Box<Ast>>),
    Lambda(Vec<String>, Box<Ast>),
    FnApply(String, Vec<Box<Ast>>),
    Array(Vec<Box<Ast>>),
    Tuple(Vec<Box<Ast>>),
    Placeholder,
    Uop(Box<Ast>, Op),
    Map(HashMap<String, Box<Ast>>),
    Try(Box<Ast>, String, Box<Ast>),
    For(Vec<String>, Box<Ast>, Option<Box<Ast>>, Box<Ast>),
    While(Box<Ast>, Box<Ast>),
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
        Ast::If(..) | Ast::Func(..) |
        Ast::Try(..) | Ast::For(..) |
        Ast::While(..) => true,
        _ => false,
    }
}

/// True if `tok` can start an expression
fn tok_is_expr(tok: &Tokens) -> bool {
    match tok {
        Tokens::Integer(_) | Tokens::Float(_)
        | Tokens::Bool(_) | Tokens::TString(_)
        | Tokens::LParen | Tokens::If 
        | Tokens::Name(_) | Tokens::OpQ 
        | Tokens::LBracket | Tokens::OpNegate 
        | Tokens::LBrace | Tokens::Try  =>
        true,
        x if tok_is_pre_uop(x) => true,
        _ => false,
    }
}

fn tok_is_defn(tok: &Tokens) -> bool {
    match *tok {
        Tokens::Let | Tokens::Fun | 
        Tokens::For | Tokens::While => true,
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
        | Tokens::OpRange | Tokens::OpConcat
        | Tokens::OpAssign
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
        Op::Dot => 13,
        Op::Ref => 12,
        Op::Index | Op::Neg | Op::AsBool | Op::Not => 11,
        Op::Exp => 10,
        Op::Mult | Op::Mod | Op::Div | Op::And => 9,
        Op::Plus | Op::Sub | Op::Or => 8,
        Op::Eq | Op::Neq | Op::Leq | Op::Geq | Op::Lt | Op::Gt => 7,
        Op::Land => 6,
        Op::Lor => 5,       
        Op::Range | Op::Concat => 4,
        Op::Pipeline => 3,
        Op::Return | Op::Throw => 2,
        Op::Assign => 1,
    }
}

/// Converts `tok` to an operator
/// Requires `tok` is a bop token
/// `uop` - true when the operator is seen as a uop
fn tok_to_op(tok: &Tokens, uop: bool) -> Option<Op> {
    match (tok, uop) {
        (&Tokens::OpMult, _) => Some(Op::Mult),
        (&Tokens::OpDiv, _) => Some(Op::Div),
        (&Tokens::OpPlus, _) => Some(Op::Plus),
        (&Tokens::OpMod, _) => Some(Op::Mod),
        (&Tokens::OpMinus, false) => Some(Op::Sub),
        (&Tokens::OpMinus, true) => Some(Op::Neg),
        (&Tokens::OpExp, _) => Some(Op::Exp),
        (&Tokens::OpLor, _) => Some(Op::Lor),
        (&Tokens::OpLand, _) => Some(Op::Land),
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
        _ => left,
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
        Some(Tokens::LBracket) => parse_array(consume(stream)),
        Some(Tokens::LBrace) => parse_map(consume(stream)),
        Some(Tokens::Fun) => parse_func(consume(stream)),
        Some(Tokens::Try) => parse_try(consume(stream)),
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
            Some(Tokens::Comma) => parse_tuple(ast, stream),
            _ => Err("Missing closing parenthesis".to_owned()),
        }
    } else {expr}
}

fn parse_try(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let body = parse_block(stream, false, true);
    if Some(Tokens::Catch) == stream.pop_front() {
        if let Some(Tokens::Name(x)) = stream.pop_front() {
            let catch = parse_block(stream, true, false);
            match (body, catch) {
                (Ok(body), Ok(catch)) => Ok(Ast::Try(Box::new(body), x, Box::new(catch))),
                (e @ Err(_), Ok(_)) | (Ok(_), e @ Err(_)) => e,
                (e, _) => e,
            }
        } else {
            Err("Missing catch variable".to_owned())
        }
    } else {
        Err("Missing catch block to try".to_owned())
    }
}

/// Parses a block that can start with a colon or be wrapped in braces
/// `only_expr_wo_brace` - set to true if the block can only contain an expression if
/// there are no braces
/// `can_omit` - set to true if the block can omit : or {}
/// Requires the block begins with the first token in `stream`.
fn parse_block(stream: &mut VecDeque<Tokens>, 
    only_expr_wo_brace: bool, can_omit : bool) -> Result<Ast, String> 
{
    let is_brace = stream.front() == Some(&Tokens::LBrace);
    let is_colon = stream.front() == Some(&Tokens::Colon);
    if is_brace || is_colon || can_omit {
        if is_brace || is_colon { consume(stream); }
        let body = if only_expr_wo_brace && !is_brace {
            parse_expr(stream, None)
        } else {
            parse(stream)
        };
        if is_brace && stream.pop_front() != Some(Tokens::RBrace) { 
            Err("Missing brace in block".to_owned())
        } else {
            body
        }
    } else {
        Err("Missing colon or left brace in block".to_owned())
    }
}

fn parse_tuple(first: Ast, stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let mut elems = vec![Box::new(first)];
    loop {
        match parse_expr(stream, None) {
            Ok(ast) => {
                elems.push(Box::new(ast));
                if stream.front() != Some(&Tokens::Comma) { break; }
            },
            e => return e,
        }
        stream.pop_front();
    }
    if stream.pop_front() != Some(Tokens::RParen) {
        Err("Unknown token after tuple. Expected )".to_owned())
    } else {
        Ok(Ast::Tuple(elems))
    }
}

/// Parses an `If`
/// Requires the `If` token has already been consumed
fn parse_conditional(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let guard = parse_expr(stream, None);
    match (guard, parse_block(stream, false, false)) {
        (Ok(guard), Ok(body)) => {
            println!("If got guard {:?} and body {:?}", guard, body);
            let gd = Box::new(guard);
            let bd = Box::new(body);
            if stream.front() == Some(&Tokens::Else) {
                match parse_block(consume(stream), true, true) {
                    Ok(else_blk) => Ok(Ast::If(gd, bd, Some(Box::new(else_blk)))),
                    e => e,
                }
            } else {
                Ok(Ast::If(gd, bd, None))
            }
        },
        (e @ Err(_), Ok(_)) | (Ok(_), e @ Err(_)) => e,
        (e, _) => e,
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

/// Parses the next token sequence as a definition
/// If the next token is not a definition, the stream is unchanged and
/// an error is returned
fn parse_defn(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.front() {
        Some(Tokens::Let) => parse_let(consume(stream)),
        Some(Tokens::Fun) => parse_func(consume(stream)),
        Some(Tokens::For) => parse_for_loop(consume(stream)),
        Some(Tokens::While) => parse_while_loop(consume(stream)),
        _ => Err("Not a defn".to_owned()),
    }
}

/// Parses a let definition
/// Requires `let` has already been consumed
fn parse_let(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    if let Some(names) = parse_comma_sep_names(stream) {
        if stream.pop_front() != Some(Tokens::OpAssign) { 
            return Err("Missing '=' in let defn".to_owned())
        }
        let val = parse_expr(stream, None);
        if val.is_err() { return val; }
        Ok(Ast::Let(names, Box::new(val.unwrap())))
    } else {
        Err("Missing constant name(s) in let".to_owned())
    }
}

/// Parses stream for names separated by commas
/// Requires the first name is the first token in the stream
/// If no names are found, does not mutate stream
/// Returns an array of name, mutability pairs
fn parse_comma_sep_names(stream: &mut VecDeque<Tokens>) -> Option<Vec<(String, bool)>> {
    let is_var = stream.front() == Some(&Tokens::Var);
    if is_var {consume(stream);}
    if let Some(Tokens::Name(nm)) = stream.front() {       
        let mut names = vec![(nm.to_string(), is_var)];
        consume(stream);

        while stream.front() == Some(&Tokens::Comma) {
            stream.pop_front();
            let is_var = stream.front() == Some(&Tokens::Var);
            if is_var { consume(stream); }
            if let Some(Tokens::Name(nm)) = stream.pop_front() {
                names.push((nm, is_var))
            } else {
                return None
            }
        }
        Some(names)
    } else { None }
}

/// Parses a function definition or expression
/// Requires `fun` has already been consumed
fn parse_func(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.pop_front() {
        Some(Tokens::Name(fn_name)) => {
            let params = get_function_params(stream);
            match get_function_body(stream) {
                (Ok(body), post) => Ok(Ast::Func(fn_name, params, Box::new(body), post)),
                (e, _) => e,
            }
        },
        Some(Tokens::LParen) => {
            let params = get_function_params(stream);
            if stream.pop_front() != Some(Tokens::RParen) {
                Err("Lambda missing closing parenthesis".to_owned())
            } else {
                match get_function_body(stream) {
                    (Ok(body), _) => Ok(Ast::Lambda(params, Box::new(body))),
                    (e, _) => e,
                }
            }
        },
        x => Err(format!("Unexpected token {:?} in function definition", x))
    } 
}

/// Parses a function's parameters
/// Requires the first name is the first token in the stream
fn get_function_params(stream: &mut VecDeque<Tokens>) -> Vec<String> {
    let mut params = Vec::<String>::new();
    while let Some(Tokens::Name(_)) = stream.front() {
        if let Some(Tokens::Name(param)) = stream.pop_front() {
            params.push(param);
        } else { panic!("WTF"); }
    }
    params
}

/// Parses a function body and postcondition if there is one
/// `need_brace`: true if the function body must be surrounded by braces
/// Gets the body, postcondition pair
/// If there is a postcondition, braces are required
fn get_function_body(stream: &mut VecDeque<Tokens>) -> (Result<Ast, String>, Option<Box<Ast>>)
{
    let postcondition =
    if Some(&Tokens::Rightarrow) == stream.front() {
        consume(stream);
        match parse_expr(stream, None) {
            Ok(ast) => Some(Box::new(ast)),
            e => return (e, None),
        }
    } else { None };
    (parse_block(stream, true, true), postcondition)
}

/// Parses a series of tokens within a `[]` as a list
/// Requires the first `[` has been consumed
fn parse_array(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
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
        Ok(Ast::Array(lst))
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
        Tokens::OpNegate | Tokens::Return 
        | Tokens::Throw | Tokens::OpAnd |
        Tokens::OpMinus => true,
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
    let op = tok_to_op(stream.front().unwrap(), true).unwrap();
    consume(stream);
    let right = 
    match stream.front() {
        Some(Tokens::LParen) | Some(Tokens::Name(_)) => parse_expr(stream, None),
        Some(x) if tok_is_pre_uop(x) => parse_expr(stream, None),
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

/// Parses a map
/// Requires the first brace has been consumed
fn parse_map(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let mut map = HashMap::<String, Box<Ast>>::new();
    loop {
        match stream.pop_front() {
            Some(Tokens::Name(x)) | Some(Tokens::TString(x)) => {
                if Some(Tokens::Colon) != stream.pop_front() { 
                    return Err("Map missing colon after name".to_owned());
                }
                match parse_expr(stream, None) {
                    Ok(val) => map.insert(x, Box::new(val)),
                    e => return e,
                };
                if Some(&Tokens::Comma) != stream.front() { break; }
                else { consume(stream); }
            },
            Some(Tokens::RBrace) => return Ok(Ast::Map(map)),
            x => return Err(format!("Unexpected token {:?} when parsing map", x)),
        }
    }
    if Some(Tokens::RBrace) != stream.pop_front() {
        Err("Map missing closing brace".to_owned())
    } else {
        Ok(Ast::Map(map))
    }
}

fn parse_for_loop(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    if let Some(names) = parse_comma_sep_names(stream) {
        if stream.pop_front() != Some(Tokens::In) { 
            return Err("For loop missing in".to_owned())
        }

        let iter =
        match parse_expr(stream, None) {
            e @ Err(_) => return e,
            Ok(ast) => Box::new(ast)
        };

        let if_expr = 
        if Some(&Tokens::If) == stream.front() {
            consume(stream);
            match parse_expr(stream, None) {
                e @ Err(_) => return e,
                Ok(ast) => Some(Box::new(ast)),
            }
        } else { None };

        let body = 
        match parse_block(stream, false, false) {
            e @ Err(_) => return e,
            Ok(ast) => Box::new(ast),
        };

        let mut v = Vec::<String>::new();
        for (nm, _) in names.into_iter() {
            v.push(nm);
        }
        Ok(Ast::For(v, iter, if_expr, body))
    } else { Err("For missing names".to_owned()) }
}

fn parse_while_loop(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    if let Ok(expr) = parse_expr(stream, None) {
        match parse_block(stream, false, false) {
            Ok(body) => Ok(Ast::While(Box::new(expr), Box::new(body))),
            e => e,
        }
    } else {
        Err("While loop missing condition".to_owned())
    }
}
