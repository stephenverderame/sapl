use std::collections::VecDeque;
use crate::lexer::Tokens;
use super::parse_expr;
use super::Ast;
use super::consume;
use super::parse_block;

pub fn parse_for_loop(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
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

pub fn parse_while_loop(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    if let Ok(expr) = parse_expr(stream, None) {
        match parse_block(stream, false, false) {
            Ok(body) => Ok(Ast::While(Box::new(expr), Box::new(body))),
            e => e,
        }
    } else {
        Err("While loop missing condition".to_owned())
    }
}

/// Parses an `If`
/// Requires the `If` token has already been consumed
pub fn parse_conditional(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
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

pub fn parse_try(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
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

/// Parses a sequence following `ast`
/// If `ast` is not part of a sequence, returns `ast`
/// If any ast in the sequence fails to parse, returns that error
pub fn parse_seq(ast: Result<Ast, String>, stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    if ast.is_err() || stream.front() == None { return ast; }
    let mut vec = vec![Box::new(ast.unwrap())];
    let mut last = vec.last().unwrap();
    while can_elide_seq(last) || stream.front() == Some(&Tokens::Seq) {
        let is_seq_token = stream.front() == Some(&Tokens::Seq);
        if is_seq_token { consume(stream); }   
        match super::parse_single(stream) {
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

/// Parses a let definition
/// Requires `let` has already been consumed
pub fn parse_let(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
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

/// True if `ast` can be in a sequence without a `;` following it
fn can_elide_seq(ast: &Ast) -> bool {
    match ast {
        Ast::If(..) | Ast::Func(..) |
        Ast::Try(..) | Ast::For(..) |
        Ast::While(..) | Ast::Struct(..) => true,
        _ => false,
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