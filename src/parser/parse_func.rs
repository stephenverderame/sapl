use std::collections::VecDeque;
use crate::lexer::Tokens;
use super::parse_expr;
use super::Ast;
use super::consume;
use super::parse_block;

/// Parses a function definition or expression
/// Requires `fun` has already been consumed
pub fn parse_func(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
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

/// Parses the next token on the stream as a name
pub fn parse_name(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.pop_front() {
        Some(Tokens::Name(name)) => Ok(Ast::Name(name)),
        e => Err(format!("Expected name. Got {:?}", e)),
    }
}

/// Parses a function's parameters
/// Requires the first name is the first token in the stream
fn get_function_params(stream: &mut VecDeque<Tokens>) -> Vec<(String, bool, Option<Box<Ast>>)> {
    let mut params = Vec::<(String, bool, Option<Box<Ast>>)>::new();
    loop {
        match stream.front() {
            Some(&Tokens::Name(_)) | Some(&Tokens::Var) => {
                let p = get_param(stream);
                params.push((p.0, p.1, None))
            },
            Some(&Tokens::LParen) => {
                let (name, is_mut) = get_param(consume(stream));
                params.push((name, is_mut, Some(get_precondition(stream).unwrap())))
            },
            Some(&Tokens::LBrace) | Some(&Tokens::RParen) | Some(&Tokens::Rightarrow) => break,
            e => {
                println!("Parse Warning: got token {:?} when parsing argument list. {}",
                e,  "Stopping parse but should have encountered a `)`, `{`, or `->`");
                break
            }
        }
    }
    params
}

/// Gets a name, mutability pair of a argument which is the first in the token stream
fn get_param(stream: &mut VecDeque<Tokens>) -> (String, bool) {
    match stream.front() {
        Some(Tokens::Name(_)) => (get_name(stream), false),
        Some(Tokens::Var) => (get_name(consume(stream)), true),
        x => panic!("Expected name or var in function parameter list. Got {:?}", x),
    }
}

/// Requires a name is the first token in the stream
fn get_name(stream: &mut VecDeque<Tokens>) -> String {
    if let Some(Tokens::Name(name)) = stream.pop_front() {
        name
    } else {
        panic!("Expected argument name")
    }
}

/// Gets an argument precondition annotation
/// Requires the annotation's colon be the first token in the stream
fn get_precondition(stream: &mut VecDeque<Tokens>) -> Result<Box<Ast>, String> {
    crate::expect!(stream, Tokens::Colon);
    let res = match super::parse_expr(stream, None) {
        Ok(ast) => Box::new(ast),
        e => panic!("Error parsing function precondition annotation. {:?}", e),
    };
    crate::expect!(stream, Tokens::RParen);
    Ok(res)
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


/// Parses a function application on the AST `func`
/// Requires the firist parenthesis has been consumed
pub fn parse_fn_apply(func: Ast, stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {

    let mut args = Vec::<Box<Ast>>::new();
    while let Ok(ast) = parse_expr(stream, None) {
        args.push(Box::new(ast));
        if stream.front() != Some(&Tokens::Comma) {
            break;
        }
        stream.pop_front();
    }
    match stream.pop_front() {
        Some(Tokens::RParen) => Ok(Ast::FnApply(Box::new(func), args)),
        e => 
            Err(format!("Missing closing parenthesis in function app. Got {:?}", e)),
    }
}