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
    if let Some(Tokens::Name(name)) = stream.pop_front() {
        /*match stream.front() {
            Some(Tokens::LParen) => parse_fn_apply(name, consume(stream)),
            _ => Ok(Ast::Name(name)),
        }*/
        Ok(Ast::Name(name))
    } else { Err("Missing name".to_owned()) }
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
    if Some(Tokens::RParen) != stream.pop_front() {
        Err("Missing closing parenthesis in function app".to_owned())
    } else {
        Ok(Ast::FnApply(Box::new(func), args))
    }
}