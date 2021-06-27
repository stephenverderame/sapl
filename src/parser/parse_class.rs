use std::collections::VecDeque;
use crate::lexer::Tokens;
use super::Ast;
use super::consume;
use super::parse_control;

#[derive(Debug, Clone, PartialEq)]
pub struct SaplStruct {
    pub name: String,
    pub publics: Vec<(String, bool, Option<Box<Ast>>)>,
    pub privates: Vec<(String, bool, Option<Box<Ast>>)>,
    pub parents: Vec<String>,
    pub ctor: Option<Box<Ast>>,
    pub dtor: Option<Box<Ast>>,
}

impl SaplStruct {
    fn new(name: String) -> SaplStruct {
        SaplStruct {
            name,
            publics: Vec::<(String, bool, Option<Box<Ast>>)>::new(),
            privates: Vec::<(String, bool, Option<Box<Ast>>)>::new(),
            parents: Vec::<String>::new(),
            ctor: None,
            dtor: None,
        }
    }
}


/// Requires struct token already consumed
pub fn parse_struct(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    parse_object(stream, false)
}

pub fn parse_type(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    parse_object(stream, true)
}

#[inline(always)]
fn parse_object(stream: &mut VecDeque<Tokens>, is_type: bool) -> Result<Ast, String> {
    if let Some(Tokens::Name(name)) = stream.pop_front() {
        let parent_types = parse_object_types(stream);
        if stream.pop_front() != Some(Tokens::LBrace) {
            return Err("Struct missing opening brace".to_owned());
        }
        match parse_struct_body(name, stream) {
            Ok(mut object) => {
                parent_types.and_then(|parents: Vec<String>| {
                    object.parents = parents; Some(())
                });
                if is_type {
                    Ok(Ast::Type(object))
                } else { Ok(Ast::Struct(object)) }
            },
            Err(e) => Err(e),
        }
        

    } else {
        Err("Struct missing name".to_owned())
    }
}

fn parse_object_types(stream: &mut VecDeque<Tokens>) -> Option<Vec<String>> {
    if stream.front() == Some(&Tokens::Colon) {
        stream.pop_front();
        match parse_control::parse_comma_sep_names(stream) {
            None => None,
            Some(x) => Some(x.into_iter().map(|(name, _): (String, bool)| {
                name
            }).collect()),
        }
    } else { None }
}

fn parse_struct_body(name: String, stream: &mut VecDeque<Tokens>) -> Result<SaplStruct, String> {
    let mut object = SaplStruct::new(name);
    loop {
        let res = 
        match stream.front() {
            Some(&Tokens::Pub) => parse_member(consume(stream), true, &mut object),
            Some(&Tokens::RBrace) => { consume(stream); break},
            _ => parse_member(stream, false, &mut object),
        };
        if let Some(msg) = res {
            return Err(msg);
        }
    }
    Ok(object)
}

fn parse_member(stream: &mut VecDeque<Tokens>, public: bool, object: &mut SaplStruct) -> Option<String> {
    match stream.front() {
        Some(&Tokens::Def) => parse_struct_def(consume(stream), public, object),
        Some(&Tokens::Fun) => parse_struct_fun(stream, public, object),
        tok => Some(format!("Unknown token {:?} in struct definition", tok)),
    }
}

/// Requires def token is consumed
fn parse_struct_def(stream: &mut VecDeque<Tokens>, public: bool, object: &mut SaplStruct) -> Option<String> {
    loop {
        let is_var = stream.front() == Some(&Tokens::Var);
        if is_var { stream.pop_front(); }
        if let Some(Tokens::Name(name)) = stream.pop_front() {
            let init = 
            match parse_def_init(stream) {
                Some(Err(err)) => return Some(err),
                None => None,
                Some(Ok(ast)) => Some(Box::new(ast)),
            };
            add_struct_val(public, (name, is_var, init), object);
        } else { return Some("Missing name from definition".to_owned()) }
        if stream.front() != Some(&Tokens::Comma) { break; }
        stream.pop_front();
    }
    if stream.front() == Some(&Tokens::Seq) { consume(stream); }
    None
}

fn parse_def_init(stream: &mut VecDeque<Tokens>) -> Option<Result<Ast, String>> {
    if stream.front() == Some(&Tokens::OpAssign) {
        stream.pop_front();
        Some(super::parse_expr(stream, None))
    } else { None }
}

fn add_struct_val(public: bool, member: (String, bool, Option<Box<Ast>>), object: &mut SaplStruct) {
    let vec =   
        if public {&mut object.publics} else {&mut object.privates};
    vec.push(member)
}

/// Requires fun token is first on the stream
fn parse_struct_fun(stream: &mut VecDeque<Tokens>, public: bool, object: &mut SaplStruct) -> Option<String> {
    match super::parse_single(stream) {
        Ok(Ast::Func(name, a, b, c)) => {
            let val = Box::new(Ast::Func(name.clone(), a, b, c));
            if name == object.name {
                object.ctor = Some(val);
            } else if name == format!("`{}", object.name) {
                object.dtor = Some(val);
            } else {
                add_struct_val(public, (name, false, Some(val)), object);
            }
            None
        },
        Ok(_) => panic!("Broken precondition"),
        Err(msg) => Some(msg),
    }
}