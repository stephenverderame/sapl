use crate::lexer::Tokens;
use std::collections::VecDeque;
mod parse_op;
use parse_op::*;
mod parse_control;
use parse_control::*;
mod parse_func;
use parse_func::*;
mod parse_class;
pub use parse_class::SaplStruct;
use parse_class::*;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Op {
    Plus,
    Mult,
    Div,
    Mod,
    Sub,
    Exp,
    Land,
    Lor,
    And,
    Or,
    Lt,
    Gt,
    Eq,
    Neq,
    Leq,
    Geq,
    Range,
    Not,
    Pipeline,
    Dot,
    Neg,
    Concat,
    AsBool,
    Index,
    Return,
    Throw,
    Assign,
    Update,
    Ref,
    Deref,
    MutRef,
    As,
    Is,
    Include,
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
    Func(
        String,
        Vec<(String, bool, Option<Box<Ast>>)>,
        Box<Ast>,
        Option<Box<Ast>>,
    ),
    Lambda(Vec<(String, bool, Option<Box<Ast>>)>, Box<Ast>),
    FnApply(Box<Ast>, Vec<Box<Ast>>),
    Array(Vec<Box<Ast>>),
    Tuple(Vec<Box<Ast>>),
    Placeholder,
    None,
    Uop(Box<Ast>, Op),
    Map(Box<Vec<(Ast, Ast)>>),
    Try(Box<Ast>, String, Box<Ast>),
    For(Vec<String>, Box<Ast>, Option<Box<Ast>>, Box<Ast>),
    While(Box<Ast>, Box<Ast>),
    Struct(SaplStruct),
    Type(SaplStruct),
    Export(Box<Ast>),
    Import(String, Option<String>),
}

/// Parses a stream of tokens `stream` into an Abstract Syntax Tree
pub fn parse(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    parse_seq(parse_single(stream), stream)
}

/// Parses a single expression or definition
fn parse_single(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.front() {
        Some(x) if tok_is_defn(&x) => parse_defn(stream),
        Some(x) if tok_is_expr(&x) => parse_expr(stream, None),
        x => Err(format!("Expected expression, got {:?}", x)),
    }
}

/// True if `tok` can start an expression
fn tok_is_expr(tok: &Tokens) -> bool {
    match tok {
        Tokens::LParen
        | Tokens::If
        | Tokens::Name(_)
        | Tokens::OpQ
        | Tokens::LBracket
        | Tokens::OpNegate
        | Tokens::LBrace
        | Tokens::Try
        | Tokens::Fun => true,
        x if tok_is_pre_uop(x) => true,
        x if tok_is_val(x) => true,
        _ => false,
    }
}

fn tok_is_defn(tok: &Tokens) -> bool {
    match *tok {
        Tokens::Let
        | Tokens::Fun
        | Tokens::For
        | Tokens::While
        | Tokens::Struct
        | Tokens::Type
        | Tokens::Pub
        | Tokens::Import => true,
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
        Tokens::None => Ok(Ast::None),
        _ => Err(format!("{:?} is not a value", tok)),
    }
}

/// Parses an arithmetic expression
/// If it cannot parse the next token sequence, an error is returned and the
/// stream is unchanged
/// `parent_precedence` is the precedence of the parent Bop or `None` if this is not a sub-expression of
/// a bop
fn parse_expr(
    stream: &mut VecDeque<Tokens>,
    parent_precedence: Option<i32>,
) -> Result<Ast, String> {
    let left = parse_expr_left(stream);
    if left.is_err() {
        return left;
    }
    match stream.front() {
        Some(x) if tok_is_bop(x) => parse_bop(left.unwrap(), parent_precedence, stream),
        Some(x) if tok_is_post_uop(x) => match parse_post_uop(left.unwrap(), stream) {
            Ok(ast) => parse_bop(ast, parent_precedence, stream),
            e => e,
        },
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
        }
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
fn parse_paren_expr(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let expr = parse_expr(stream, None);
    if let Ok(ast) = expr {
        match stream.pop_front() {
            Some(Tokens::RParen) => Ok(ast),
            Some(Tokens::Comma) => parse_tuple(ast, stream),
            _ => Err("Missing closing parenthesis".to_owned()),
        }
    } else {
        expr
    }
}

/// Parses a block that can start with a colon or be wrapped in braces
/// `only_expr_wo_brace` - set to true if the block can only contain an expression if
/// there are no braces
/// `can_omit` - set to true if the block can omit : or {}
/// Requires the block begins with the first token in `stream`.
fn parse_block(
    stream: &mut VecDeque<Tokens>,
    only_expr_wo_brace: bool,
    can_omit: bool,
) -> Result<Ast, String> {
    let is_brace = stream.front() == Some(&Tokens::LBrace);
    let is_colon = stream.front() == Some(&Tokens::Colon);
    if is_brace || is_colon || can_omit {
        if is_brace || is_colon {
            consume(stream);
        }
        let body = if only_expr_wo_brace && !is_brace {
            parse_expr(stream, None)
        } else {
            parse(stream)
        };
        if is_brace {
            crate::expect!(stream, Tokens::RBrace);
        }
        body
    } else {
        Err(format!(
            "Parsing block got {:?} instead of a colon or brace",
            stream.front()
        ))
    }
}

fn parse_tuple(first: Ast, stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let mut elems = vec![Box::new(first)];
    loop {
        match parse_expr(stream, None) {
            Ok(ast) => {
                elems.push(Box::new(ast));
                if stream.front() != Some(&Tokens::Comma) {
                    break;
                }
            }
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

/// Parses the next token sequence as a definition
/// If the next token is not a definition, the stream is unchanged and
/// an error is returned
fn parse_defn(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    match stream.front() {
        Some(Tokens::Let) => parse_let(consume(stream)),
        Some(Tokens::Fun) => parse_func(consume(stream)),
        Some(Tokens::For) => parse_for_loop(consume(stream)),
        Some(Tokens::While) => parse_while_loop(consume(stream)),
        Some(Tokens::Struct) => parse_struct(consume(stream)),
        Some(Tokens::Type) => parse_type(consume(stream)),
        Some(Tokens::Import) => parse_import(consume(stream)),
        Some(Tokens::Pub) => match parse_defn(consume(stream)) {
            Ok(def) => Ok(Ast::Export(Box::new(def))),
            e => e,
        },
        x => Err(format!("Expected a definition. Got {:?}", x)),
    }
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

/// Unconditionally pops the front of `stream` and returns `stream`
fn consume(stream: &mut VecDeque<Tokens>) -> &mut VecDeque<Tokens> {
    stream.pop_front();
    stream
}

/// Pops the front of the stream an ensures that the token is the same as the expected token
#[macro_export]
macro_rules! expect {
    ($a:expr, $b:expr) => {{
        match $a.pop_front() {
            Some(t) if t == $b => (),
            e => return Err(format!("Parse Error: Expected {:?} but got {:?}", $b, e)),
        }
    }};
    ($a:expr, $b:expr, $c:expr) => {{
        match $a.pop_front() {
            Some(t) if t == $b => (),
            e => {
                return Err(format!(
                    "Parse Error: Expected {:?} but got {:?}. {:?}",
                    $b, e, $c
                ))
            }
        }
    }};
}

/// Parses a map
/// Requires the first brace has been consumed
fn parse_map(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    let mut map = Vec::<(Ast, Ast)>::new();
    loop {
        if stream.front() == Some(&Tokens::RBrace) {
            break;
        }
        let key = match parse_expr(stream, None) {
            Ok(ast) => ast,
            e => return e,
        };
        if Some(Tokens::Colon) != stream.pop_front() {
            return Err("Map missing colon after name".to_owned());
        }
        match parse_expr(stream, None) {
            Ok(val) => map.push((key, val)),
            e => return e,
        };
        if Some(&Tokens::Comma) != stream.front() {
            break;
        } else {
            consume(stream);
        }
    }
    if Some(Tokens::RBrace) != stream.pop_front() {
        Err("Map missing closing brace".to_owned())
    } else {
        Ok(Ast::Map(Box::new(map)))
    }
}

fn parse_import(stream: &mut VecDeque<Tokens>) -> Result<Ast, String> {
    if let Some(Tokens::Name(mut import)) = stream.pop_front() {
        if stream.front() == Some(&Tokens::OpMult) {
            stream.pop_front();
            import.push('*');
        }
        let mut prefix = if import.find("::*").is_some() {
            None
        } else {
            Some(import.clone())
        };
        if stream.front() == Some(&Tokens::As) {
            if let Some(Tokens::Name(rename)) = consume(stream).pop_front() {
                prefix = Some(rename);
            } else {
                return Err("Missing name after as in import definition".to_owned());
            }
        }
        let mut import = import.replace("::*", "").replace("::", "/");
        import.push_str(".sapl");
        Ok(Ast::Import(import, prefix))
    } else {
        Err("Missing module to import".to_owned())
    }
}
