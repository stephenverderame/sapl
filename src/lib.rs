mod lexer;
mod parser;
mod evaluator;
mod test;

pub use evaluator::Values;
pub use evaluator::Res;
pub use evaluator::Scope;
pub use evaluator::Environment;
use std::io::Read;
use std::io::Write;
use std::rc::Rc;
use std::cell::RefCell;
pub use evaluator::get_std_environment;

/// Parses sapl code from an input stream
/// The input stream must contain only sapl code
pub fn parse_sapl(input: impl Read) -> Res {
    let mut op : Option<&mut Scope> = None;
    parse_and_eval(input, &mut op)
}

pub fn parse_and_eval(input: impl Read, scope: &mut Option<&mut impl Environment>) -> Res {
    let mut tokens = lexer::tokenize(input);
    let ast = parser::parse(&mut tokens);
    if let Ok(ast) = ast {
        match scope {
            Some(env) => evaluator::eval(&ast, *env),
            _ => evaluator::evaluate(&ast),
        }
    } else {
        Res::Bad(ast.unwrap_err())
    }
}

/// Parses sapl that is in between `delim`
/// Streams `input` to `output` and inserts the result of sapl code
/// `Unit` values are not written, and Strings do not have quotes
/// Requires `delim` is >= 2 characters
/// `delim` can be escaped by prepending backslashed before each character
pub fn preprocess_sapl(input: impl Read, output: Rc<RefCell<dyn Write>>, delim: &str) {
    let mut scope = evaluator::get_std_environment();
    parse_sapl_inplace(input, output, delim, &mut Some(&mut scope))
}

fn add_lambda(scope: &mut impl Environment, name: &str, func: Rc<dyn Fn(Vec<Values>) -> Res>, min_args: usize) {
    scope.add(name.to_owned(), Values::RustFunc(func, min_args), false);
}

/// Parses sapl from `input` in between `delim` and writes the result to `output`
/// `env` - the environment to evaluate the code in or `None` to use the standard environment
fn parse_sapl_inplace(mut input: impl Read, output: Rc<RefCell<dyn Write>>, delim: &str, 
    env: &mut Option<&mut impl Environment>) 
{
    let mut buffer = String::new();
    input.read_to_string(&mut buffer).unwrap();
    let mut is_sapl = false;
    let escaped = escaped_delim(delim);
    augment_environment(&output, env);
    for ln in buffer.split(delim) {
        if is_sapl {
            match parse_and_eval(ln.as_bytes(), env) {
                Res::Vl(val) | Res::Ret(val) | Res::Exn(val) => {
                    if val != Values::Unit {
                        output.borrow_mut()
                            .write_all(format!("{:?}", val).as_bytes()).unwrap();
                    }
                },
                e => {
                    output.borrow_mut()
                        .write_all(format!("{:?}", e).as_bytes()).unwrap();
                },
            }
        } else {
            let res = ln.replace(delim, &escaped);
            output.borrow_mut().write_all(res.as_bytes()).unwrap();
        }
        is_sapl = !is_sapl;
    }
}

fn augment_environment(output: &Rc<RefCell<dyn Write>>, scope: &mut Option<&mut impl Environment>) {
    use evaluator::Res::Vl;
    if let Some(scope) = scope {
        let write = output.clone();
        add_lambda(*scope, "print", Rc::new(move |args: Vec<Values>| -> Res {
            for arg in args {
                write.borrow_mut().write_all(format!("{:?}", arg).as_bytes())
                    .unwrap();
            }
            Vl(Values::Unit)
        }), 1);
        let write = output.clone();
        add_lambda(*scope, "println", Rc::new(move |args: Vec<Values>| -> Res {
            for arg in args {
                write.borrow_mut().write_all(format!("{:?}", arg).as_bytes())
                    .unwrap();
            }
            write.borrow_mut().write_all("\n".as_bytes()).unwrap();
            Vl(Values::Unit)
        }), 1);
    } 
}

fn escaped_delim(delim: &str) -> String {
    let mut escaped = String::new();
    for c in delim.as_bytes() {
        escaped.push(*c as char);
        escaped.push('\\');
    }
    escaped
}
