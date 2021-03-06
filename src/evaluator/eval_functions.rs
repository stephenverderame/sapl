use super::eval;
use crate::evaluator::Values;
use crate::evaluator::Res;
use Res::Vl;
use super::exn::*;
use crate::parser::Ast;
use crate::evaluator::*;
use super::vals::eval_args;
use std::cell::RefCell;
use std::rc::Rc;
use crate::parser::SaplStruct;

/// Evaluates a function definition and adds the function to `scope`
/// Captures names references in `ast` that are also in `scope` by copying them into a new
/// environment
pub fn eval_func(name: &String, params: &Vec<(String, bool, Option<Box<Ast>>)>, ast: &Ast, postcondition: &Option<Box<Ast>>,
    scope: &mut impl Environment, public: bool)
    -> Res
{
    let v = match sapl_func_to_val(name, params, ast, postcondition, scope) {
        Vl(v) => v,
        e => return e,
    };
    super::scope_add(scope, name, v, false, public);
    Vl(Values::Unit)
}

/// Converts a function AST (Function or lambda to a value)
pub fn func_to_val(func: &Ast, scope: &mut impl Environment) -> Res {
    match func {
        Ast::Lambda(params, ast) => eval_lambda(params, ast, scope),
        Ast::Func(name, params, ast, pce) => sapl_func_to_val(name, params, &*ast, pce, scope),
        x => Bad(format!("Eval function error: {:?} is not a function", x)),
    }
}


/// Converts a sapl function AST to a sapl function value
/// Captures names in `ast` that are also in `scope` via copy
/// Adds the new function to the new function scope
fn sapl_func_to_val(name: &String, params: &Vec<(String, bool, Option<Box<Ast>>)>, ast: &Ast, pce: &Option<Box<Ast>>, 
    scope: &mut impl Environment) -> Res 
{
    let nw_scope = Rc::new(RefCell::new(std_sapl::get_std_environment()));
    let post = if pce.is_some() {
        let ptr = pce.as_ref().unwrap();
        capture_into_scope(&*ptr, &mut nw_scope.borrow_mut(), scope);
        Some(*ptr.clone())
    } else { None };   
    capture_into_scope(ast, &mut nw_scope.borrow_mut(), scope);
    capture_ast_fn_params(params, &mut nw_scope.borrow_mut(), scope);
    let func = Values::Func(eval_fn_params(params), ast.clone(), nw_scope.clone(), post);
    nw_scope.borrow_mut().add(name.to_string(), func.clone(), false);
    Vl(func)
}

/// Converts the AST function arguments to evaluator function arguments
fn eval_fn_params(params: &Vec<(String, bool, Option<Box<Ast>>)>) ->
    Vec<(String, bool, Option<Ast>)>
{
    let mut v = Vec::<(String, bool, Option<Ast>)>::new();
    for (nm, var, annotation) in params {
        let annotation = match annotation {
            Some(ptr) => Some(*ptr.clone()),
            _ => None,
        };
        v.push((nm.clone(), *var, annotation))
    }
    v
}

/// Captures the paramater annotations of `params` into `scope` from `old_scope`
fn capture_ast_fn_params(params: &Vec<(String, bool, Option<Box<Ast>>)>, 
    scope: &mut Scope, old_scope: &impl Environment) 
{
    for (.., annotation) in params {
        capture_scope_from_box(annotation, scope, old_scope);
    }
}

/// Evaluates a lambda expression
/// Captures names references in `ast` that are also in `scope` by copying them into a new
/// environment
/// Copies "this" into the new environment to allow recursion
pub fn eval_lambda(params: &Vec<(String, bool, Option<Box<Ast>>)>, ast: &Ast, scope: &mut impl Environment) 
    -> Res
{
    let nw_scope = Rc::new(RefCell::new(std_sapl::get_std_environment()));
    nw_scope.borrow_mut().add("this".to_owned(), 
        Values::Func(eval_fn_params(params), ast.clone(), nw_scope.clone(), None), false);
    capture_into_scope(ast, &mut nw_scope.borrow_mut(), scope);
    capture_ast_fn_params(params, &mut nw_scope.borrow_mut(), scope);
    Vl(Values::Func(eval_fn_params(params), ast.clone(), nw_scope, None))
}

/// Searches for names in `ast` and captures them by copying their corresponding value from
/// `old_scope` into `scope`
fn capture_into_scope(ast: &Ast, scope: &mut Scope, old_scope: &impl Environment) {
    match ast {
        Ast::Name(x) => capture_name(x, scope, old_scope),
        Ast::If(guard, body, other) | Ast::For(_, guard, other, body) => {
            capture_into_scope(&*guard, scope, old_scope);
            capture_into_scope(&*body, scope, old_scope);
            if let Some(x) = other {
                capture_into_scope(&*x, scope, old_scope);
            }
        },
        Ast::Seq(children) | Ast::Array(children) | Ast::Tuple(children) => {
            for node in children {
                capture_into_scope(&*node, scope, old_scope);
            }
        },
        Ast::Let(_, ast) | Ast::Uop(ast, _) | Ast::Export(ast) => 
            capture_into_scope(&*ast, scope, old_scope),
        Ast::Func(name, params, ast, condition) => {
            capture_name(name, scope, old_scope);
            capture_into_scope(&*ast, scope, old_scope);
            if let Some(ast) = condition {
                capture_into_scope(&*ast, scope, old_scope);
            }
            
            for (.., annotation) in params {
                if let Some(ast) = annotation {
                    capture_into_scope(&*ast, scope, old_scope);
                }
            }
        },
        Ast::Lambda(params, ast) => {
            capture_into_scope(&*ast, scope, old_scope);
            for (.., annotation) in params {
                if let Some(ast) = annotation {
                    capture_into_scope(&*ast, scope, old_scope);
                }
            }
        },
        Ast::FnApply(func, args) => {
            capture_into_scope(&*func, scope, old_scope);
            for expr in args {
                capture_into_scope(&*expr, scope, old_scope);
            }
        },
        Ast::While(one, two) | Ast::Bop(one, _, two ) 
        | Ast::Try(one, _, two) => {
            capture_into_scope(one, scope, old_scope);
            capture_into_scope(two, scope, old_scope);
        },
        Ast::Map(children) => {
            for (key, val) in &**children {
                capture_into_scope(val, scope, old_scope);
                capture_into_scope(key, scope, old_scope);
            }
        },
        Ast::Struct(SaplStruct {name: _, publics, privates, parents: _, ctor, dtor, ..}) |
        Ast::Type(SaplStruct {name: _, publics, privates, parents: _, ctor, dtor, ..}) => {
            for mem in publics.iter().chain(privates.iter()) {
                let (_, _, ast) = mem;
                capture_scope_from_box(ast, scope, old_scope);
            }
            capture_scope_from_box(ctor, scope, old_scope);
            capture_scope_from_box(dtor, scope, old_scope);
        }, 
        Ast::Placeholder | Ast::VInt(_) | Ast::VStr(_)
        | Ast::VBool(_) | Ast::VFloat(_) | Ast::Import(..)
        | Ast::None => (),

    }
}

/// Captures `name` into `scope` if it exists in `old_scope`
fn capture_name(name: &str, scope: &mut impl Environment, old_scope: &impl Environment) {
    if let Some((val, mtble)) = super::name_lookup(name, old_scope) {
        scope.add(name.to_string(), val.borrow().clone(), mtble)
    } 
}

/// @see capture_into_scope
fn capture_scope_from_box(ast: &Option<Box<Ast>>, scope: &mut Scope, 
    old_scope: &impl Environment) 
{
    if let Some(ast) = ast {
        capture_into_scope(&**ast, scope, old_scope);
    }
}

/// Evaluates a function application
/// Requires arguments are expressions that do no modify any values in the scope
pub fn eval_fn_app(func: &Ast, args: &Vec<Box<Ast>>, scope: &mut impl Environment)
    -> Res
{
    let func = match func {
        Ast::Name(nm) => {
            match super::name_lookup(nm, scope) {
                Some((ptr, _)) => ptr.clone(),
                _ => return ukn_name(nm),
            }
        },
        f => match eval(f, scope) {
            Vl(v) => Rc::new(RefCell::new(v)),
            e => return e,
        },
    };
    let mut val_args = Vec::<Values>::new();
    eval_args(args, scope, |val| -> Option<Res> {
        val_args.push(val);
        None
    });
    let x = apply_function(&func.borrow(), val_args, false, false); x

}

/// Applies `args` to the function `func`
/// If `args` contains a placeholder, performs a partial application
/// If `allow_incomplete` is true, assumes that if `args.len()` is less than
/// If `force_partial` is true, then the result is always a partial application
/// the amount of parameters, the remaining parameters will be placeholders and a partial application
/// will be performed
pub fn apply_function(func: &Values, mut args: Vec<Values>, allow_incomplete: bool, force_partial: bool) 
    -> Res 
{
    match func {
        Values::Func(params, ast, fn_scope, postcondition) =>
            apply_sapl_function(params, ast, fn_scope, postcondition, args, allow_incomplete, force_partial),
        Values::RustFunc(func, min_args) => 
            apply_rust_function(func.clone(), *min_args, args, allow_incomplete, force_partial),
        Values::Ref(func, _) =>
            apply_function(&func.borrow(), args, allow_incomplete, force_partial),
        Values::WeakRef(func, _) =>
            apply_function(&func.upgrade().unwrap().borrow(), args, allow_incomplete, force_partial),
        Values::Object(class, cc) => {
            let Class {members, ..} = &*class.borrow();
            if let Some(Member {val, ..}) = members.get("__call__") {
                if let func @ Values::RustFunc(..) = &*val.borrow() {
                    args.insert(0, Values::Object(class.clone(), *cc));
                    return apply_function(func, args, false, false)
                }
            }
            str_exn(&format!("Object does not have a valid operator() override"))
        },
        x => str_exn(&format!("{}: Apply function, {:?}", NFUNC, x)[..]),
    } 
}

/// Applies a function defined in Sapl
/// `params` the names of the local parameters
/// `ast` the body of the function
/// `fn_scope` the saved scope of the function
/// `postcondition` the function's PCE
/// `args` the values passed to the function
/// `allow_incomplete` true to allow implicit partial application
fn apply_sapl_function(params: &Vec<(String, bool, Option<Ast>)>, ast: &Ast, fn_scope: &Rc<RefCell<Scope>>, 
    postcondition: &Option<Ast>, mut args: Vec<Values>, allow_incomplete: bool,
    force_partial: bool) -> Res 
{
    if allow_incomplete && args.len() < params.len() {
        args.append(&mut vec![Values::Placeholder; params.len() - args.len()]);
    } else if params.len() != args.len() { 
        return Bad(format!("Arg count mismatch passed formals {} vs args {} and incomplete: {}",
            params.len(), args.len(), allow_incomplete)); 
    }

    let mut sub = fn_scope.borrow().cpy();
    let mut sc = ScopeProxy::from(&mut sub);
    let mut is_partial = false;
    let mut missing_params = Vec::<(String, bool, Option<Ast>)>::new();
    for ((nm, var, annotation), arg) in params.iter().zip(args.into_iter()) {
        match arg {
            Values::Placeholder => {
                missing_params.push((nm.to_string(), *var, annotation.clone()));
                is_partial = true
            },
            v => {
                match check_func_annotation(v, annotation, &*fn_scope.borrow(), "arg") {
                    Vl(v) => sc.add(nm.to_string(), v, *var),
                    e => return e,
                }
            },
        }
    }
    if is_partial || force_partial {
        Vl(Values::Func(missing_params, ast.clone(), 
            Rc::new(RefCell::new(sc.cpy())), postcondition.clone()))
    } else {
        match eval(ast, &mut sc) {
            Res::Ret(v) | Vl(v) => 
                check_func_annotation(v, postcondition, &*fn_scope.borrow(), "result"),
            e => e,
        }
    }
}

/// Evaluates a built in interpreter function
/// `func` the rust function
/// `min_args` the minimum arguments to call `func`
/// `args` the args passed to `func`
/// `allow_incomplete` true to allow implicit partial application
fn apply_rust_function(func: Rc<dyn Fn(Vec<Values>) -> Res>, min_args: usize, 
    args: Vec<Values>, allow_incomplete: bool, force_partial: bool) -> Res 
{
    let mut params = Vec::<Values>::new();
    let mut missing_params = Vec::<usize>::new();
    let mut idx = 0;
    for a in args {
        match a {
            Values::Placeholder => missing_params.push(idx),
            v => params.push(v),
        };
        idx = idx + 1;
    }
    //println!("Applying rust function with args: {:?}", params);
    if (params.len() < min_args && allow_incomplete)
        || !missing_params.is_empty() || force_partial
    {
        let min_args = std::cmp::max(min_args - params.len(), 0);
        let new_func = move |mut a: Vec<Values>| -> Res {
            let mut params = params.clone();
            for i in 0 .. missing_params.len() {
                if !a.is_empty() {
                    params.insert(missing_params[i], a.swap_remove(0));
                } else { break; }
            }
            if !a.is_empty() {
                params.append(&mut a);
            }
            
            apply_rust_function(func.clone(), min_args, params, allow_incomplete, false)
        };
        Vl(Values::RustFunc(Rc::new(new_func), min_args))
    } else {
        func(params)
    }
}

/// Checks the function annotation
/// Returns `result` if the annotation is true or the annotation contains a valid name only
/// Otherwise, returns an exception
/// When evaluating the annotation, adds to the postcondition expression scope a name which is the type of the result
/// that stores the value of the result. Also adds the value to scope under the name `variable_name`
fn check_func_annotation(result: Values, postcondition: &Option<Ast>, scope: &impl Environment, variable_name: &str) -> Res {
    match postcondition {
        None => Vl(result),
        Some(ast) => {
            let mut scope = ImmuScope::from(scope);
            let result = Rc::new(RefCell::new(result));
            add_type_variables_to_scope(&result, &mut scope);
            scope.add_direct(variable_name.to_owned(), result.clone(), false);
            match eval(&ast, &mut scope) {
                Vl(_) if matches!(&ast, &Ast::Name(_)) => Vl(result.borrow().clone()),
                Vl(Values::Bool(true)) => Vl(result.borrow().clone()),
                _ => str_exn(&format!(
                    "{}: expected a value to satisfy {:?} but got {:?}", PCE_FAIL, ast, &*result.borrow())),

            }
        },
    }
}

fn add_type_variables_to_scope(val: &Rc<RefCell<Values>>, scope: &mut impl Environment) {
    scope.add_direct(type_of(&*val.borrow()), val.clone(), false);
    if &*val.borrow() == &Values::Unit {
        scope.add("none".to_owned(), Values::Unit, false);
    } else {
        scope.add_direct("some".to_owned(), val.clone(), false);
        match &*val.borrow() {
            Values::Float(_) | Values::Int(_) =>
                scope.add_direct("number".to_owned(), val.clone(), false),
            Values::Tuple(_) =>
                scope.add_direct("tuple".to_owned(), val.clone(), false),
            Values::Type(class) => {
                scope.add_direct("object".to_owned(), val.clone(), false);
                let Class {parents, ..} = &**class;
                for p in parents {
                    scope.add_direct(p.clone(), val.clone(), false);
                }
            },
            Values::Object(class, ..) => {
                scope.add_direct("object".to_owned(), val.clone(), false);
                let Class {parents, ..} = &*class.borrow();
                for p in parents {
                    scope.add_direct(p.clone(), val.clone(), false);
                }
            }
            _ => (),
        }
    }
}