use super::eval;
use crate::evaluator::Values;
use crate::evaluator::Res;
use Res::Vl;
use crate::parser::Ast;
use crate::evaluator::*;
use super::eval_functions::apply_function;
use std::cell::RefCell;
use std::rc::Rc;
use crate::parser::SaplStruct;

#[derive(PartialEq, Debug)]
pub struct Member {
    pub val: Rc<RefCell<Values>>,
    pub is_var: bool,
    pub is_pub: bool,

}

impl Clone for Member {
    fn clone(&self) -> Member {
        Member {
            val: Rc::new(RefCell::new(self.val.borrow().clone())),
            is_var: self.is_var,
            is_pub: self.is_pub,
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Class {
    pub name: String,
    pub members: HashMap<String, Member>,
    pub dtor: Box<Option<Values>>,

}

/*
impl Drop for Class {
    fn drop(&mut self) {
        if let Some(f @ Values::Func(..)) = &*self.dtor {
            apply_function(&f, Vec::<Values>::new(), false, false);
        }
    }
}*/

pub fn eval_class_def(class: &SaplStruct, scope: &mut impl Environment) -> Res {
    let mut mems = HashMap::<String, Member>::new();
    if let Some(r) = iter_members(&mut mems, &class.privates, false, scope) {
        return r;
    }
    if let Some(r) = iter_members(&mut mems, &class.publics, true, scope) {
        return r;
    }
    match get_class_ctor(mems, class, scope) {
        Vl(ctor) => scope.add(class.name.to_owned(), ctor, false),
        e => return e,
    };
    Vl(Values::Unit)
}

fn iter_members(mems: &mut HashMap<String, Member>, container: &Vec<(String, bool, Option<Box<Ast>>)>, is_pub: bool, scope: &mut impl Environment) 
    -> Option<Res>
{
    for (name, is_var, val) in container {
        let val = if let Some(val) = val {
            if let f @ Ast::Func(..) = &**val {
                match eval_functions::func_to_val(f, scope) {
                    Vl(v) => v,
                    e => return Some(e),
                }
            } else {
                match eval(&*val, scope) {
                    Vl(v) => v,
                    e => return Some(e),
                }
            }
        } else { Values::Unit };
        mems.insert(name.to_owned(),
            Member {val: Rc::new(RefCell::new(val)),
                    is_var: *is_var,
                    is_pub}
        );
    }
    None
}

fn process_fn_mem(mems: HashMap<String, Member>) -> HashMap<String, Member> {
    let mut map = HashMap::<String, Member>::new();
    for pair in mems {
        match pair {           
            (k, Member {val, is_var, is_pub}) if matches!(&*val.borrow(), Values::Func(..)) => {
                if let Values::Func(params, body, scope, pce) = &*val.borrow_mut() {
                    let min_args = params.len() + 1;
                    let params = params.clone();
                    let body = body.clone();
                    let pce = pce.clone();
                    let scope = scope.clone();
                    let rf = move |mut args: Vec<Values>| -> Res {
                        let this = args.remove(0);
                        if let Values::Ref(ptr, mtble) = this {
                            if let Values::Object(Class {name:_, members, ..}) = &mut *ptr.borrow_mut() {
                                add_mems_to_scope(members, &mut *scope.borrow_mut(), mtble);
                            } else { panic!("Self is not the first param. Got {:?} and {:?}", ptr, args); }
                        } else { panic!("Self is not the first param. Got {:?} and {:?}", this, args); }
                        apply_function(&Values::Func(params.clone(), 
                            body.clone(), scope.clone(), pce.clone()), 
                            args, false, false)
                    };
                    map.insert(k, Member { 
                        val: Rc::new(RefCell::new(
                            Values::RustFunc(Rc::new(rf), min_args))), 
                        is_var, is_pub 
                    });
                } else { panic!("Precondition violated") }
            },
            (k, v) => { map.insert(k, v); },
        }
    }
    map
}

fn add_mems_to_scope(mems: &HashMap<String, Member>, scope: &mut Scope, mut_obj: bool) {
    for (name, Member {val, is_var, is_pub: _}) in mems.iter() {
        let var = *is_var && mut_obj;
        scope.add_direct(name.clone(), val.clone(), var);
        scope.add_direct(format!("_{}", name), val.clone(), var);
    }
}

fn get_class_ctor(mems: HashMap<String, Member>, class: &SaplStruct, scope: &mut impl Environment) -> Res {
    let ctor = if let Some(func) = &class.ctor {
        match eval_functions::func_to_val(&*func, scope) {
            Vl(v) => Some(v),
            e => return e,
        }
    } else { None };
    let dtor = if let Some(func) = &class.dtor {
        match eval(&*func, scope) {
            Vl(v) => Some(v),
            e => return e,
        }
    } else { None };
    let dtor = Box::new(dtor);
    let min_args = match &ctor {
        Some(Values::Func(ps, ..)) => ps.len(),
        _ => 0,
    };
    let name = class.name.clone();
    let func = move |args: Vec<Values>| -> Res {
        let mems = mems.clone();
        let ctor = ctor.clone();
        let dtor = dtor.clone();
        let name = name.clone();
        if let Some(Values::Func(ps, body, fn_scope, pce)) = ctor {
            for (k, v) in &mems {
                fn_scope.borrow_mut().add_direct(k.to_owned(), v.val.clone(), true);
                fn_scope.borrow_mut().add_direct(format!("_{}", k), v.val.clone(), true);
            }
            match apply_function(&Values::Func(ps, body, fn_scope, pce), args, false, false) {
                Vl(_) => (),
                e => return e,
            }
        }
        Vl(Values::Object(Class {
            name,
            members: process_fn_mem(mems),
            dtor,
        }))

    };
    Vl(Values::RustFunc(Rc::new(func), min_args))

}