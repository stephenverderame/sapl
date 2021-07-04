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
use std::fmt;

#[derive(PartialEq)]
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

impl std::fmt::Debug for Member {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ val: {:?}, var: {}, pub: {} }}", &*self.val.borrow(),
            self.is_var, self.is_pub)
    }
}

#[derive(PartialEq, Clone)]
pub struct Class {
    pub name: String,
    pub members: HashMap<String, Member>,
    pub dtor: Option<Values>,
    pub friends: Vec<String>,
    pub parents: Vec<String>,

}

impl std::fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {{ {:?}, dtor: {:?} }}", self.name, self.members, self.dtor)
    }
}

/*
impl Drop for Class {
    fn drop(&mut self) {
        if let Some(f @ Values::Func(..)) = &*self.dtor {
            apply_function(&f, Vec::<Values>::new(), false, false);
        }
    }
}*/

pub fn eval_class_def(class: &SaplStruct, scope: &mut impl Environment, public: bool) -> Res {
    let (mems, friends) = match get_object_members(class, scope) {
        Ok(map) => map,
        Err(e) => return e,
    };
    match get_class_ctor(mems, class, scope, friends) {
        Vl(ctor) => super::scope_add(scope, &class.name, ctor, false, public),
        e => return e,
    };
    Vl(Values::Unit)
}

pub fn eval_type_def(interface: &SaplStruct, scope: &mut impl Environment, public: bool) -> Res {
    let (mems, friends) = match get_object_members(interface, scope) {
        Ok(tup) => tup,
        Err(e) => return e,
    };
    super::scope_add(scope, &interface.name, Values::Type(Rc::new(
        Class {
            name: interface.name.to_owned(),
            members: mems,
            dtor: None,
            friends: friends,
            parents: interface.parents.clone(),
        }
    )), false, public);
    Vl(Values::Unit)
}

/// Gets a tuple of the all of the object's members and friends including
/// the ones that are inherited from parents or `Res` on error
fn get_object_members(object: &SaplStruct, scope: &mut impl Environment) 
    -> Result<(HashMap<String, Member>, Vec<String>), Res>
{
    let mut mems = HashMap::<String, Member>::new();
    let mut all_friends = object.friends.clone();
    for parent in &object.parents {
        if let Some((ptr, _)) = super::name_lookup(&parent, scope) {
            if let Values::Type(ptr) = &*ptr.borrow() {
                let Class {name:_, members, friends, ..} = ptr.as_ref();
                for (name, mem) in members {
                    println!("Adding {:?}", mem);
                    mems.insert(name.clone(), mem.clone());
                }
                for name in friends {
                    all_friends.push(name.clone());
                }
            } else { return Err(str_exn("Cannot subtype from non-type")); }
        } else { return Err(ukn_name(parent)); }
    }
    if let Some(r) = iter_members(&mut mems, &object.privates, false, scope) {
        return Err(r);
    }
    if let Some(r) = iter_members(&mut mems, &object.publics, true, scope) {
        return Err(r);
    }
    Ok((mems, all_friends))
}


/// Iterates through a list of members of an object
/// `mems` the total member list (output)
/// `container` is a list of members
/// `is_pub` if the member list is public
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

/// Processes object members by iterating through and converting functions 
/// to rust functions with all members added to scope
/// The new function will wrap the old one and expect a self reference as the first parameter to 
fn process_fn_mem(mems: HashMap<String, Member>, ctor: Rc<RefCell<Values>>, cname: &String) -> HashMap<String, Member> {
    let mut map = HashMap::<String, Member>::new();
    for pair in mems {
        match pair {           
            (k, Member {val, is_var, is_pub}) if matches!(&*val.borrow(), Values::Func(..)) => {
                if let Values::Func(params, body, scope, pce) = &*val.borrow() {
                    let min_args = params.len() + 1;
                    let params = params.clone();
                    let body = body.clone();
                    let pce = pce.clone();
                    let scope = scope.clone();
                    let ctor = ctor.clone();
                    let cname = cname.clone();
                    let rf = move |mut args: Vec<Values>| -> Res {
                        let this = args.remove(0);
                        match this {
                            Values::Ref(ptr, mtble) => add_self(&*ptr.borrow(), mtble, scope.clone()),
                            Values::WeakRef(ptr, mtble) => add_self(&*ptr.upgrade().unwrap().borrow(), mtble, scope.clone()),
                            obj @ Values::Object(..) => add_self(&obj, true, scope.clone()),
                            x => panic!("Self is not the first param. Got {:?}", x),
                        };
                        scope.borrow_mut().add_direct(cname.clone(), ctor.clone(), false);
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

/// Adds the `self` variable to scope
/// The self variable will have the SelfCall or Constructor calling context
fn add_self(val: &Values, mtble: bool, scope: Rc<RefCell<Scope>>) {
    if let Values::Object(ptr, ctx) = val {
        let is_ctor = ctx == &CallingContext::Constructor;
        let mtble = mtble || is_ctor;
        let nw_ctx = if is_ctor { *ctx } else { CallingContext::SelfCall };
        scope.borrow_mut().add("self".to_owned(), Values::Object(ptr.clone(), nw_ctx), mtble);
    } else { panic!("Self is not the first param. Got {:?}", val); }
}


fn get_class_ctor(mems: HashMap<String, Member>, class: &SaplStruct, scope: &mut impl Environment, friends: Vec<String>) -> Res {
    let ctor = if let Some(func) = &class.ctor {
        match eval_functions::func_to_val(&*func, scope) {
            Vl(v) => Some(v),
            e => return e,
        }
    } else { None };
    let min_args = match &ctor {
        Some(Values::Func(ps, ..)) => ps.len(),
        _ => 0,
    };
    let name = class.name.clone();
    let rust_ctor = Rc::new(RefCell::new(Values::Unit));
    let rc = rust_ctor.clone();
    let parents = class.parents.clone();
    let func = move |args: Vec<Values>| -> Res {
        let mems = mems.clone();
        let ctor = ctor.clone();
        let name = name.clone();
        let self_obj = Rc::new(RefCell::new(
            Class {
                name: name.clone(),
                members: process_fn_mem(mems, rc.clone(), &name.clone()),
                dtor: None,
                friends: friends.clone(),
                parents: parents.clone(),
            }
        ));
        if let Some(Values::Func(ps, body, fn_scope, pce)) = ctor {
            fn_scope.borrow_mut().add("self".to_owned(), 
                Values::Object(self_obj.clone(), CallingContext::Constructor), true);
            fn_scope.borrow_mut().add_direct(name, rc.clone(), false);
            match apply_function(&Values::Func(ps, body, fn_scope, pce), 
                args, false, false) 
            {
                Vl(_) => (),
                e => return e,
            }
        }
        Vl(Values::Object(self_obj, CallingContext::Public))

    };
    let fun = Rc::new(func);
    *(rust_ctor.borrow_mut()) = Values::RustFunc(fun.clone(), min_args);
    Vl(Values::RustFunc(fun, min_args))

}