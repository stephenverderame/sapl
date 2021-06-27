use crate::evaluator::Values;
use std::collections::HashMap;
use std::collections::LinkedList;
use std::rc::Rc;
use std::cell::RefCell;



pub trait Environment: std::fmt::Debug {
    /// Gets a value stored in this environment with the name `x`
    fn find(&self, name: &str) -> Option<(Rc<RefCell<Values>>, bool)>;

    /// Adds a new value into the environment with the name `name` and 
    /// mutability `mutable`
    /// Will add the value to the first scope in the stack and shadow previous
    /// values with the same name if they exist
    /// If `name` is `"_"`, the value is not added
    fn add(&mut self, name: String, val: Values, mutable: bool);

    fn add_direct(&mut self, name: String, val: Rc<RefCell<Values>>, mutable: bool);

    /// Updates an existing value in the environment
    /// Returns `false` if `name` is immutable or if the value
    /// isn't found
    fn update(&mut self, name: &str, val: Values) -> bool;

    /// Performs a deep copy of the environment
    fn cpy(&self) -> Scope;

    /// Internal use only: creates a new sub scope
    /// Should be called through the RAII `ScopeProxy`
    fn new_scope(&mut self);
    
    /// Internal use only: pops a sub scope off the scope stack
    /// Should be called through the RAII `ScopeProxy`
    fn pop_scope(&mut self);

    /// Gets all names `scope::xxx`
    fn find_all_in_scope(&self, scope: &str) -> Vec<(String, Rc<RefCell<Values>>, bool)>;
}

/// An environment which owns its own data
#[derive(Clone, PartialEq, Debug)]
pub struct Scope {
    names: LinkedList<HashMap<String, (Rc<RefCell<Values>>, bool)>>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            names: {
                let mut ll = LinkedList::<HashMap::<String, (Rc<RefCell<Values>>, bool)>>::new();
                ll.push_back(HashMap::<String, (Rc<RefCell<Values>>, bool)>::new());
                ll
            },
        }
    }

    /// Adds the entirety of `scope` as a child to this scope
    /// `scope` becomes sub scopes of this scope
    pub fn add_sub_scope(&mut self, mut scope: Scope) {
        scope.names.append(&mut self.names);
        self.names = scope.names
    }
}

impl Environment for Scope {
    fn find(&self, name: &str) -> Option<(Rc<RefCell<Values>>, bool)> {
        for map in &self.names {
            if let Some((val, mtble)) = map.get(name) {
                return Some((val.clone(), *mtble));
            }
        }
        None
        
    }

    fn add(&mut self, name: String, val: Values, mutable: bool) {
        if name != "_" {
            self.names.front_mut().unwrap()
                .insert(name, (Rc::new(RefCell::new(val)), mutable));
        }
    }

    fn add_direct(&mut self, name: String, val: Rc<RefCell<Values>>, mutable: bool) {
        if name != "_" {
            self.names.front_mut().unwrap()
                .insert(name, (val, mutable));
        }
    }

    fn update(&mut self, name: &str, val: Values) -> bool {
        for map in &mut self.names {
            match map.get_mut(name) {
                Some((v, true)) => {
                    *v.borrow_mut() = val;
                    return true;
                },
                Some((_, false)) => return false,
                None => (),

            }
        }
        false
    }

    fn new_scope(&mut self) {
        self.names.push_front(HashMap::<String, (Rc<RefCell<Values>>, bool)>::new());
    }

    fn pop_scope(&mut self) {
        self.names.pop_front();
    }

    fn cpy(&self) -> Scope {
        let mut cp = Scope::new();
        for map in &self.names {
            let mut mp_nw = HashMap::<String, (Rc<RefCell<Values>>, bool)>::new();
            for (k, (ptr, mtbl)) in map {
                mp_nw.insert(k.to_owned(), (Rc::new(RefCell::new(ptr.borrow().clone())), *mtbl));
            }
            cp.names.push_back(mp_nw);
        }
        cp
    }

    fn find_all_in_scope(&self, scope: &str) -> Vec<(String, Rc<RefCell<Values>>, bool)> {
        let mut list = Vec::<(String, Rc<RefCell<Values>>, bool)>::new();
        for map in &self.names {
            for (k, (val, val_mut)) in map {
                if k.contains(&format!("{}::", scope)[..]) {
                    list.push((k.clone(), val.clone(), *val_mut));
                }
            }
        }
        return list;
    }


}

/// An environment which wraps another environment to create a new sub scope
/// Handles the pushing and popping of the scope stack in its "constructor" and
/// "destructor"
/// RAII for `new_scope` and `pop_scope`
#[derive(Debug)]
pub struct ScopeProxy<'a> {
    scope: &'a mut dyn Environment,
}

impl<'a> ScopeProxy<'a> {
    pub fn new(parent: &'a mut impl Environment) -> ScopeProxy<'a> {
        parent.new_scope();
        ScopeProxy {
            scope: parent,
        }
    }

    pub fn from(parent: &'a mut Scope) -> ScopeProxy<'a> {
        parent.new_scope();
        ScopeProxy {
            scope: parent,
        }
    }
}

impl<'a> Drop for ScopeProxy<'a> {
    fn drop(&mut self) {
        self.scope.pop_scope();
    }
}

impl<'a> Environment for ScopeProxy<'a> {
    fn find(&self, name: &str) -> Option<(Rc<RefCell<Values>>, bool)> {
        self.scope.find(name)
    }
    fn add(&mut self, name: String, val: Values, mutable: bool) {
        self.scope.add(name, val, mutable)
    }
    fn update(&mut self, name: &str, val: Values) -> bool {
        self.scope.update(name, val)
    }
    fn new_scope(&mut self) {
        self.scope.new_scope()
    }
    fn pop_scope(&mut self) {
        self.scope.pop_scope()
    }
    fn cpy(&self) -> Scope {
        self.scope.cpy()
    }
    fn add_direct(&mut self, name: String, val: Rc<RefCell<Values>>, mutable: bool) {
        self.scope.add_direct(name, val, mutable);
    }
    fn find_all_in_scope(&self, scope: &str) -> Vec<(String, Rc<RefCell<Values>>, bool)> {
        self.scope.find_all_in_scope(scope)
    }
}

/// An environment with an immutable parent scope reference and
/// a mutable, data owning, child scope
/// Basically strings together an immutable and owned environment into 
/// one environment
#[derive(Debug)]
pub struct ImmuScope<'a> {
    parent: &'a dyn Environment,
    children: Scope,
}

impl<'a> ImmuScope<'a> {
    pub fn from(parent: &impl Environment) -> ImmuScope {
        ImmuScope {
            parent,
            children: Scope::new(),
        }
    }
}

impl<'a> Environment for ImmuScope<'a> {
    fn find(&self, name: &str) -> Option<(Rc<RefCell<Values>>, bool)> {
        if let res @ Some(_) = self.children.find(name) {
            res
        } else {
            self.parent.find(name)
        }
    }
    fn add(&mut self, name: String, val: Values, mutable: bool) {
        self.children.add(name, val, mutable)
    }
    fn update(&mut self, name: &str, val: Values) -> bool {
        self.children.update(name, val)
    }
    fn new_scope(&mut self) {
        self.children.new_scope()
    }
    fn pop_scope(&mut self) {
        self.children.pop_scope()
    }
    fn cpy(&self) -> Scope {
        let mut sc = self.parent.cpy();
        sc.add_sub_scope(self.children.cpy());
        sc
    }
    fn add_direct(&mut self, name: String, val: Rc<RefCell<Values>>, mutable: bool) {
        self.children.add_direct(name, val, mutable);
    }
    fn find_all_in_scope(&self, scope: &str) -> Vec<(String, Rc<RefCell<Values>>, bool)> {
        let mut x = self.children.find_all_in_scope(scope);
        x.append(&mut self.parent.find_all_in_scope(scope));
        x
    }
}