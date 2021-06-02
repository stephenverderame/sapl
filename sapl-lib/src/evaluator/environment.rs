use crate::evaluator::Values;
use std::collections::HashMap;
use std::collections::LinkedList;

struct Internal {}

pub trait Environment {
    fn find(&self, name: String) -> Result<Values, String>;
    fn add(&mut self, name: String, val: Values, mutable: bool);
    fn update(&mut self, name: String, val: Values) -> bool;
    fn new_scope(&mut self, int: Internal);
    fn pop_scope(&mut self, int: Internal);
    fn cpy(&self) -> Scope;
}

#[derive(Clone, PartialEq, Debug)]
pub struct Scope {
    names: LinkedList<HashMap<String, (Values, bool)>>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            names: {
                let mut ll = LinkedList::<HashMap::<String, (Values, bool)>>::new();
                ll.push_back(HashMap::<String, (Values, bool)>::new());
                ll
            },
        }
    }
}

impl Environment for Scope {
    fn find(&self, name: String) -> Result<Values, String> {
        println!("{:?}", self.names.front());
        for map in &self.names {
            if let Some((val, _)) = map.get(&name) {
                return Ok(val.clone());
            }
        }
        Err(format!("Unknown name {}", name))
        
    }

    /// Adds `name` to the scope
    fn add(&mut self, name: String, val: Values, mutable: bool) {
        self.names.front_mut().unwrap().insert(name, (val, mutable));
        println!("Adding... {:?}", self.names.front());
    }

    /// Updates `name` in whichever scope it exists
    /// Returns `true` if successful, `false` otherwise
    fn update(&mut self, name: String, val: Values) -> bool {
        for map in &mut self.names {
            match map.get_mut(&name) {
                Some((v, true)) => {
                    *v = val;
                    return true;
                },
                Some((_, false)) => return false,
                None => (),

            }
        }
        false
    }

    fn new_scope(&mut self, _: Internal) {
        self.names.push_front(HashMap::<String, (Values, bool)>::new());
    }

    fn pop_scope(&mut self, _: Internal) {
        self.names.pop_front();
    }

    fn cpy(&self) -> Scope {
        self.clone()
    }


}

pub struct ScopeProxy<'a> {
    scope: &'a mut dyn Environment,
}

impl<'a> ScopeProxy<'a> {
    pub fn new(parent: &'a mut impl Environment) -> ScopeProxy<'a> {
        parent.new_scope(Internal {});
        ScopeProxy {
            scope: parent,
        }
    }
}

impl<'a> Drop for ScopeProxy<'a> {
    fn drop(&mut self) {
        self.scope.pop_scope(Internal {});
    }
}

impl<'a> Environment for ScopeProxy<'a> {
    fn find(&self, name: String) -> Result<Values, String> {
        self.scope.find(name)
    }
    fn add(&mut self, name: String, val: Values, mutable: bool) {
        self.scope.add(name, val, mutable)
    }
    fn update(&mut self, name: String, val: Values) -> bool {
        self.scope.update(name, val)
    }
    fn new_scope(&mut self, x: Internal) {
        self.scope.new_scope(x)
    }
    fn pop_scope(&mut self, x: Internal) {
        self.scope.pop_scope(x)
    }
    fn cpy(&self) -> Scope {
        self.scope.cpy()
    }
}