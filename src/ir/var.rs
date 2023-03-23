use super::Type;
use crate::parser::Variable as ParsedVariable;
use std::{
    cell::{Ref, RefCell},
    collections::HashSet,
    fmt::{Display, Error, Formatter},
    rc::Rc,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Variable(Rc<VarInner>);

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if let Some(v) = self.name() {
            write!(f, "{}", v)?;
        }

        write!(f, "<{}>", self.idx())
    }
}

impl Variable {
    pub fn new(name: Option<ParsedVariable>, idx: usize) -> Variable {
        Variable(Rc::new(VarInner {
            name,
            idx,
            types: RefCell::new(HashSet::new()),
        }))
    }

    pub fn add_type(&self, typ: Type) {
        let mut types = self.0.types.borrow_mut();
        types.insert(typ);
    }

    pub fn idx(&self) -> usize {
        self.0.idx
    }

    pub fn types(&self) -> Ref<'_, HashSet<Type>> {
        self.0.types.borrow()
    }

    pub fn name(&self) -> Option<&ParsedVariable> {
        self.0.name.as_ref()
    }
}

#[derive(Debug, PartialEq, Eq)]
struct VarInner {
    pub name: Option<ParsedVariable>,
    pub types: RefCell<HashSet<Type>>,
    pub idx: usize,
}

// #[derive(Debug, Clone, Copy, PartialEq)]
// pub struct VarRef(pub usize, pub usize);

// impl Display for VarRef {
//     fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
//         write!(f, "<{}, {}>", self.0, self.1)
//     }
// }
