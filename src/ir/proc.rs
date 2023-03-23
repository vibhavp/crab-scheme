use super::{IRNodeRef, Target, Variable, WithIRNodeNames};
use std::{
    fmt::{Display, Error, Formatter},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct Procedure {
    inner: Rc<ProcInner>,
}

impl Procedure {
    pub fn new(
        start: IRNodeRef,
        params: Vec<Variable>,
        rest: Option<Variable>,
        captured: Vec<Variable>,
    ) -> Self {
        Procedure {
            inner: Rc::new(ProcInner {
                start: Target::new(start),
                params,
                rest,
                captured,
            }),
        }
    }

    pub fn start(&self) -> IRNodeRef {
        self.inner.start.0.get()
    }

    pub fn params(&self) -> &[Variable] {
        &self.inner.params
    }

    pub fn rest(&self) -> Option<&Variable> {
        self.inner.rest.as_ref()
    }

    pub fn set_start(&mut self, node_ref: IRNodeRef) {
        self.inner.start.0.set(node_ref);
    }
}

#[derive(Debug)]
struct ProcInner {
    pub start: Target,
    pub params: Vec<Variable>,
    pub rest: Option<Variable>,
    pub captured: Vec<Variable>,
}

impl<'a> Display for WithIRNodeNames<'a, &Procedure> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(
            f,
            "procedure{}(",
            WithIRNodeNames(self.0, self.1.inner.start.clone()),
        )?;

        for (idx, arg_var) in self.1.params().iter().enumerate() {
            write!(f, "{}", arg_var)?;
            if idx != self.1.params().len() - 1 {
                write!(f, ", ")?;
            }
        }
        if let Some(rest_var) = self.1.rest() {
            write!(f, "{}", rest_var)?;
        }
        write!(f, ")")
    }
}
