mod control;
mod node;
mod ops;
mod proc;
mod query;
mod types;
// pub mod validate;
mod var;

use slotmap::{new_key_type, HopSlotMap, SecondaryMap};
use std::fmt::{Display, Error, Formatter};

pub use self::control::{ControlFlow, Target};
use self::query::{IRQuery, TraversalError};
pub use node::Node;
pub use ops::{Application, CallTarget, Operation, PrimitiveApplication};
pub use proc::Procedure;
pub use types::Type;
pub use var::Variable;

use crate::parser::Datum;

pub struct WithIRNodeNames<'a, T>(pub &'a SecondaryMap<IRNodeRef, usize>, pub T);

new_key_type! {
    pub struct IRNodeRef;
}

impl<'a> Display for WithIRNodeNames<'a, IRNodeRef> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let name = self.0.get(self.1).ok_or(Error)?;
        write!(f, "@{}", name)?;
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct IR<'a> {
    pub start: IRNodeRef,
    pub nodes: HopSlotMap<IRNodeRef, Node<'a>>,
    pub node_names: SecondaryMap<IRNodeRef, usize>,
}

impl<'a> IR<'a> {
    pub fn traverse<F, B>(&'a self, visitor: F) -> Result<Option<B>, TraversalError>
    where
        F: FnMut(usize, &Node<'a>) -> std::ops::ControlFlow<B, ()>,
    {
        self.traverse_from(self.start, visitor)
    }

    pub fn display_from(&self, node: IRNodeRef) -> DisplayFrom<'_, 'a> {
        DisplayFrom {
            ir: self,
            start: node,
        }
    }
}

pub struct DisplayFrom<'a, 'b> {
    ir: &'a IR<'b>,
    start: IRNodeRef,
}

impl<'a, 'b> Display for DisplayFrom<'a, 'b> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let break_err = self.ir.traverse_from(self.start, move |node_name, node| {
            if let Err(e) = writeln!(
                f,
                "{}\t{}",
                node_name,
                WithIRNodeNames(&self.ir.node_names, node)
            ) {
                std::ops::ControlFlow::Break(e)
            } else {
                std::ops::ControlFlow::Continue(())
            }
        });
        if let Ok(Some(err)) = break_err {
            Err(err)
        } else {
            Ok(())
        }
    }
}

impl<'a> Display for IR<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.display_from(self.start))
    }
}

#[derive(Debug, Clone)]
pub enum IRExpression<'a> {
    Primitive(PrimitiveApplication),
    Procedure(Procedure),
    Variable(Variable),
    Constant(Datum<'a>),
    Application(Application),
}

impl<'a> Display for WithIRNodeNames<'a, &IRExpression<'a>> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self.1 {
            IRExpression::Variable(v) => write!(f, "{}", v),
            IRExpression::Constant(d) => write!(f, "{}", d),
            IRExpression::Procedure(p) => write!(f, "{}", WithIRNodeNames(self.0, p)),
            IRExpression::Application(a) => write!(f, "{}", WithIRNodeNames(self.0, a)),
            _ => todo!(),
        }
    }
}
