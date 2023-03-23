use std::{
    cell::Cell,
    fmt::{Display, Error, Formatter},
    rc::Rc,
};

use super::{IRExpression, IRNodeRef, Variable, WithIRNodeNames};

#[derive(Debug, Default, Clone)]
pub struct Target(pub Rc<Cell<IRNodeRef>>);

impl Target {
    pub fn new(r: IRNodeRef) -> Self {
        Target(Rc::new(Cell::new(r)))
    }
}

impl<'a> Display for WithIRNodeNames<'a, Target> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let name = self.0.get(self.1 .0.get()).ok_or(Error)?;
        write!(f, "@{}", name)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ControlFlow<'a> {
    Goto(Target),
    BranchIf {
        cond: Variable,
        target: Target,
        finally: Target,
    },
    Branch {
        cond: Variable,
        target_then: Target,
        target_else: Target,
        finally: Target,
    },
    Return(IRExpression<'a>),
}

impl<'a> Display for WithIRNodeNames<'a, ControlFlow<'a>> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match &self.1 {
            ControlFlow::Goto(t) => write!(f, "goto {}", WithIRNodeNames(self.0, t.clone())),
            ControlFlow::BranchIf {
                cond,
                target,
                finally,
            } => {
                write!(
                    f,
                    "if {} then goto {} finally {}",
                    cond,
                    WithIRNodeNames(self.0, target.clone()),
                    WithIRNodeNames(self.0, finally.clone())
                )
            }
            ControlFlow::Branch {
                cond,
                target_then,
                target_else,
                finally,
            } => write!(
                f,
                "if {} then {} else {} finally {}",
                cond,
                WithIRNodeNames(self.0, target_then.clone()),
                WithIRNodeNames(self.0, target_else.clone()),
                WithIRNodeNames(self.0, finally.clone())
            ),
            ControlFlow::Return(v) => write!(f, "return {}", WithIRNodeNames(self.0, v)),
        }
    }
}
