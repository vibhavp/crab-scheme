use super::{IRExpression, Procedure, Variable, WithIRNodeNames};
use std::fmt::{Display, Error, Formatter};

#[derive(Debug, Clone)]
pub enum Operation<'a> {
    Define(Variable),
    Set(Variable, IRExpression<'a>),
    Application(Application),
    Nop,
}

impl<'a> Display for WithIRNodeNames<'a, &Operation<'a>> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match &self.1 {
            Operation::Define(v) => write!(f, "define {}", v),
            Operation::Set(v, e) => write!(f, "set {} = {}", v, WithIRNodeNames(self.0, e)),
            Operation::Application(app) => write!(f, "{}", WithIRNodeNames(self.0, app)),
            Operation::Nop => write!(f, "nop"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Application {
    pub procedure: CallTarget,
    pub args: Vec<Variable>,
}

#[derive(Debug, Clone)]
pub enum CallTarget {
    Static(Procedure),
    Variable(Variable),
}

impl<'a> Display for WithIRNodeNames<'a, &CallTarget> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match &self.1 {
            CallTarget::Static(p) => write!(f, "{}", WithIRNodeNames(self.0, p)),
            CallTarget::Variable(var) => write!(f, "{}", var),
        }
    }
}

impl<'a> Display for WithIRNodeNames<'a, &Application> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}(", WithIRNodeNames(self.0, &self.1.procedure))?;
        for (idx, arg) in self.1.args.iter().enumerate() {
            write!(f, "{}", arg)?;
            if idx != self.1.args.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MathOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Pow,
    Modulo,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BooleanOp {
    And,
    Or,
    Not,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BitwiseOp {
    And,
    Or,
    Not,
    Xor,
}

#[derive(Debug, Clone)]
pub enum PrimitiveApplication {
    Math(MathOp, Variable, Variable),
    Boolean(BooleanOp, Variable, Variable),
    BitwiseOp(BitwiseOp, Variable, Variable),
}
