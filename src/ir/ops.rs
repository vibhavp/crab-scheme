use super::{IRExpression, Variable, WithIRNodeNames};
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
            Operation::Application(app) => write!(f, "{}", app),
            Operation::Nop => write!(f, "nop"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Application {
    pub procedure: Variable,
    pub args: Vec<Variable>,
}

impl Display for Application {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}(", self.procedure)?;
        for (idx, arg) in self.args.iter().enumerate() {
            write!(f, "{}", arg)?;
            if idx != self.args.len() - 1 {
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

    BitwiseAnd,
    BitwiseOr,
    BitwiseNot,
    BitwiseXor,

    LogicalAnd,
    LogicalOr,
    LogicalNot,
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Math(MathOp, Variable, Variable),
}
