use super::{ops::Operation, ControlFlow, WithIRNodeNames};
use std::fmt::{Display, Error, Formatter};

#[derive(Debug, Clone)]
pub struct Node<'a>(pub Operation<'a>, pub Option<ControlFlow<'a>>);

impl<'a> Display for WithIRNodeNames<'a, &Node<'a>> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", WithIRNodeNames(self.0, &self.1 .0))?;
        if let Some(ref control) = self.1 .1 {
            write!(f, "\t{}", WithIRNodeNames(self.0, control.clone()))?;
        }
        Ok(())
    }
}
