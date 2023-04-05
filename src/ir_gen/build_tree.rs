use melior::ir::operation::ResultValue;
use thiserror::Error;

use super::translate::{MLIRTranslationContext, TranslationContext};
use crate::ir::{IRExpression, Operation, Variable};

pub trait BuildTree<'a>: Sized {
    type Error;

    fn build(self, context: &mut TranslationContext<'a>) -> Result<IRExpression<'a>, Self::Error>;

    fn build_node_added(
        self,
        context: &mut TranslationContext<'a>,
    ) -> Result<(IRExpression<'a>, bool), Self::Error> {
        context.with_node_added(|context| self.build(context))
    }

    fn build_to_variable(
        self,
        context: &mut TranslationContext<'a>,
        jump_from_last_node: bool,
    ) -> Result<Variable, Self::Error> {
        let result = self.build(context)?;
        if let IRExpression::Variable(v) = result {
            Ok(v)
        } else {
            let (var, _) = context.push_define(None, jump_from_last_node);
            context.push_op(Operation::Set(var.clone(), result), true);
            Ok(var)
        }
    }
}

pub trait BuildMLIR: Sized {
    type Error;

    fn build_mlir<'ctx>(
        self,
        context: MLIRTranslationContext<'ctx>,
    ) -> Result<ResultValue<'_>, Self::Error>;
}
