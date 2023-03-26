use tracing::instrument;

use super::{build_exprs::ExprBuildError, build_tree::BuildTree, translate::TranslationContext};
use crate::{
    ir::IRExpression,
    parser::{Form, Program},
};

impl<'a> BuildTree<'a> for Program<'a> {
    type Error = ExprBuildError;

    #[instrument]
    fn build(self, context: &mut TranslationContext<'a>) -> Result<IRExpression<'a>, Self::Error> {
        let mut last = None;
        for expr in self.0 {
            last = Some(expr.build(context)?);
        }
        Ok(last.unwrap())
    }
}

impl<'a> BuildTree<'a> for Form<'a> {
    type Error = ExprBuildError;

    fn build(self, context: &mut TranslationContext<'a>) -> Result<IRExpression<'a>, Self::Error> {
        match self {
            Self::Definition(d) => d.build(context),
            Self::Expression(e) => e.build(context),
        }
    }
}
