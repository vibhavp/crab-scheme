use super::{build_exprs::ExprBuildError, build_tree::BuildTree, translate::TranslationContext};
use crate::{ir::IRExpression, parser::Body};
use tracing::instrument;

impl<'a> BuildTree<'a> for Body<'a> {
    type Error = ExprBuildError;

    #[instrument]
    fn build(self, context: &mut TranslationContext<'a>) -> Result<IRExpression<'a>, Self::Error> {
        for def in self.0 {
            def.build(context)?;
        }

        let mut last = None;
        let exprs: Vec<_> = self.1.into();
        for expr in exprs {
            last = Some(expr.build(context)?);
        }

        Ok(last.unwrap())
    }
}
