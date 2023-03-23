use super::translate::TranslationContext;
use crate::ir::IRExpression;

pub trait BuildTree<'a>: Sized {
    type Error;

    fn build(self, context: &mut TranslationContext<'a>) -> Result<IRExpression<'a>, Self::Error>;

    fn build_node_added(
        self,
        context: &mut TranslationContext<'a>,
    ) -> Result<(IRExpression<'a>, bool), Self::Error> {
        context.with_node_added(|context| self.build(context))
    }
}
