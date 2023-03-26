use super::{build_exprs::ExprBuildError, build_tree::BuildTree, translate::TranslationContext};
use crate::{
    ir::{ControlFlow, IRExpression, Node, Operation},
    parser::{Definition, VariableDefinition},
};
use tracing::instrument;

impl<'a> BuildTree<'a> for VariableDefinition<'a> {
    type Error = ExprBuildError;

    #[instrument]
    fn build(self, context: &mut TranslationContext<'a>) -> Result<IRExpression<'a>, Self::Error> {
        let var = match self {
            Self::SimpleDefine { var, expr } => {
                let (var, _) = context.push_define(Some(var), true);
                let e = expr.build(context)?;
                context.push_op(Operation::Set(var.clone(), e), true);

                var
            }
            Self::Function {
                name,
                args,
                rest,
                body,
            } => {
                let (var, _) = context.push_define(Some(name), true);
                let proc = context.with_new_procedure(args, rest, |context| {
                    let result = body.build(context)?;
                    context.push_node(
                        Node(Operation::Nop, Some(ControlFlow::Return(result))),
                        true,
                    );
                    Ok(())
                })?;
                context.push_op(
                    Operation::Set(var.clone(), IRExpression::Procedure(proc)),
                    true,
                );
                var
            }
        };

        Ok(IRExpression::Variable(var))
    }
}

impl<'a> BuildTree<'a> for Definition<'a> {
    type Error = ExprBuildError;

    #[instrument]
    fn build(self, context: &mut TranslationContext<'a>) -> Result<IRExpression<'a>, Self::Error> {
        match self {
            Self::Variable(v) => v.build(context),
            Self::Begin(defs) => {
                let mut last = None;

                for def in defs {
                    last = Some(def.build(context)?);
                }
                Ok(last.unwrap())
            }
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ir::WithIRNodeNames,
        ir_gen::{build_tree::BuildTree, translate::TranslationContext},
        parser::program,
    };
    use nom::error::VerboseError;

    #[test]
    fn define_var_ir() {
        let (_, p) =
            program::<_, VerboseError<&str>>("(define a \"cond\")").expect("should parse form");

        let mut context = TranslationContext::default();
        p.build(&mut context).expect("should succesfully compile");

        println!("{}", context.ir());
    }

    #[test]
    fn define_func_ir() {
        let (_, p) =
            program::<_, VerboseError<&str>>("(define (a b) \"foo\")").expect("should parse form");

        let mut context = TranslationContext::default();

        p.build(&mut context).expect("should succesfully compile");

        println!("{}", context.ir());
        for proc in context.procedures() {
            println!("{}:", WithIRNodeNames(&context.ir().node_names, proc));
            println!("{}", context.ir().display_from(proc.start()));
        }
    }
}
