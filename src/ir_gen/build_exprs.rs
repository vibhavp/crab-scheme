use crate::{
    ir::{Application, CallTarget, ControlFlow, IRExpression, Node, Operation, Target},
    parser::{Binding, Expression, Formals, Identifier, Let, Variable as ParsedVariable},
};
use thiserror::Error;
use tracing::instrument;

use super::{build_tree::BuildTree, translate::TranslationContext};

#[derive(Debug, Error)]
pub enum ExprBuildError {
    #[error("undefined variable `{0}`")]
    UnknownVariable(ParsedVariable),
}

impl<'a> BuildTree<'a> for Expression<'a> {
    type Error = ExprBuildError;

    #[instrument]
    fn build(self, context: &mut TranslationContext<'a>) -> Result<IRExpression<'a>, Self::Error> {
        match self {
            Expression::Constant(c) => Ok(IRExpression::Constant(c.into())),
            Expression::Quote(d) => Ok(IRExpression::Constant(d)),
            Expression::Variable(v) => {
                let var = context
                    .lexical_scopes
                    .lookup_variable(&v)
                    .ok_or(ExprBuildError::UnknownVariable(v))?;

                Ok(IRExpression::Variable(var))
            }
            Expression::If {
                cond,
                then,
                else_expr,
            } => {
                let (result_var, _) = context.push_define(None, true);
                let (cond_var, _) = context.push_define(None, true);

                let target_then = Target::default();
                let target_else = Target::default();
                let target_finally = Target::default();

                {
                    let branch_node = Node(
                        Operation::Set(cond_var.clone(), cond.build(context)?),
                        if else_expr.is_some() {
                            Some(ControlFlow::Branch {
                                cond: cond_var,
                                target_then: target_then.clone(),
                                target_else: target_else.clone(),
                                finally: target_finally.clone(),
                            })
                        } else {
                            Some(ControlFlow::BranchIf {
                                cond: cond_var,
                                target: target_then.clone(),
                                finally: target_finally.clone(),
                            })
                        },
                    );
                    context.push_node(branch_node, true);
                }

                {
                    let then_node_ref = {
                        let then_start = context.push_op(Operation::Nop, false);

                        let (then_result, have_then_target) = then.build_node_added(context)?;

                        let cur_node = context.push_node(
                            Node(
                                Operation::Set(result_var.clone(), then_result),
                                Some(ControlFlow::Goto(target_finally.clone())),
                            ),
                            true,
                        );
                        if have_then_target {
                            then_start
                        } else {
                            cur_node
                        }
                    };
                    target_then.0.set(then_node_ref);
                }

                {
                    if let Some(else_expr) = else_expr {
                        let else_node_ref = {
                            let else_start = context.push_op(Operation::Nop, false);
                            let (else_result, have_else_target) =
                                else_expr.build_node_added(context)?;
                            let cur_node = context.push_node(
                                Node(
                                    Operation::Set(result_var.clone(), else_result),
                                    Some(ControlFlow::Goto(target_finally.clone())),
                                ),
                                true,
                            );

                            if have_else_target {
                                else_start
                            } else {
                                cur_node
                            }
                        };
                        target_else.0.set(else_node_ref);
                    }
                }
                let finally_node_ref = context.push_op(Operation::Nop, false);
                target_finally.0.set(finally_node_ref);

                Ok(IRExpression::Variable(result_var))
            }
            Expression::Set { var, expr } => {
                let var = context
                    .lexical_scopes
                    .lookup_variable(&var)
                    .ok_or(ExprBuildError::UnknownVariable(var))?;
                let value = expr.build(context)?;
                context.push_op(Operation::Set(var.clone(), value), true);
                Ok(IRExpression::Variable(var))
            }
            Expression::Let(let_expr) => Ok(let_expr.build(context)?),
            Expression::Application(app) => {
                let mut call: Vec<_> = app.into();
                // match call[0] {
                //     Expression::Variable(Identifier::Plus) => {}
                //     Expression::Variable(Identifier::)
                //     }
                // if let Expression::Variable(ident) = &call[0] {
                //     match ident {}
                // }
                let (proc_var, _) = context.push_define(None, true);
                {
                    let proc_expr = call.remove(0).build(context)?;
                    context.push_op(Operation::Set(proc_var.clone(), proc_expr), true);
                }

                let args_vars = call
                    .into_iter()
                    .map(|expr| {
                        let (v, _) = context.push_define(None, true);
                        let op = Operation::Set(v.clone(), expr.build(context)?);
                        context.push_op(op, true);

                        Ok(v)
                    })
                    .collect::<Result<_, _>>()?;

                Ok(IRExpression::Application(Application {
                    procedure: CallTarget::Variable(proc_var),
                    args: args_vars,
                }))
            }
            Expression::Lambda { formals, body } => {
                let (args, rest) = match formals {
                    Formals::Rest(v) => (vec![], Some(v)),
                    Formals::Variables(vars) => (vars, None),
                    Formals::VariablesRest(vars, rest) => (vars.into(), Some(rest)),
                };

                let proc = context.with_new_procedure(args, rest, |context| {
                    let result = body.build(context)?;
                    context.push_node(
                        Node(Operation::Nop, Some(ControlFlow::Return(result))),
                        true,
                    );
                    Ok(())
                })?;
                Ok(IRExpression::Procedure(proc))
            }
            _ => todo!(),
        }
    }
}

impl<'a> BuildTree<'a> for Let<'a> {
    type Error = ExprBuildError;

    #[instrument]
    fn build(self, context: &mut TranslationContext<'a>) -> Result<IRExpression<'a>, Self::Error> {
        context.with_new_scope(|context| match self {
            Self::SimpleLet(bindings, body) => {
                let mut worklist = Vec::new();

                for Binding { var, expr } in bindings.into_iter() {
                    let (ir_var, _) = context.push_define(None, true);
                    worklist.push((var, ir_var.clone()));

                    let binding_expr = expr.build(context)?;
                    context.push_op(Operation::Set(ir_var, binding_expr), true);
                }

                for (var, ir_var) in worklist.into_iter() {
                    let varref = context.lexical_scopes.add_variable(var);
                    context.push_op(Operation::Set(varref, IRExpression::Variable(ir_var)), true);
                }

                body.build(context)
            }
            Self::NamedLet(variable, bindings, body) => context.with_new_scope(|context| {
                let (var, _) = context.push_define(Some(variable), true);
                let (vars, exprs): (Vec<_>, Vec<_>) =
                    bindings.into_iter().map(|b| (b.var, b.expr)).unzip();

                let proc = context.with_new_procedure(vars, None, |context| {
                    let result = body.build(context)?;
                    context.push_node(
                        Node(Operation::Nop, Some(ControlFlow::Return(result))),
                        true,
                    );
                    Ok(())
                })?;
                context.push_op(
                    Operation::Set(var, IRExpression::Procedure(proc.clone())),
                    true,
                );
                let arg_vars = exprs
                    .into_iter()
                    .map(|expr| {
                        let (arg_var, _) = context.push_define(None, true);
                        let arg_val = expr.build(context)?;
                        context.push_op(Operation::Set(arg_var.clone(), arg_val), true);
                        Ok(arg_var)
                    })
                    .collect::<Result<_, _>>()?;
                Ok(IRExpression::Application(Application {
                    procedure: CallTarget::Static(proc),
                    args: arg_vars,
                }))
            }),
        })
    }
}

#[cfg(test)]
mod test {
    use nom::error::VerboseError;

    use crate::{
        ir_gen::{build_tree::BuildTree, translate::TranslationContext},
        parser::program,
    };

    #[test]
    fn if_ir() {
        let (_, p) = program::<_, VerboseError<&str>>("(if \"cond\" \"then\" \"else\")")
            .expect("should parse form");

        let mut context = TranslationContext::default();
        p.build(&mut context).expect("should succesfully compile");

        println!("{}", context.ir());
    }
}
