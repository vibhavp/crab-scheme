use super::{build_tree::BuildTree, scope::LexicalScopeManager};
use crate::{
    ir::{
        Application, ControlFlow, IRExpression, IRNodeRef, Node, Operation, Procedure, Target,
        Variable, IR,
    },
    parser::{
        Binding, Body, Definition, Expression, Let, Variable as ParsedVariable, VariableDefinition,
    },
};
use slotmap::Key;
use thiserror::Error;

#[derive(Default)]
pub struct TranslationContext<'a> {
    lexical_scopes: LexicalScopeManager,
    ir: IR<'a>,

    cur_node: IRNodeRef,
    cur_node_idx: usize,

    cur_procedure: Option<Procedure>,
    procedures: Vec<Procedure>,
}

impl<'a> TranslationContext<'a> {
    pub fn cur_node_idx(&self) -> usize {
        self.cur_node_idx
    }

    pub fn with_node_added<F, O, E>(&mut self, f: F) -> Result<(O, bool), E>
    where
        F: FnOnce(&mut TranslationContext<'a>) -> Result<O, E>,
    {
        let last_idx = self.cur_node_idx;
        let result = f(self)?;
        Ok((result, last_idx == self.cur_node_idx))
    }

    pub fn with_new_scope<F, O>(&mut self, f: F) -> O
    where
        F: FnOnce(&mut TranslationContext<'a>) -> O,
    {
        self.lexical_scopes.push_scope();
        let r = f(self);
        self.lexical_scopes.pop_scope();
        r
    }

    pub fn with_new_procedure<F, E>(
        &mut self,
        params: Vec<ParsedVariable>,
        rest: Option<ParsedVariable>,
        f: F,
    ) -> Result<Procedure, E>
    where
        F: FnOnce(&mut TranslationContext<'a>) -> Result<(), E>,
    {
        let cur_node = std::mem::take(&mut self.cur_node);
        let cur_procedure = self.cur_procedure.take();

        let (procedure, _) = self.with_new_scope(|context| {
            let params: Vec<Variable> = params
                .into_iter()
                .map(|v| context.lexical_scopes.add_variable(v))
                .collect();
            let rest = rest.map(|v| context.lexical_scopes.add_variable(v));
            let new_proc = Procedure::new(Default::default(), params, rest, vec![]);
            context.cur_procedure = Some(new_proc.clone());
            Ok((new_proc, f(context)?))
        })?;

        self.cur_procedure = cur_procedure;
        self.procedures.push(procedure.clone());
        self.cur_node = cur_node;

        Ok(procedure)
    }

    pub fn push_define(
        &mut self,
        name: Option<ParsedVariable>,
        jump_from_last_node: bool,
    ) -> (Variable, IRNodeRef) {
        let v = if let Some(name) = name {
            self.lexical_scopes.add_variable(name)
        } else {
            self.lexical_scopes.add_artificial_variable()
        };

        (
            v.clone(),
            self.push_node(Node(Operation::Define(v), None), jump_from_last_node),
        )
    }

    pub fn push_op(&mut self, op: Operation<'a>, jump_from_last_node: bool) -> IRNodeRef {
        self.push_node(Node(op, None), jump_from_last_node)
    }

    pub fn push_node(&mut self, node: Node<'a>, jump_from_last_node: bool) -> IRNodeRef {
        let node_ref = self.ir.nodes.insert(node);
        self.ir.node_names.insert(node_ref, self.cur_node_idx);

        if let Some(last_node) = self.ir.nodes.get_mut(self.cur_node) {
            if jump_from_last_node {
                last_node.1 = Some(ControlFlow::Goto(Target::new(node_ref)));
            }
        } else if self.ir.start.is_null() {
            self.ir.start = node_ref;
        }

        if let Some(ref mut proc) = self.cur_procedure {
            if proc.start().is_null() {
                proc.set_start(node_ref);
            }
        }

        self.cur_node = node_ref;
        self.cur_node_idx += 1;

        node_ref
    }
}

impl<'a> BuildTree<'a> for VariableDefinition<'a> {
    type Error = ExprBuildError;

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

#[derive(Debug, Error)]
pub enum ExprBuildError {
    #[error("undefined variable `{0}`")]
    UnknownVariable(ParsedVariable),
}

impl<'a> BuildTree<'a> for Expression<'a> {
    type Error = ExprBuildError;

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
                    procedure: proc_var,
                    args: args_vars,
                }))
            }
            _ => todo!(),
        }
    }
}

impl<'a> BuildTree<'a> for Let<'a> {
    type Error = ExprBuildError;

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
            _ => todo!(),
        })
    }
}

impl<'a> BuildTree<'a> for Definition<'a> {
    type Error = ExprBuildError;

    fn build(self, context: &mut TranslationContext<'a>) -> Result<IRExpression<'a>, Self::Error> {
        match self {
            Self::Variable(v) => v.build(context),
            _ => todo!(),
        }
    }
}

impl<'a> BuildTree<'a> for Body<'a> {
    type Error = ExprBuildError;

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

#[cfg(test)]
mod test {
    use nom::error::VerboseError;

    use crate::{
        ir::WithIRNodeNames,
        parser::{program, Definition, Form},
    };

    use super::{BuildTree, TranslationContext};

    #[test]
    fn if_ir() {
        let (_, mut p) = program::<VerboseError<&str>>("(if \"cond\" \"then\" \"else\")")
            .expect("should parse form");

        let mut context = TranslationContext::default();

        let f = p.0.pop().unwrap();
        match f {
            Form::Expression(e) => e.build(&mut context).unwrap(),
            _ => todo!(),
        };

        println!("{}", context.ir);
    }

    #[test]
    fn define_var_ir() {
        let (_, mut p) =
            program::<VerboseError<&str>>("(define a \"cond\")").expect("should parse form");

        let mut context = TranslationContext::default();

        let f = p.0.pop().unwrap();
        match f {
            Form::Definition(Definition::Variable(v)) => v.build(&mut context).unwrap(),
            _ => todo!(),
        };

        println!("{}", context.ir);
    }

    #[test]
    fn define_func_ir() {
        let (_, mut p) =
            program::<VerboseError<&str>>("(define (a b) \"foo\")").expect("should parse form");

        let mut context = TranslationContext::default();

        let f = p.0.pop().unwrap();
        match f {
            Form::Definition(Definition::Variable(v)) => v.build(&mut context).unwrap(),
            _ => todo!(),
        };

        println!("{}", context.ir);
        for proc in context.procedures {
            println!("{}:", WithIRNodeNames(&context.ir.node_names, &proc));
            println!("{}", context.ir.display_from(proc.start()));
        }
    }
}
