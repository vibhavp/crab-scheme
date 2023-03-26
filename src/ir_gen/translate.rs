use super::scope::LexicalScopeManager;
use crate::{
    ir::{ControlFlow, IRNodeRef, Node, Operation, Procedure, Target, Variable, IR},
    parser::Variable as ParsedVariable,
};
use slotmap::Key;
use tracing::instrument;

#[derive(Debug, Default)]
pub struct TranslationContext<'a> {
    pub lexical_scopes: LexicalScopeManager,
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

    pub fn ir(&self) -> &IR<'a> {
        &self.ir
    }

    pub fn procedures(&self) -> &[Procedure] {
        &self.procedures
    }

    #[instrument[skip(f)]]
    pub fn with_node_added<F, O, E>(&mut self, f: F) -> Result<(O, bool), E>
    where
        F: FnOnce(&mut TranslationContext<'a>) -> Result<O, E>,
    {
        let last_idx = self.cur_node_idx;
        let result = f(self)?;
        Ok((result, last_idx == self.cur_node_idx))
    }

    #[instrument[skip(f)]]
    pub fn with_new_scope<F, O>(&mut self, f: F) -> O
    where
        F: FnOnce(&mut TranslationContext<'a>) -> O,
    {
        self.lexical_scopes.push_scope();
        let r = f(self);
        self.lexical_scopes.pop_scope();
        r
    }

    #[instrument(skip(f))]
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
            debug_assert!(new_proc.start().is_null());
            context.cur_procedure = Some(new_proc.clone());
            Ok((new_proc, f(context)?))
        })?;

        debug_assert!(!procedure.start().is_null());

        self.cur_procedure = cur_procedure;
        self.procedures.push(procedure.clone());
        self.cur_node = cur_node;

        Ok(procedure)
    }

    #[instrument]
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

    #[instrument]
    pub fn push_op(&mut self, op: Operation<'a>, jump_from_last_node: bool) -> IRNodeRef {
        self.push_node(Node(op, None), jump_from_last_node)
    }

    #[instrument]
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

#[cfg(test)]
mod test {
    use super::TranslationContext;
    use crate::{ir::WithIRNodeNames, ir_gen::build_tree::BuildTree, parser::program};
    use nom::error::VerboseError;

    #[test]
    fn program_ir() {
        let (_, p) = program::<_, VerboseError<&str>>(include_str!("../../test_data/fft.ss"))
            .expect("should parse file");
        let mut context = TranslationContext::default();
        p.build(&mut context).expect("should succesfully compile");

        println!("{}", context.ir);
        for proc in context.procedures {
            println!("{}:", WithIRNodeNames(&context.ir.node_names, &proc));
            println!("{}", context.ir.display_from(proc.start()));
        }
    }
}
