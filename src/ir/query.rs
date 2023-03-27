use super::{IRNodeRef, Node, IR};
use crate::ir::ControlFlow;
use slotmap::SecondaryMap;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum TraversalError {
    #[error("invalid node reference")]
    InvalidNodeRef(IRNodeRef),
}

pub trait IRQuery {
    fn resolve_node(&self, node: IRNodeRef) -> Option<&Node>;
    fn resolve_node_name(&self, node: IRNodeRef) -> Option<usize>;

    fn traverse_from<'a, F, B>(
        &'a self,
        node: IRNodeRef,
        mut visitor: F,
    ) -> Result<Option<B>, TraversalError>
    where
        F: FnMut(usize, &Node<'a>) -> std::ops::ControlFlow<B, ()>,
    {
        let mut stack = Vec::new();
        let mut visited_nodes = SecondaryMap::new();

        fn push_new_node(
            stack: &mut Vec<IRNodeRef>,
            map: &mut SecondaryMap<IRNodeRef, ()>,
            node_ref: IRNodeRef,
        ) {
            if !map.contains_key(node_ref) {
                stack.push(node_ref);
                map.insert(node_ref, ());
            }
        }

        push_new_node(&mut stack, &mut visited_nodes, node);

        while let Some(node_ref) = stack.pop() {
            let node = self
                .resolve_node(node_ref)
                .ok_or(TraversalError::InvalidNodeRef(node_ref))?;
            let node_name = self
                .resolve_node_name(node_ref)
                .ok_or(TraversalError::InvalidNodeRef(node_ref))?;

            if let std::ops::ControlFlow::Break(b) = visitor(node_name, node) {
                return Ok(Some(b));
            }

            match &node.1 {
                Some(ControlFlow::Goto(target)) => {
                    push_new_node(&mut stack, &mut visited_nodes, target.0.get());
                }
                Some(ControlFlow::BranchIf {
                    cond,
                    target,
                    finally,
                }) => {
                    push_new_node(&mut stack, &mut visited_nodes, finally.0.get());
                    push_new_node(&mut stack, &mut visited_nodes, target.0.get());
                }
                Some(ControlFlow::Branch {
                    cond,
                    target_then,
                    target_else,
                    finally,
                }) => {
                    push_new_node(&mut stack, &mut visited_nodes, finally.0.get());
                    push_new_node(&mut stack, &mut visited_nodes, target_else.0.get());
                    push_new_node(&mut stack, &mut visited_nodes, target_then.0.get());
                }
                _ => {}
            }
        }

        Ok(None)
    }
}

impl<'a> IRQuery for IR<'a> {
    fn resolve_node(&self, node: IRNodeRef) -> Option<&Node> {
        self.nodes.get(node)
    }

    fn resolve_node_name(&self, node: IRNodeRef) -> Option<usize> {
        self.node_names.get(node).copied()
    }
}
