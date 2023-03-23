use crate::{ir::Variable, parser::Variable as ParsedVariable};
use std::collections::HashMap;

pub(super) struct LexicalScopeManager {
    scopes: Vec<Scope>,
    cur_idx: usize,
}

#[derive(Default)]
struct Scope {
    variables: Vec<Variable>,
    names: HashMap<ParsedVariable, usize>,
}

impl Default for LexicalScopeManager {
    fn default() -> Self {
        Self {
            scopes: vec![Default::default()],
            cur_idx: 0,
        }
    }
}

impl LexicalScopeManager {
    pub fn push_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    pub fn pop_scope(&mut self) {
        self.scopes
            .pop()
            .expect("at least one lexical scope should always exist");
    }

    fn incr_cur_idx(&mut self) {
        self.cur_idx += 1;
    }

    pub fn add_variable(&mut self, var: ParsedVariable) -> Variable {
        let scope = self
            .scopes
            .last_mut()
            .expect("at least one lexical scope should always exist");

        let v = Variable::new(Some(var.clone()), self.cur_idx);
        scope.variables.push(v.clone());
        let var_idx = scope.variables.len() - 1;
        scope.names.insert(var, var_idx);
        self.incr_cur_idx();

        v
    }

    pub fn add_artificial_variable(&mut self) -> Variable {
        let scope = self
            .scopes
            .last_mut()
            .expect("at least one lexical scope should always exist");
        let v = Variable::new(None, self.cur_idx);
        scope.variables.push(v.clone());
        self.incr_cur_idx();

        v
    }

    pub fn lookup_variable(&self, var: &ParsedVariable) -> Option<Variable> {
        self.scopes.iter().rev().find_map(|scope| {
            scope
                .names
                .get(var)
                .map(|idx| scope.variables[*idx].clone())
        })
    }
}

#[cfg(test)]
mod test {
    use crate::parser::Variable as ParsedVariable;

    use super::LexicalScopeManager;

    #[test]
    fn lexical_scope_basic() {
        let mut manager = LexicalScopeManager::default();

        let v1 = ParsedVariable::InitialSubsequent("foo".into());
        let v2 = ParsedVariable::InitialSubsequent("bar".into());
        let v3 = ParsedVariable::InitialSubsequent("baz".into());

        let ir_v1 = manager.add_variable(v1.clone());
        assert_eq!(manager.lookup_variable(&v1), Some(ir_v1));

        manager.push_scope();
        let ir_v2 = manager.add_variable(v2.clone());
        assert_eq!(manager.lookup_variable(&v2), Some(ir_v2));

        let ir_v3 = manager.add_variable(v3.clone());
        assert_eq!(manager.lookup_variable(&v3), Some(ir_v3));

        manager.pop_scope();
        assert_eq!(manager.lookup_variable(&v3), None);
        assert_eq!(manager.lookup_variable(&v2), None);

        manager.lookup_variable(&v1);
    }

    #[test]
    #[should_panic(expected = "at least one lexical scope should always exist")]
    fn invalid_lexical_scope_pop() {
        let mut manager = LexicalScopeManager::default();
        manager.pop_scope();
        manager.pop_scope();
    }
}
