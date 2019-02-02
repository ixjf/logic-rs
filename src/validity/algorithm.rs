use super::truth_tree::*;
use crate::parser::{Predicate, SingularTerm, Statement, Subscript, Term, Variable};
use std::cmp::Ordering;
use std::collections::BinaryHeap;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Rule {
    QuantifierExchange,
    ExistentialQuantifier,
    UniversalQuantifier,
    DoubleNegation,
    Conjunction,
    NegationOfConditional,
    NegationOfDisjunction,
    Conditional,
    NegationOfConjunction,
    Disjunction,
}

#[derive(PartialEq, Eq)]
struct QueueNode {
    statement_id: Id,
    statement: Statement,
    rule: Option<Rule>,
    branch_id: Id,
}

impl Ord for QueueNode {
    fn cmp(&self, other: &QueueNode) -> Ordering {
        // Priority order, top should come first, bottom last
        let rule_priority_order: [Rule; 10] = [
            Rule::QuantifierExchange,
            Rule::ExistentialQuantifier,
            Rule::UniversalQuantifier,
            Rule::DoubleNegation,
            Rule::Conjunction,
            Rule::NegationOfConditional,
            Rule::NegationOfDisjunction,
            Rule::Conditional,
            Rule::NegationOfConjunction,
            Rule::Disjunction,
        ];

        match (&self.rule, &other.rule) {
            // Atomic statements should come first in the queue
            (Some(_), None) => Ordering::Less,
            (None, Some(_)) => Ordering::Greater,
            // If rule B comes after rule A in rule_priority_order,
            // then A should come first
            (Some(rule_a), Some(rule_b)) => rule_priority_order
                .iter()
                .position(|x| *x == *rule_b)
                .unwrap()
                .cmp(
                    &rule_priority_order
                        .iter()
                        .position(|x| *x == *rule_a)
                        .unwrap(),
                ),
            (None, None) => Ordering::Equal,
        }
    }
}

impl PartialOrd for QueueNode {
    fn partial_cmp(&self, other: &QueueNode) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

enum DerivedRuleWhatdo {
    AddToExistingBranches,
    AsNewBranches,
}

struct RuleDeriveResult {
    statements: Vec<Statement>,
    whatdo: DerivedRuleWhatdo,
}

// FIXME Maybe this should be in truth_tree.rs?
#[derive(Clone, Debug)]
pub struct TreeNodeLocation(pub Id, pub Id); // Statement ID, Branch ID

#[derive(Clone, Debug)]
pub struct TreeNode {
    pub statement: Statement,
    pub derived_from: Option<(TreeNodeLocation, Rule)>,
}

pub struct TruthTreeMethod {
    tree: TruthTree<TreeNode>,
}

impl TruthTreeMethod {
    pub fn new(statements: &Vec<Statement>) -> Self {
        TruthTreeMethod {
            tree: TruthTree::new(Branch::new(
                statements
                    .iter()
                    .map(|x| TreeNode {
                        statement: x.clone(),
                        derived_from: None,
                    })
                    .collect(),
            )),
        }
    }

    pub fn compute(mut self) -> TruthTree<TreeNode> {
        let mut queue = BinaryHeap::new();

        // Populate the queue with the main trunk
        for (statement_id, tree_node) in self
            .tree
            .branch_from_id(&self.tree.main_trunk_id())
            .statements()
        {
            let rule = self.matches_some_rule(&tree_node.statement);

            queue.push(QueueNode {
                statement_id: statement_id.clone(),
                statement: tree_node.statement.clone(),
                rule,
                branch_id: self.tree.main_trunk_id(),
            });
        }

        // All nodes on the queue are already on the tree
        // A node represents some statement that needs to have a rule applied to it
        while let Some(QueueNode {
            statement_id,
            statement,
            rule,
            branch_id,
        }) = queue.pop()
        {
            if self
                .tree
                .traverse_downwards_branch_ids(&self.tree.main_trunk_id())
                .filter(|x| {
                    !self.tree.branch_from_id(&x).is_closed() && self.tree.branch_is_last_child(&x)
                })
                .count()
                == 0
            {
                // There are no open branches in the entire tree, stop
                break;
            }

            match rule {
                Some(rule) => {
                    let result = self.apply_rule(rule.clone(), &statement, &branch_id);

                    // Open child branches of branch where original statement is
                    let open_branches_ids = self
                        .tree
                        .traverse_downwards_branch_ids(&branch_id)
                        .filter(|x| {
                            !self.tree.branch_from_id(&x).is_closed()
                                && self.tree.branch_is_last_child(&x)
                        })
                        .collect::<Vec<_>>();

                    for child_branch_id in open_branches_ids {
                        for x in &result.statements {
                            let (derived_statement_id, derived_statement_branch_id) = {
                                match result.whatdo {
                                    DerivedRuleWhatdo::AddToExistingBranches => {
                                        // Add derived statement to all open child branches of branch_id
                                        // at the end of the tree (i.e. child branches that have no children)
                                        let new_statement_id = self
                                            .tree
                                            .branch_from_id_mut(&child_branch_id)
                                            .append_statement(TreeNode {
                                                statement: x.clone(),
                                                derived_from: Some((
                                                    TreeNodeLocation(
                                                        statement_id.clone(),
                                                        branch_id.clone(),
                                                    ),
                                                    rule.clone(),
                                                )),
                                            });

                                        (new_statement_id, child_branch_id.clone())
                                    }
                                    DerivedRuleWhatdo::AsNewBranches => {
                                        // Each derived statement will create a new child branch on every open
                                        // branch of branch_id that is at the end of the tree
                                        let new_branch = Branch::new(vec![TreeNode {
                                            statement: x.clone(),
                                            derived_from: Some((
                                                TreeNodeLocation(
                                                    statement_id.clone(),
                                                    branch_id.clone(),
                                                ),
                                                rule.clone(),
                                            )),
                                        }]);

                                        let root_statement_id =
                                            new_branch.statement_ids().next().unwrap();

                                        let new_branch_id = self
                                            .tree
                                            .append_branch_at(new_branch, &child_branch_id);

                                        (root_statement_id, new_branch_id.clone())
                                    }
                                }
                            };

                            // Add derived statement to queue for further processing
                            let new_node = QueueNode {
                                statement_id: derived_statement_id,
                                statement: x.clone(),
                                rule: self.matches_some_rule(&x),
                                branch_id: derived_statement_branch_id.clone(),
                            };
                            queue.push(new_node);

                            /*
                            // If the statement contradicts another in derived_statement_branch_id or
                            // any of its parents, keep the statement in the branch
                            // but close it and move on to the next one
                            if self.statement_is_contradiction(&x, &derived_statement_branch_id) {
                                self.tree
                                    .branch_from_id_mut(&derived_statement_branch_id)
                                    .close();
                                break;
                            }*/
                        }
                    }
                }
                None => {
                    // No rule to apply (statement is already atomic formula),
                    // statement is already on tree, so we do nothing here
                    // except checking for contradictions
                    if self.statement_is_contradiction(&statement, &branch_id) {
                        self.tree.branch_from_id_mut(&branch_id).close();
                        continue;
                    }
                }
            }

            // Original rules don't need to be marked done
            // The algorithm doesn't need it, and you can know which are 'done'
            // by checking the IDs that statements derive from
        }

        self.tree
    }

    fn statement_is_contradiction(&self, statement: &Statement, branch_id: &Id) -> bool {
        // Usually, you'd think to only iterate towards the root of the tree
        // to find a contradiction, however, it's easier if we accept the entire
        // branch the statement is on. After all, if there is a contradiction
        // anywhere in the branch, whether it be above or below (which can't really
        // happen in the current state of things of 'compute'), the branch will
        // close.

        for (_, ancestor_branch) in self.tree.traverse_upwards_branches(&branch_id) {
            for (_, tree_node) in ancestor_branch.statements() {
                match (&tree_node.statement, statement) {
                    (Statement::LogicalNegation(ref a), ref b @ _) => {
                        if **a == **b {
                            return true;
                        }
                    }
                    (ref a @ _, Statement::LogicalNegation(ref b)) => {
                        if **a == **b {
                            return true;
                        }
                    }
                    _ => {}
                }
            }
        }

        false
    }

    fn matches_some_rule(&self, statement: &Statement) -> Option<Rule> {
        if self.can_apply_qe_rule(&statement) {
            Some(Rule::QuantifierExchange)
        } else if self.can_apply_eq_rule(&statement) {
            Some(Rule::ExistentialQuantifier)
        } else if self.can_apply_uq_rule(&statement) {
            Some(Rule::UniversalQuantifier)
        } else if self.can_apply_double_negation_rule(&statement) {
            Some(Rule::DoubleNegation)
        } else if self.can_apply_conjunction_rule(&statement) {
            Some(Rule::Conjunction)
        } else if self.can_apply_negation_of_conditional_rule(&statement) {
            Some(Rule::NegationOfConditional)
        } else if self.can_apply_negation_of_disjunction_rule(&statement) {
            Some(Rule::NegationOfDisjunction)
        } else if self.can_apply_conditional_rule(&statement) {
            Some(Rule::Conditional)
        } else if self.can_apply_negation_of_conjunction_rule(&statement) {
            Some(Rule::NegationOfConjunction)
        } else if self.can_apply_disjunction_rule(&statement) {
            Some(Rule::Disjunction)
        } else {
            None
        }
    }

    fn can_apply_qe_rule(&self, statement: &Statement) -> bool {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::Existential(_, _) | Statement::Universal(_, _) => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn can_apply_eq_rule(&self, statement: &Statement) -> bool {
        match statement {
            Statement::Existential(_, _) => true,
            _ => false,
        }
    }

    fn can_apply_uq_rule(&self, statement: &Statement) -> bool {
        match statement {
            Statement::Universal(_, _) => true,
            _ => false,
        }
    }

    fn can_apply_double_negation_rule(&self, statement: &Statement) -> bool {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalNegation(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn can_apply_conjunction_rule(&self, statement: &Statement) -> bool {
        match statement {
            Statement::LogicalConjunction(_, _) => true,
            _ => false,
        }
    }

    fn can_apply_negation_of_conditional_rule(&self, statement: &Statement) -> bool {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalConditional(_, _) => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn can_apply_negation_of_disjunction_rule(&self, statement: &Statement) -> bool {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalDisjunction(_, _) => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn can_apply_conditional_rule(&self, statement: &Statement) -> bool {
        match statement {
            Statement::LogicalConditional(_, _) => true,
            _ => false,
        }
    }

    fn can_apply_negation_of_conjunction_rule(&self, statement: &Statement) -> bool {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalConjunction(_, _) => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn can_apply_disjunction_rule(&self, statement: &Statement) -> bool {
        match statement {
            Statement::LogicalDisjunction(_, _) => true,
            _ => false,
        }
    }

    fn apply_rule(&self, rule: Rule, statement: &Statement, branch_id: &Id) -> RuleDeriveResult {
        match rule {
            Rule::QuantifierExchange => self.apply_qe_rule(&statement),
            Rule::ExistentialQuantifier => self.apply_eq_rule(&statement, &branch_id),
            Rule::UniversalQuantifier => self.apply_uq_rule(&statement, &branch_id),
            Rule::DoubleNegation => self.apply_double_negation_rule(&statement),
            Rule::Conjunction => self.apply_conjunction_rule(&statement),
            Rule::NegationOfConditional => self.apply_negation_of_conditional_rule(&statement),
            Rule::NegationOfDisjunction => self.apply_negation_of_disjunction_rule(&statement),
            Rule::Conditional => self.apply_conditional_rule(&statement),
            Rule::NegationOfConjunction => self.apply_negation_of_conjunction_rule(&statement),
            Rule::Disjunction => self.apply_disjunction_rule(&statement),
        }
    }

    fn apply_qe_rule(&self, statement: &Statement) -> RuleDeriveResult {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::Existential(ref var, ref pred) => RuleDeriveResult {
                    statements: vec![Statement::Universal(
                        var.clone(),
                        Predicate::Negative(Box::new(pred.clone())),
                    )],
                    whatdo: DerivedRuleWhatdo::AddToExistingBranches,
                },
                Statement::Universal(ref var, ref pred) => RuleDeriveResult {
                    statements: vec![Statement::Existential(
                        var.clone(),
                        Predicate::Negative(Box::new(pred.clone())),
                    )],
                    whatdo: DerivedRuleWhatdo::AddToExistingBranches,
                },
                _ => panic!("attempt to apply wrong rule to statement"),
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_eq_rule(&self, statement: &Statement, branch_id: &Id) -> RuleDeriveResult {
        match statement {
            Statement::Existential(ref var, ref pred) => RuleDeriveResult {
                statements: vec![self.instantiate_quantified_statement(
                    &pred,
                    &var,
                    &self.first_unused_in_singular_term_stack(
                        &self.build_singular_term_stack_for_branch(&branch_id),
                    ),
                )],
                whatdo: DerivedRuleWhatdo::AddToExistingBranches,
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_uq_rule(&self, statement: &Statement, branch_id: &Id) -> RuleDeriveResult {
        match statement {
            Statement::Universal(ref var, ref pred) => {
                // Build a list of all singular terms which we haven't instantiated
                // this universal statement to yet
                //
                // If there are no singular terms in this branch at all,
                // pick a random one
                let stack = self.build_singular_term_stack_for_branch(&branch_id);

                let new_singular_terms = if stack.is_empty() {
                    vec![self.first_unused_in_singular_term_stack(&stack)]
                } else {
                    stack
                        .iter()
                        .cloned()
                        .filter(|x| {
                            let instantiated_statement =
                                self.instantiate_quantified_statement(&pred, &var, &x);

                            for (_, ancestor_branch) in
                                self.tree.traverse_upwards_branches(&branch_id)
                            {
                                for (_, tree_node) in ancestor_branch.statements() {
                                    if instantiated_statement == tree_node.statement {
                                        return false;
                                    }
                                }
                            }

                            true
                        })
                        .collect::<Vec<_>>()
                };

                RuleDeriveResult {
                    statements: new_singular_terms
                        .iter()
                        .map(|x| self.instantiate_quantified_statement(&pred, &var, &x))
                        .collect(),
                    whatdo: DerivedRuleWhatdo::AddToExistingBranches,
                }
            }
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_double_negation_rule(&self, statement: &Statement) -> RuleDeriveResult {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalNegation(ref inner_rst) => RuleDeriveResult {
                    statements: vec![*inner_rst.clone()],
                    whatdo: DerivedRuleWhatdo::AddToExistingBranches,
                },
                _ => panic!("attempt to apply wrong rule to statement"),
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_conjunction_rule(&self, statement: &Statement) -> RuleDeriveResult {
        match statement {
            Statement::LogicalConjunction(ref lst, ref rst) => RuleDeriveResult {
                statements: vec![*lst.clone(), *rst.clone()],
                whatdo: DerivedRuleWhatdo::AddToExistingBranches,
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_negation_of_conditional_rule(&self, statement: &Statement) -> RuleDeriveResult {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalConditional(ref lst, ref rst) => RuleDeriveResult {
                    statements: vec![*lst.clone(), Statement::LogicalNegation(rst.clone())],
                    whatdo: DerivedRuleWhatdo::AddToExistingBranches,
                },
                _ => panic!("attempt to apply wrong rule to statement"),
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_negation_of_disjunction_rule(&self, statement: &Statement) -> RuleDeriveResult {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalDisjunction(ref lst, ref rst) => RuleDeriveResult {
                    statements: vec![
                        Statement::LogicalNegation(lst.clone()),
                        Statement::LogicalNegation(rst.clone()),
                    ],
                    whatdo: DerivedRuleWhatdo::AddToExistingBranches,
                },
                _ => panic!("attempt to apply wrong rule to statement"),
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_conditional_rule(&self, statement: &Statement) -> RuleDeriveResult {
        match statement {
            Statement::LogicalConditional(ref lst, ref rst) => RuleDeriveResult {
                statements: vec![Statement::LogicalNegation(lst.clone()), *rst.clone()],
                whatdo: DerivedRuleWhatdo::AsNewBranches,
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_negation_of_conjunction_rule(&self, statement: &Statement) -> RuleDeriveResult {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalConjunction(ref inner_lst, ref inner_rst) => RuleDeriveResult {
                    statements: vec![
                        Statement::LogicalNegation(inner_lst.clone()),
                        Statement::LogicalNegation(inner_rst.clone()),
                    ],
                    whatdo: DerivedRuleWhatdo::AsNewBranches,
                },
                _ => panic!("attempt to apply wrong rule to statement"),
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_disjunction_rule(&self, statement: &Statement) -> RuleDeriveResult {
        match statement {
            Statement::LogicalDisjunction(ref lst, ref rst) => RuleDeriveResult {
                statements: vec![*lst.clone(), *rst.clone()],
                whatdo: DerivedRuleWhatdo::AsNewBranches,
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn build_singular_term_stack_for_branch(&self, branch_id: &Id) -> Vec<SingularTerm> {
        let mut stack = Vec::new();

        for ancestor_branch_id in self.tree.traverse_upwards_branch_ids(&branch_id) {
            let ancestor_branch = self.tree.branch_from_id(&ancestor_branch_id);

            for statement_id in ancestor_branch.statement_ids() {
                self.find_singular_terms_in_statement(
                    &mut stack,
                    &ancestor_branch.statement_from_id(&statement_id).statement,
                );
            }
        }

        stack
    }

    fn find_singular_terms_in_statement(
        &self,
        mut stack: &mut Vec<SingularTerm>,
        statement: &Statement,
    ) {
        match statement {
            Statement::Singular(_, ref terms) => terms.iter().for_each(|t| {
                if !stack.contains(t) {
                    stack.push(t.clone());
                }
            }),
            Statement::LogicalConjunction(ref lst, ref rst)
            | Statement::LogicalDisjunction(ref lst, ref rst)
            | Statement::LogicalConditional(ref lst, ref rst) => {
                self.find_singular_terms_in_statement(&mut stack, &lst);
                self.find_singular_terms_in_statement(&mut stack, &rst);
            }
            Statement::LogicalNegation(ref rst) => {
                self.find_singular_terms_in_statement(&mut stack, &rst)
            }
            Statement::Existential(_, ref pred) | Statement::Universal(_, ref pred) => {
                self.find_singular_terms_in_predicate(&mut stack, &pred)
            }
            _ => {}
        }
    }

    fn find_singular_terms_in_predicate(
        &self,
        mut stack: &mut Vec<SingularTerm>,
        pred: &Predicate,
    ) {
        match pred {
            Predicate::Simple(_, ref terms) => terms.iter().for_each(|t| match t {
                Term::SingularTerm(ref singular_term) => {
                    if !stack.contains(singular_term) {
                        stack.push(singular_term.clone());
                    }
                }
                _ => {}
            }),
            Predicate::Conjunctive(ref lpred, ref rpred)
            | Predicate::Disjunctive(ref lpred, ref rpred)
            | Predicate::Conditional(ref lpred, ref rpred) => {
                self.find_singular_terms_in_predicate(&mut stack, &lpred);
                self.find_singular_terms_in_predicate(&mut stack, &rpred);
            }
            Predicate::Negative(rpred) => {
                self.find_singular_terms_in_predicate(&mut stack, &rpred);
            }
        }
    }

    fn instantiate_quantified_statement(
        &self,
        pred: &Predicate,
        var: &Variable,
        replace_with: &SingularTerm,
    ) -> Statement {
        match pred {
            Predicate::Simple(ref pred_letter, ref terms) => {
                let singular_terms = terms
                    .iter()
                    .map(|x| match x {
                        Term::Variable(ref v) => {
                            if v != var {
                                panic!("unknown variable in quantified statement")
                            }
                            replace_with.clone()
                        }
                        Term::SingularTerm(ref t) => t.clone(),
                    })
                    .collect::<Vec<SingularTerm>>();

                Statement::Singular(pred_letter.clone(), singular_terms)
            }
            Predicate::Conjunctive(ref lpred, ref rpred) => Statement::LogicalConjunction(
                Box::new(self.instantiate_quantified_statement(&lpred, &var, &replace_with)),
                Box::new(self.instantiate_quantified_statement(&rpred, &var, &replace_with)),
            ),
            Predicate::Negative(ref rpred) => Statement::LogicalNegation(Box::new(
                self.instantiate_quantified_statement(&rpred, &var, &replace_with),
            )),
            Predicate::Disjunctive(ref lpred, ref rpred) => Statement::LogicalDisjunction(
                Box::new(self.instantiate_quantified_statement(&lpred, &var, &replace_with)),
                Box::new(self.instantiate_quantified_statement(&rpred, &var, &replace_with)),
            ),
            Predicate::Conditional(ref lpred, ref rpred) => Statement::LogicalConditional(
                Box::new(self.instantiate_quantified_statement(&lpred, &var, &replace_with)),
                Box::new(self.instantiate_quantified_statement(&rpred, &var, &replace_with)),
            ),
        }
    }

    fn first_unused_in_singular_term_stack(&self, stack: &Vec<SingularTerm>) -> SingularTerm {
        // Ideally, this would be encapsulated by the parser, so we wouldn't
        // have to deal with the plain text grammar
        //
        // First iterates from 'a' to 'w' checking if any
        // of these singular terms are available
        // Returns the first one
        // If none are, it checks if 'a1' is available (where 1 is a
        // subscript), or 'b1', or 'c1', or 'd1', etc.
        // If it is, returns that
        // If it isn't, it checks if 'a2', 'b2', 'c2', etc. is available
        // etc. etc. etc.
        for c in 'a' as u8..='w' as u8 {
            if !stack.iter().any(|x| x.0 == c as char) {
                return SingularTerm(c as char, Subscript(None));
            }
        }

        for subscript in 1.. {
            // This subscript is not used by this x
            // And there isn't some x further in the stack with the same character
            // and the subscript we want
            match stack.iter().find_map(|x| {
                if x.1 != subscript && !stack.iter().any(|y| x.0 == y.0 && y.1 == subscript) {
                    Some(x)
                } else {
                    None
                }
            }) {
                Some(s) => return SingularTerm(s.0, Subscript(Some(subscript))),
                None => {}
            }
        }

        unreachable!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{Degree, PredicateLetter, SimpleStatementLetter, SingularTerm, Subscript};

    #[test]
    fn queue_node_priority_order_correct() {
        let branch = Branch::new(vec!["mock"]);
        let mock_id = branch.statement_ids().next().unwrap();
        let mock_statement = Statement::Simple(SimpleStatementLetter('A', Subscript(None)));

        let mut queue = BinaryHeap::new();

        // Unordered pushes
        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::Conjunction),
            branch_id: mock_id.clone(),
        });

        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::QuantifierExchange),
            branch_id: mock_id.clone(),
        });

        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::DoubleNegation),
            branch_id: mock_id.clone(),
        });

        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::ExistentialQuantifier),
            branch_id: mock_id.clone(),
        });

        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::NegationOfDisjunction),
            branch_id: mock_id.clone(),
        });

        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::Disjunction),
            branch_id: mock_id.clone(),
        });

        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::UniversalQuantifier),
            branch_id: mock_id.clone(),
        });

        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::NegationOfConjunction),
            branch_id: mock_id.clone(),
        });

        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::NegationOfConditional),
            branch_id: mock_id.clone(),
        });

        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::Conditional),
            branch_id: mock_id.clone(),
        });

        assert_eq!(queue.pop().unwrap().rule, Some(Rule::QuantifierExchange));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::ExistentialQuantifier));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::UniversalQuantifier));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::DoubleNegation));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::Conjunction));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::NegationOfConditional));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::NegationOfDisjunction));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::Conditional));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::NegationOfConjunction));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::Disjunction));
    }

    /*
    #[test]
    fn truth_tree_method_works_more_or_less() {
        let algo = TruthTreeMethod::new(&vec![
            Statement::Singular(
                PredicateLetter('A', Subscript(None), Degree(1)),
                vec![SingularTerm('a', Subscript(None))],
            ),
            Statement::LogicalNegation(Box::new(Statement::Existential(
                Variable('x', Subscript(None)),
                Predicate::Simple(
                    PredicateLetter('A', Subscript(None), Degree(1)),
                    vec![Term::Variable(Variable('x', Subscript(None)))],
                ),
            ))),
            Statement::LogicalDisjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'D',
                    Subscript(None),
                ))),
                Box::new(Statement::LogicalNegation(Box::new(Statement::Simple(
                    SimpleStatementLetter('C', Subscript(None)),
                )))),
            ),
            Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
        ]);

        let tree = algo.compute();

        for branch_id in tree.branch_id_iter(&tree.main_trunk_id()) {
            println!("Branch ID: {:#?}", branch_id.clone());

            let branch = tree.branch_from_id(&branch_id);

            println!("Branch is closed: {0}", branch.is_closed());

            for statement_id in branch.statement_id_iter() {
                let node = branch.statement_from_id(&statement_id);

                println!("- Statement ID: {:#?}", statement_id.clone());

                println!("- Statement data: ");

                println!("  - Statement: {:#?}", node.statement.clone());
                println!("  - Derived from: {:#?}", node.derived_from.clone());
            }
        }
    }*/
}
