use super::truth_tree::*;
use crate::parser::{Formula, SingularTerm, Statement, Subscript, Term, Variable};
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

pub struct TruthTreeMethod {
    tree: TruthTree,
}

impl TruthTreeMethod {
    pub fn new(statements: &Vec<Statement>) -> Self {
        TruthTreeMethod {
            tree: TruthTree::new(Branch::new(
                statements
                    .iter()
                    .map(|x| BranchNode {
                        statement: x.clone(),
                        derived_from: None,
                    })
                    .collect(),
            )),
        }
    }

    pub fn compute(mut self) -> TruthTree {
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
                                            .append_statement(BranchNode {
                                                statement: x.clone(),
                                                derived_from: Some((
                                                    BranchNodeLocation(
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
                                        let new_branch = Branch::new(vec![BranchNode {
                                            statement: x.clone(),
                                            derived_from: Some((
                                                BranchNodeLocation(
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
                Statement::Existential(ref var, ref formula) => RuleDeriveResult {
                    statements: vec![Statement::Universal(
                        var.clone(),
                        Box::new(Formula::Negation(Box::new(*formula.clone()))),
                    )],
                    whatdo: DerivedRuleWhatdo::AddToExistingBranches,
                },
                Statement::Universal(ref var, ref formula) => RuleDeriveResult {
                    statements: vec![Statement::Existential(
                        var.clone(),
                        Box::new(Formula::Negation(Box::new(*formula.clone()))),
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
            Statement::Existential(_, _) => RuleDeriveResult {
                statements: vec![self.instantiate_quantified_statement(
                    &statement,
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
            Statement::Universal(_, _) => {
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
                                self.instantiate_quantified_statement(&statement, &x);

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
                        .map(|x| self.instantiate_quantified_statement(&statement, &x))
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
            Statement::Existential(_, ref formula) | Statement::Universal(_, ref formula) => {
                self.find_singular_terms_in_formula(&mut stack, &formula)
            }
            _ => {}
        }
    }

    fn find_singular_terms_in_formula(&self, mut stack: &mut Vec<SingularTerm>, formula: &Formula) {
        match formula {
            Formula::Predicate(_, ref terms) => terms.iter().for_each(|t| match t {
                Term::SingularTerm(ref singular_term) => {
                    if !stack.contains(singular_term) {
                        stack.push(singular_term.clone());
                    }
                }
                _ => {}
            }),
            Formula::Statement(ref st) => self.find_singular_terms_in_statement(&mut stack, &st),
            Formula::Conjunction(ref lformula, ref rformula)
            | Formula::Disjunction(ref lformula, ref rformula)
            | Formula::Conditional(ref lformula, ref rformula) => {
                self.find_singular_terms_in_formula(&mut stack, &lformula);
                self.find_singular_terms_in_formula(&mut stack, &rformula);
            }
            Formula::Negation(ref rformula) => {
                self.find_singular_terms_in_formula(&mut stack, &rformula);
            }
        }
    }

    fn instantiation_transform_into_statement(&self, formula: &Formula) -> Statement {
        // See instantiate_quantified_statement for details
        match formula {
            Formula::Predicate(ref pred_letter, ref terms) => {
                // If the root is a predicate, and we know there can't be
                // free variables, then we can safely transform this into
                // a singular statement.
                Statement::Singular(
                    pred_letter.clone(),
                    terms
                        .iter()
                        .map(|x| match x {
                            Term::SingularTerm(t) => t.clone(),
                            Term::Variable(_) => {
                                panic!("variable at root of instantiated quantified statement")
                            }
                        })
                        .collect(),
                )
            }
            Formula::Conjunction(ref lformula, ref rformula) => Statement::LogicalConjunction(
                Box::new(self.instantiation_transform_into_statement(&*lformula.clone())),
                Box::new(self.instantiation_transform_into_statement(&*rformula.clone())),
            ),
            Formula::Negation(ref rformula) => Statement::LogicalNegation(Box::new(
                self.instantiation_transform_into_statement(&*rformula.clone()),
            )),
            Formula::Disjunction(ref lformula, ref rformula) => Statement::LogicalDisjunction(
                Box::new(self.instantiation_transform_into_statement(&*lformula.clone())),
                Box::new(self.instantiation_transform_into_statement(&*rformula.clone())),
            ),
            Formula::Conditional(ref lformula, ref rformula) => Statement::LogicalConditional(
                Box::new(self.instantiation_transform_into_statement(&*lformula.clone())),
                Box::new(self.instantiation_transform_into_statement(&*rformula.clone())),
            ),
            Formula::Statement(ref st) => *st.clone(), // An existential or universal statement
        }
    }

    fn instantiation_replace_in_formula(
        &self,
        formula: &Formula,
        var: &Variable,
        replace_with: &SingularTerm,
    ) -> Formula {
        // Replaces all occurrences of 'var' with 'replace_with' but leaves everything
        // else as-is
        match formula {
            Formula::Predicate(ref pred_letter, ref terms) => {
                let terms = terms
                    .iter()
                    .map(|x| match x {
                        Term::Variable(ref v) => {
                            if v == var {
                                Term::SingularTerm(replace_with.clone())
                            } else {
                                Term::Variable(v.clone())
                            }
                        }
                        t @ Term::SingularTerm(_) => t.clone(),
                    })
                    .collect();

                Formula::Predicate(pred_letter.clone(), terms)
            }
            Formula::Conjunction(ref lformula, ref rformula) => Formula::Conjunction(
                Box::new(self.instantiation_replace_in_formula(
                    &*lformula.clone(),
                    &var,
                    &replace_with,
                )),
                Box::new(self.instantiation_replace_in_formula(
                    &*rformula.clone(),
                    &var,
                    &replace_with,
                )),
            ),
            Formula::Negation(ref rformula) => Formula::Negation(Box::new(
                self.instantiation_replace_in_formula(&*rformula.clone(), &var, &replace_with),
            )),
            Formula::Disjunction(ref lformula, ref rformula) => Formula::Disjunction(
                Box::new(self.instantiation_replace_in_formula(
                    &*lformula.clone(),
                    &var,
                    &replace_with,
                )),
                Box::new(self.instantiation_replace_in_formula(
                    &*rformula.clone(),
                    &var,
                    &replace_with,
                )),
            ),
            Formula::Conditional(ref lformula, ref rformula) => Formula::Conditional(
                Box::new(self.instantiation_replace_in_formula(
                    &*lformula.clone(),
                    &var,
                    &replace_with,
                )),
                Box::new(self.instantiation_replace_in_formula(
                    &*rformula.clone(),
                    &var,
                    &replace_with,
                )),
            ),
            Formula::Statement(ref statement) => match **statement {
                Statement::Simple(_) | Statement::Singular(_, _) => {
                    Formula::Statement(Box::new(*statement.clone()))
                }
                Statement::LogicalConjunction(ref lst, ref rst) => Formula::Conjunction(
                    Box::new(self.instantiation_replace_in_formula(
                        &Formula::Statement(lst.clone()),
                        &var,
                        &replace_with,
                    )),
                    Box::new(self.instantiation_replace_in_formula(
                        &Formula::Statement(rst.clone()),
                        &var,
                        &replace_with,
                    )),
                ),
                Statement::LogicalNegation(ref rst) => {
                    Formula::Negation(Box::new(self.instantiation_replace_in_formula(
                        &Formula::Statement(rst.clone()),
                        &var,
                        &replace_with,
                    )))
                }
                Statement::LogicalDisjunction(ref lst, ref rst) => Formula::Disjunction(
                    Box::new(self.instantiation_replace_in_formula(
                        &Formula::Statement(lst.clone()),
                        &var,
                        &replace_with,
                    )),
                    Box::new(self.instantiation_replace_in_formula(
                        &Formula::Statement(rst.clone()),
                        &var,
                        &replace_with,
                    )),
                ),
                Statement::LogicalConditional(ref lst, ref rst) => Formula::Conditional(
                    Box::new(self.instantiation_replace_in_formula(
                        &Formula::Statement(lst.clone()),
                        &var,
                        &replace_with,
                    )),
                    Box::new(self.instantiation_replace_in_formula(
                        &Formula::Statement(rst.clone()),
                        &var,
                        &replace_with,
                    )),
                ),
                Statement::Existential(ref var_pls_dont_shadow, ref formula) => {
                    Formula::Statement(Box::new(Statement::Existential(
                        var_pls_dont_shadow.clone(),
                        Box::new(self.instantiation_replace_in_formula(
                            &formula,
                            &var,
                            &replace_with,
                        )),
                    )))
                }
                Statement::Universal(ref var_pls_dont_shadow, ref formula) => {
                    Formula::Statement(Box::new(Statement::Universal(
                        var_pls_dont_shadow.clone(),
                        Box::new(self.instantiation_replace_in_formula(
                            &formula,
                            &var,
                            &replace_with,
                        )),
                    )))
                }
            },
        }
    }

    fn instantiate_quantified_statement(
        &self,
        statement: &Statement,
        replace_with: &SingularTerm,
    ) -> Statement {
        // In order to instantiate a quantified statement, we take
        // the variable it binds to and replace it everywhere in the statement
        // with an instantiation of a singular term type.
        // A quantified statement's inner formula can be made, at the very root,
        // of either predicates or statements. Assuming a valid input, free
        // variables cannot appear, hence any predicate within a formula either
        // becomes a singular statement (it's at the root and its only terms that are
        // variables are our quantifier's variable) or a predicate that is enclosed
        // within another quantified statement in this formula.
        // This is to say that the instantiated formula of a quantified statement
        // also HAS to be a statement.
        match statement {
            Statement::Existential(ref var, ref formula)
            | Statement::Universal(ref var, ref formula) => self
                .instantiation_transform_into_statement(&self.instantiation_replace_in_formula(
                    &formula,
                    &var,
                    &replace_with,
                )),
            _ => panic!(
                "called instantiated_quantified_statement\
                 with non-quantified statement"
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
            for c in 'a' as u8..='w' as u8 {
                if !stack.iter().any(|x| match x {
                    SingularTerm(a, b) => *a == c as char && b.0 == Some(subscript),
                }) {
                    return SingularTerm(c as char, Subscript(Some(subscript)));
                }
            }
        }

        unreachable!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{
        Degree, PredicateLetter, SimpleStatementLetter, SingularTerm, Statement, Subscript,
    };

    #[test]
    fn queue_node_priority_order_correct() {
        let branch = Branch::new(vec![BranchNode {
            statement: Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
            derived_from: None,
        }]);
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

    // FIXME Algorithm needs proper testing. Not sure how to do that

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

    #[test]
    fn instantiate_quantified_statement() {
        // Quantified statement: ∀x((A¹x & B¹x) ⊃ ∀y((~C¹y) ⊃ ∃z(A²zy & B²zx)))
        // After instantiation:     (A¹a & B¹a) ⊃ ∀y((~C¹y) ⊃ ∃z(A²zy & B²za))
        let statement = Statement::Universal(
            Variable('x', Subscript(None)),
            Box::new(Formula::Conditional(
                Box::new(Formula::Conjunction(
                    Box::new(Formula::Predicate(
                        PredicateLetter('A', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                    Box::new(Formula::Predicate(
                        PredicateLetter('B', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                )),
                Box::new(Formula::Statement(Box::new(Statement::Universal(
                    Variable('y', Subscript(None)),
                    Box::new(Formula::Conditional(
                        Box::new(Formula::Negation(Box::new(Formula::Predicate(
                            PredicateLetter('C', Subscript(None), Degree(1)),
                            vec![Term::Variable(Variable('y', Subscript(None)))],
                        )))),
                        Box::new(Formula::Statement(Box::new(Statement::Existential(
                            Variable('z', Subscript(None)),
                            Box::new(Formula::Conjunction(
                                Box::new(Formula::Predicate(
                                    PredicateLetter('A', Subscript(None), Degree(2)),
                                    vec![
                                        Term::Variable(Variable('z', Subscript(None))),
                                        Term::Variable(Variable('y', Subscript(None))),
                                    ],
                                )),
                                Box::new(Formula::Predicate(
                                    PredicateLetter('B', Subscript(None), Degree(2)),
                                    vec![
                                        Term::Variable(Variable('z', Subscript(None))),
                                        Term::Variable(Variable('x', Subscript(None))),
                                    ],
                                )),
                            )),
                        )))),
                    )),
                )))),
            )),
        );

        let truth_tree = TruthTreeMethod::new(&vec![statement.clone()]);

        println!(
            "{:#?}",
            truth_tree
                .instantiate_quantified_statement(&statement, &SingularTerm('a', Subscript(None)))
        );
    }
}
