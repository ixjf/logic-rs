use super::truth_tree::*;
use crate::parser::{Formula, SingularTerm, Statement, Subscript, Term, Variable};
use snowflake::ProcessUniqueId;
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::iter::once;

#[derive(Clone, PartialEq, Eq, Debug)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
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

#[derive(PartialEq, Eq, Debug, Clone)]
struct QueueEntry {
    statement_id: Id,
    statement: Statement,
    rule: Option<(Rule, bool)>,
    branch_id: Id,
    failed_last: bool, // Indicates whether the rule was applied successfully the last time
                       // Useful for recursive rules where we need to avoid infinite loops if no rule in the queue
                       // can be applied any longer. (e.g. UQ - it can never be removed from the queue
                       // as it may be applicable anytime, but we also need to know if we can't apply
                       // any more rules in order to stop the algorithm)
}

impl Ord for QueueEntry {
    fn cmp(&self, other: &QueueEntry) -> Ordering {
        // Priority order, top should come first, bottom last
        // There is a reason for this order.
        // 1. QE can potentially add new EQs to the tree
        // and EQs add new singular terms. This one is mostly just
        // to keep in line with the book. When writing a truth tree by hand,
        // keeping track of UQ rules applied can get difficult.
        // 2. EQ adds new singular terms, which UQ depends on.
        // The sooner we add those singular terms to the stack,
        // the sooner we may be able to close the tree
        // 3. UQ should come last out of all non-branching rules
        // since we want to apply UQ to one singular term and move on,
        // and come back later (if we instantiate UQ to all singular terms
        // on the branch at once, we then can't close the tree until
        // we apply rules to all those new statements). But if this rule
        // doesn't have lower priority than the other non-branching rules,
        // then as soon as we add it to the queue, it will be popped,
        // since it'll be the first one out.
        // 4. Non-branching rules before branching rules because, again,
        // we want to keep the truth tree as compact as possible
        //
        // Above is defined the order as the book puts it. However:
        // There _is_ a difference between this order and the order
        // as defined in the book, and that is the order of QE, EQ,
        // and UQ rules in relation to all others. If non-branching rules are to be applied _before_
        // branching rules, then these rules I mentioned should come
        // before branching rules. However, and according to this:
        // http://www.cogsci.rpi.edu/~heuveb/teaching/Logic/CompLogic/Web/Handouts/FO-Completeness-Truth-Tree.pdf
        // if we move quantifier rules to the end, even after
        // branching rules, then (if there is no mistake in the algorithm)
        // we can guarantee that if the algorithm generates an infinite
        // tree, then the initial set of statements is satisfiable.
        // The problem of knowing whether we are facing an infinite truth tree
        // remains, but at least we can be certain that if we have an
        // unsatisfiable initial set of statements, then our algorithm
        // will generate a closed tree.
        let rule_priority_order: [Rule; 10] = [
            Rule::DoubleNegation,
            Rule::Conjunction,
            Rule::NegationOfConditional,
            Rule::NegationOfDisjunction,
            Rule::Conditional,
            Rule::NegationOfConjunction,
            Rule::Disjunction,
            Rule::QuantifierExchange,
            Rule::ExistentialQuantifier,
            Rule::UniversalQuantifier,
        ];

        // NOTE This is pointless since UQ is already the last rule of all
        // to be applied and the queue already follows FIFO when
        // comparing two entries with same rule
        // Whichever failed the last time shall come last as well
        // (see end of 'compute' method for details)
        // match (self.failed_last, other.failed_last) {
        //     (true, false) => {
        //         return Ordering::Less;
        //     }
        //     (false, true) => {
        //         return Ordering::Greater;
        //     }
        //     (true, true) => {
        //         return Ordering::Equal;
        //     }
        //     // ^ If both failed last, follow insertion order (FIFO)
        //     (false, false) => {}
        // }

        match (&self.rule, &other.rule) {
            // Atomic statements should come first in the queue
            (Some(_), None) => Ordering::Less,
            (None, Some(_)) => Ordering::Greater,
            // If rule B comes after rule A in rule_priority_order,
            // then A should come first
            (Some(rule_a), Some(rule_b)) => rule_priority_order
                .iter()
                .position(|x| *x == rule_b.0)
                .unwrap()
                .cmp(
                    &rule_priority_order
                        .iter()
                        .position(|x| *x == rule_a.0)
                        .unwrap(),
                ),
            (None, None) => Ordering::Equal,
        }
    }
}

impl PartialOrd for QueueEntry {
    fn partial_cmp(&self, other: &QueueEntry) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

enum ApplyRuleWhatdo {
    AddToExistingBranches,
    AsNewBranches,
}

struct RuleDeriveResult {
    statements: Vec<Statement>,
    whatdo: ApplyRuleWhatdo,
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
            queue.push(QueueEntry {
                statement_id: statement_id.clone(),
                statement: tree_node.statement.clone(),
                rule: self.matches_some_rule(&tree_node.statement),
                branch_id: self.tree.main_trunk_id(),
                failed_last: false,
            });
        }

        // All nodes on the queue are already on the tree
        // A node represents some statement that needs to have a rule applied to it
        'outer: while let Some(QueueEntry {
            statement_id,
            statement,
            rule,
            branch_id,
            mut failed_last,
        }) = queue.pop()
        {
            match rule {
                Some((rule, repeat)) => {
                    // Open child branches of branch where original statement is
                    // This reflects the open branches BEFORE we added the results
                    // of the application of rules.
                    let open_branches_ids = self
                        .tree
                        .traverse_downwards_branch_ids(&branch_id)
                        .filter(|x| {
                            !self.tree.branch_from_id(&x).is_closed()
                                && self.tree.branch_is_last_child(&x)
                        })
                        .collect::<Vec<_>>();

                    if open_branches_ids.len() == 0 {
                        continue;
                    }

                    // Generate a unique ID for all resulting statements
                    // (to identify them as resulting from the same application of a rule)
                    let mut derivation_id = DerivationId {
                        id: ProcessUniqueId::new(),
                        index: 0u64,
                    };

                    // Apply the rule to every open branch
                    for child_branch_id in open_branches_ids {
                        match self.apply_rule(rule.clone(), &statement, &child_branch_id) {
                            Some(result) => {
                                for x in &result.statements {
                                    let (derived_statement_id, derived_statement_branch_id) = {
                                        match result.whatdo {
                                            ApplyRuleWhatdo::AddToExistingBranches => {
                                                // Add derived statement to all open child branches of branch_id
                                                // at the end of the tree (i.e. child branches that have no children)
                                                let new_statement_id = self
                                                    .tree
                                                    .branch_from_id_mut(&child_branch_id)
                                                    .append_statement(BranchNode {
                                                        statement: x.clone(),
                                                        derived_from: Some((
                                                            BranchNodeLocation {
                                                                node_id: statement_id.clone(),
                                                                branch_id: branch_id.clone(),
                                                            },
                                                            rule.clone(),
                                                            derivation_id.clone(),
                                                        )),
                                                    });

                                                // Each derived statement added to the same branch has a unique 'index'
                                                derivation_id.index += 1;

                                                (new_statement_id, child_branch_id.clone())
                                            }

                                            ApplyRuleWhatdo::AsNewBranches => {
                                                // Each derived statement will create a new child branch on every open
                                                // branch under branch_id that is at the end of the tree
                                                let new_branch = Branch::new(vec![BranchNode {
                                                    statement: x.clone(),
                                                    derived_from: Some((
                                                        BranchNodeLocation {
                                                            node_id: statement_id.clone(),
                                                            branch_id: branch_id.clone(),
                                                        },
                                                        rule.clone(),
                                                        derivation_id.clone(),
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
                                    let new_node = QueueEntry {
                                        statement_id: derived_statement_id,
                                        statement: x.clone(),
                                        rule: self.matches_some_rule(&x),
                                        branch_id: derived_statement_branch_id.clone(),
                                        failed_last: false,
                                    };
                                    queue.push(new_node);
                                }

                                failed_last = false;

                                // A new node was added to the tree
                                // This invalidates the failed_last state, since we don't know if rules on the queue
                                // might apply to any potential new singular terms
                                // If we didn't invalidate the failed_last state, we could have a situation where:
                                // tried all uq rules, failed
                                // failed_last = true on all uq rules
                                // some other rule then succeeded
                                // rule pushed off the queue
                                // compute() iterates over remaining uq rules, one of them doesn't apply
                                // and since failed_last = true, it fails, even though there are other rules
                                // to try
                                queue = BinaryHeap::from(
                                    queue
                                        .into_vec()
                                        .iter()
                                        .map(|x| QueueEntry {
                                            failed_last: false,
                                            ..x.clone()
                                        })
                                        .collect::<Vec<_>>(),
                                );
                                // FIXME Is there a more efficient way (if we're thinking about that)
                                // to do this other than using RefCell (mutating the contents of
                                // the queue)?
                            }
                            None => {
                                // Rule didn't need to be applied
                                if repeat {
                                    // Some rules can be reapplied over and over (i.e. UQ)
                                    // If this is the case, we readd this node to the queue

                                    // However, if we don't do anything else, we will run into an infinite
                                    // loop when the tree doesn't close.

                                    // Check if we haven't tried every other rule and some didn't fail
                                    // If they all failed, we might as well stop now
                                    if !queue.iter().any(|x| !x.failed_last) {
                                        break 'outer;
                                    }
                                }

                                // Since we've reached here, there are other rules that may yet be applied
                                // What happens now is the algorithm iterates over the queue, and if no
                                // other rule can be applied, the code above will break the loop and finish
                                // For now, let us tell the algorithm at least this rule failed

                                // Important: we won't run into an infinite loop where there are branching rules
                                // to be applied but that can't because the queue pops UQ first all the time,
                                // because the impl for the Ord trait on QueueEntry makes sure that if
                                // the rule has failed_last set, it'll always come after ALL other rules.
                                // Since UQ is the only repeat rule, we're safe
                                failed_last = true;
                            }
                        }
                    }

                    if repeat {
                        // See right above, if we reached here, we're safe
                        queue.push(QueueEntry {
                            statement_id,
                            statement,
                            rule: Some((rule.clone(), repeat)),
                            branch_id,
                            failed_last: failed_last,
                        });
                    }
                }
                None => {
                    // No rule to apply (statement is already atomic formula),
                    // statement is already on tree, so we do nothing here
                    // except checking for contradiction
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

    fn matches_some_rule(&self, statement: &Statement) -> Option<(Rule, bool)> {
        if self.can_apply_qe_rule(&statement) {
            Some((Rule::QuantifierExchange, false))
        } else if self.can_apply_eq_rule(&statement) {
            Some((Rule::ExistentialQuantifier, false))
        } else if self.can_apply_uq_rule(&statement) {
            Some((Rule::UniversalQuantifier, true))
        } else if self.can_apply_double_negation_rule(&statement) {
            Some((Rule::DoubleNegation, false))
        } else if self.can_apply_conjunction_rule(&statement) {
            Some((Rule::Conjunction, false))
        } else if self.can_apply_negation_of_conditional_rule(&statement) {
            Some((Rule::NegationOfConditional, false))
        } else if self.can_apply_negation_of_disjunction_rule(&statement) {
            Some((Rule::NegationOfDisjunction, false))
        } else if self.can_apply_conditional_rule(&statement) {
            Some((Rule::Conditional, false))
        } else if self.can_apply_negation_of_conjunction_rule(&statement) {
            Some((Rule::NegationOfConjunction, false))
        } else if self.can_apply_disjunction_rule(&statement) {
            Some((Rule::Disjunction, false))
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

    fn apply_rule(
        &self,
        rule: Rule,
        statement: &Statement,
        branch_id: &Id,
    ) -> Option<RuleDeriveResult> {
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

    fn apply_qe_rule(&self, statement: &Statement) -> Option<RuleDeriveResult> {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::Existential(ref var, ref formula) => Some(RuleDeriveResult {
                    statements: vec![Statement::Universal(
                        var.clone(),
                        Box::new(Formula::Negation(Box::new(*formula.clone()))),
                    )],
                    whatdo: ApplyRuleWhatdo::AddToExistingBranches,
                }),
                Statement::Universal(ref var, ref formula) => Some(RuleDeriveResult {
                    statements: vec![Statement::Existential(
                        var.clone(),
                        Box::new(Formula::Negation(Box::new(*formula.clone()))),
                    )],
                    whatdo: ApplyRuleWhatdo::AddToExistingBranches,
                }),
                _ => panic!("attempt to apply wrong rule to statement"),
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_eq_rule(&self, statement: &Statement, branch_id: &Id) -> Option<RuleDeriveResult> {
        match statement {
            Statement::Existential(_, _) => Some(RuleDeriveResult {
                statements: vec![self.instantiate_quantified_statement(
                    &statement,
                    &self.first_unused_in_singular_term_stack(
                        &self.build_singular_term_stack_for_branch(&branch_id),
                    ),
                )],
                whatdo: ApplyRuleWhatdo::AddToExistingBranches,
            }),
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_uq_rule(&self, statement: &Statement, branch_id: &Id) -> Option<RuleDeriveResult> {
        match statement {
            Statement::Universal(_, _) => {
                // Find some singular term which we haven't instantiated this universal
                // statement to yet
                //
                // If there are no singular terms in this branch at all,
                // pick a random one
                //
                // This rule only instantiates to one singular term at a time
                // in order to avoid unnecessarily long truth trees
                let stack = self.build_singular_term_stack_for_branch(&branch_id);

                let new_singular_term = if stack.is_empty() {
                    Some(self.first_unused_in_singular_term_stack(&stack))
                } else {
                    stack.iter().cloned().find(|x| {
                        let instantiated_statement =
                            self.instantiate_quantified_statement(&statement, &x);

                        for (_, ancestor_branch) in self.tree.traverse_upwards_branches(&branch_id)
                        {
                            for (_, tree_node) in ancestor_branch.statements() {
                                if instantiated_statement == tree_node.statement {
                                    return false;
                                }
                            }
                        }

                        true
                    })
                };

                match new_singular_term {
                    Some(s) => Some(RuleDeriveResult {
                        statements: vec![self.instantiate_quantified_statement(&statement, &s)],
                        whatdo: ApplyRuleWhatdo::AddToExistingBranches,
                    }),
                    None => None,
                }
            }
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_double_negation_rule(&self, statement: &Statement) -> Option<RuleDeriveResult> {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalNegation(ref inner_rst) => Some(RuleDeriveResult {
                    statements: vec![*inner_rst.clone()],
                    whatdo: ApplyRuleWhatdo::AddToExistingBranches,
                }),
                _ => panic!("attempt to apply wrong rule to statement"),
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_conjunction_rule(&self, statement: &Statement) -> Option<RuleDeriveResult> {
        match statement {
            Statement::LogicalConjunction(ref lst, ref rst) => Some(RuleDeriveResult {
                statements: vec![*lst.clone(), *rst.clone()],
                whatdo: ApplyRuleWhatdo::AddToExistingBranches,
            }),
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_negation_of_conditional_rule(
        &self,
        statement: &Statement,
    ) -> Option<RuleDeriveResult> {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalConditional(ref lst, ref rst) => Some(RuleDeriveResult {
                    statements: vec![*lst.clone(), Statement::LogicalNegation(rst.clone())],
                    whatdo: ApplyRuleWhatdo::AddToExistingBranches,
                }),
                _ => panic!("attempt to apply wrong rule to statement"),
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_negation_of_disjunction_rule(
        &self,
        statement: &Statement,
    ) -> Option<RuleDeriveResult> {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalDisjunction(ref lst, ref rst) => Some(RuleDeriveResult {
                    statements: vec![
                        Statement::LogicalNegation(lst.clone()),
                        Statement::LogicalNegation(rst.clone()),
                    ],
                    whatdo: ApplyRuleWhatdo::AddToExistingBranches,
                }),
                _ => panic!("attempt to apply wrong rule to statement"),
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_conditional_rule(&self, statement: &Statement) -> Option<RuleDeriveResult> {
        match statement {
            Statement::LogicalConditional(ref lst, ref rst) => Some(RuleDeriveResult {
                statements: vec![Statement::LogicalNegation(lst.clone()), *rst.clone()],
                whatdo: ApplyRuleWhatdo::AsNewBranches,
            }),
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_negation_of_conjunction_rule(
        &self,
        statement: &Statement,
    ) -> Option<RuleDeriveResult> {
        match statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalConjunction(ref inner_lst, ref inner_rst) => {
                    Some(RuleDeriveResult {
                        statements: vec![
                            Statement::LogicalNegation(inner_lst.clone()),
                            Statement::LogicalNegation(inner_rst.clone()),
                        ],
                        whatdo: ApplyRuleWhatdo::AsNewBranches,
                    })
                }
                _ => panic!("attempt to apply wrong rule to statement"),
            },
            _ => panic!("attempt to apply wrong rule to statement"),
        }
    }

    fn apply_disjunction_rule(&self, statement: &Statement) -> Option<RuleDeriveResult> {
        match statement {
            Statement::LogicalDisjunction(ref lst, ref rst) => Some(RuleDeriveResult {
                statements: vec![*lst.clone(), *rst.clone()],
                whatdo: ApplyRuleWhatdo::AsNewBranches,
            }),
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
                // We can be certain that if Formula::Predicate appears, then
                // it's at the root, since any Formula::Predicate that doesn't
                // appear at the root can only appear in a quantified
                // statement, but these two cases are handled in Formula::Statement
                // below
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
        for subscript in once(Subscript(None)).chain((1..).map(|x| Subscript(Some(x)))) {
            for c in 'a' as u8..='w' as u8 {
                let singular_term = SingularTerm(c as char, subscript.clone());
                if !stack.iter().any(|x| x == &singular_term) {
                    return singular_term;
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
    fn queue_entry_priority_order_correct() {
        let branch = Branch::new(vec![BranchNode {
            statement: Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
            derived_from: None,
        }]);
        let mock_id = branch.statement_ids().next().unwrap();
        let mock_statement = Statement::Simple(SimpleStatementLetter('A', Subscript(None)));

        let mut queue = BinaryHeap::new();

        // Unordered pushes
        queue.push(QueueEntry {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some((Rule::Conjunction, false)),
            branch_id: mock_id.clone(),
            failed_last: false,
        });

        queue.push(QueueEntry {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some((Rule::QuantifierExchange, false)),
            branch_id: mock_id.clone(),
            failed_last: false,
        });

        queue.push(QueueEntry {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some((Rule::DoubleNegation, false)),
            branch_id: mock_id.clone(),
            failed_last: false,
        });

        queue.push(QueueEntry {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some((Rule::ExistentialQuantifier, false)),
            branch_id: mock_id.clone(),
            failed_last: false,
        });

        queue.push(QueueEntry {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some((Rule::NegationOfDisjunction, false)),
            branch_id: mock_id.clone(),
            failed_last: false,
        });

        queue.push(QueueEntry {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some((Rule::Disjunction, false)),
            branch_id: mock_id.clone(),
            failed_last: false,
        });

        queue.push(QueueEntry {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some((Rule::UniversalQuantifier, true)),
            branch_id: mock_id.clone(),
            failed_last: false,
        });

        queue.push(QueueEntry {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some((Rule::NegationOfConjunction, false)),
            branch_id: mock_id.clone(),
            failed_last: false,
        });

        queue.push(QueueEntry {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some((Rule::NegationOfConditional, false)),
            branch_id: mock_id.clone(),
            failed_last: false,
        });

        queue.push(QueueEntry {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some((Rule::Conditional, false)),
            branch_id: mock_id.clone(),
            failed_last: false,
        });

        assert_eq!(
            queue.pop().unwrap().rule,
            Some((Rule::DoubleNegation, false))
        );
        assert_eq!(queue.pop().unwrap().rule, Some((Rule::Conjunction, false)));
        assert_eq!(
            queue.pop().unwrap().rule,
            Some((Rule::NegationOfConditional, false))
        );
        assert_eq!(
            queue.pop().unwrap().rule,
            Some((Rule::NegationOfDisjunction, false))
        );

        assert_eq!(queue.pop().unwrap().rule, Some((Rule::Conditional, false)));
        assert_eq!(
            queue.pop().unwrap().rule,
            Some((Rule::NegationOfConjunction, false))
        );
        assert_eq!(queue.pop().unwrap().rule, Some((Rule::Disjunction, false)));
        assert_eq!(
            queue.pop().unwrap().rule,
            Some((Rule::QuantifierExchange, false))
        );
        assert_eq!(
            queue.pop().unwrap().rule,
            Some((Rule::ExistentialQuantifier, false))
        );
        assert_eq!(
            queue.pop().unwrap().rule,
            Some((Rule::UniversalQuantifier, true))
        );
    }

    #[test]
    fn statement_is_contradiction() {
        let mut truth_tree_method = TruthTreeMethod::new(&vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]);

        let child_branch_id = truth_tree_method.tree.append_branch_at(
            Branch::new(vec![BranchNode {
                statement: Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
                derived_from: None,
            }]),
            &truth_tree_method.tree.main_trunk_id(),
        );

        assert!(truth_tree_method.statement_is_contradiction(
            &Statement::LogicalNegation(Box::new(Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None)
            ),))),
            &child_branch_id
        ));

        assert!(!truth_tree_method.statement_is_contradiction(
            &Statement::Simple(SimpleStatementLetter('A', Subscript(None)),),
            &child_branch_id
        ));
    }

    #[test]
    fn matches_some_rule() {
        let truth_tree_method = TruthTreeMethod::new(&vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]);

        assert_eq!(
            truth_tree_method.matches_some_rule(&Statement::LogicalNegation(Box::new(
                Statement::Existential(
                    Variable('x', Subscript(None)),
                    Box::new(Formula::Predicate(
                        PredicateLetter('B', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))]
                    ))
                )
            ))),
            Some((Rule::QuantifierExchange, false))
        );

        assert_eq!(
            truth_tree_method.matches_some_rule(&Statement::LogicalNegation(Box::new(
                Statement::Universal(
                    Variable('x', Subscript(None)),
                    Box::new(Formula::Predicate(
                        PredicateLetter('B', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))]
                    ))
                )
            ))),
            Some((Rule::QuantifierExchange, false))
        );

        assert_eq!(
            truth_tree_method.matches_some_rule(&Statement::Existential(
                Variable('x', Subscript(None)),
                Box::new(Formula::Predicate(
                    PredicateLetter('B', Subscript(None), Degree(1)),
                    vec![Term::Variable(Variable('x', Subscript(None)))]
                ))
            )),
            Some((Rule::ExistentialQuantifier, false))
        );

        assert_eq!(
            truth_tree_method.matches_some_rule(&Statement::Universal(
                Variable('x', Subscript(None)),
                Box::new(Formula::Predicate(
                    PredicateLetter('B', Subscript(None), Degree(1)),
                    vec![Term::Variable(Variable('x', Subscript(None)))]
                ))
            )),
            Some((Rule::UniversalQuantifier, true))
        );

        assert_eq!(
            truth_tree_method.matches_some_rule(&Statement::LogicalNegation(Box::new(
                Statement::LogicalNegation(Box::new(Statement::Simple(SimpleStatementLetter(
                    'C',
                    Subscript(None)
                ))))
            ))),
            Some((Rule::DoubleNegation, false))
        );

        assert_eq!(
            truth_tree_method.matches_some_rule(&Statement::LogicalConjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'C',
                    Subscript(None)
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'D',
                    Subscript(None)
                )))
            )),
            Some((Rule::Conjunction, false))
        );

        assert_eq!(
            truth_tree_method.matches_some_rule(&Statement::LogicalNegation(Box::new(
                Statement::LogicalConditional(
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'C',
                        Subscript(None)
                    ))),
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'D',
                        Subscript(None)
                    )))
                )
            ))),
            Some((Rule::NegationOfConditional, false))
        );

        assert_eq!(
            truth_tree_method.matches_some_rule(&Statement::LogicalNegation(Box::new(
                Statement::LogicalDisjunction(
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'C',
                        Subscript(None)
                    ))),
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'D',
                        Subscript(None)
                    )))
                )
            ))),
            Some((Rule::NegationOfDisjunction, false))
        );

        assert_eq!(
            truth_tree_method.matches_some_rule(&Statement::LogicalConditional(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'C',
                    Subscript(None)
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'D',
                    Subscript(None)
                )))
            )),
            Some((Rule::Conditional, false))
        );

        assert_eq!(
            truth_tree_method.matches_some_rule(&Statement::LogicalNegation(Box::new(
                Statement::LogicalConjunction(
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'C',
                        Subscript(None)
                    ))),
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'D',
                        Subscript(None)
                    )))
                )
            ))),
            Some((Rule::NegationOfConjunction, false))
        );

        assert_eq!(
            truth_tree_method.matches_some_rule(&Statement::LogicalDisjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'C',
                    Subscript(None)
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'D',
                    Subscript(None)
                )))
            )),
            Some((Rule::Disjunction, false))
        );

        assert_eq!(
            truth_tree_method.matches_some_rule(&Statement::Simple(SimpleStatementLetter(
                'C',
                Subscript(None)
            ))),
            None
        );
    }

    #[test]
    fn apply_qe_rule() {
        let truth_tree_method = TruthTreeMethod::new(&vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]);

        let rule_derive_result = truth_tree_method
            .apply_rule(
                Rule::QuantifierExchange,
                &Statement::LogicalNegation(Box::new(Statement::Existential(
                    Variable('x', Subscript(None)),
                    Box::new(Formula::Predicate(
                        PredicateLetter('B', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                ))),
                &truth_tree_method.tree.main_trunk_id(),
            )
            .unwrap();

        assert_eq!(rule_derive_result.statements.len(), 1);
        assert_eq!(
            rule_derive_result.statements.first().unwrap(),
            &Statement::Universal(
                Variable('x', Subscript(None)),
                Box::new(Formula::Negation(Box::new(Formula::Predicate(
                    PredicateLetter('B', Subscript(None), Degree(1)),
                    vec![Term::Variable(Variable('x', Subscript(None)))]
                ))))
            )
        );
        match rule_derive_result.whatdo {
            ApplyRuleWhatdo::AddToExistingBranches => {}
            ApplyRuleWhatdo::AsNewBranches => assert!(false),
        }

        let rule_derive_result = truth_tree_method
            .apply_rule(
                Rule::QuantifierExchange,
                &Statement::LogicalNegation(Box::new(Statement::Universal(
                    Variable('x', Subscript(None)),
                    Box::new(Formula::Predicate(
                        PredicateLetter('B', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                ))),
                &truth_tree_method.tree.main_trunk_id(),
            )
            .unwrap();

        assert_eq!(rule_derive_result.statements.len(), 1);
        assert_eq!(
            rule_derive_result.statements.first().unwrap(),
            &Statement::Existential(
                Variable('x', Subscript(None)),
                Box::new(Formula::Negation(Box::new(Formula::Predicate(
                    PredicateLetter('B', Subscript(None), Degree(1)),
                    vec![Term::Variable(Variable('x', Subscript(None)))]
                ))))
            )
        );
        match rule_derive_result.whatdo {
            ApplyRuleWhatdo::AddToExistingBranches => {}
            ApplyRuleWhatdo::AsNewBranches => assert!(false),
        }
    }

    #[test]
    fn apply_eq_rule() {
        let truth_tree_method = TruthTreeMethod::new(&vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]);

        let rule_derive_result = truth_tree_method
            .apply_rule(
                Rule::ExistentialQuantifier,
                &Statement::Existential(
                    Variable('x', Subscript(None)),
                    Box::new(Formula::Predicate(
                        PredicateLetter('B', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                ),
                &truth_tree_method.tree.main_trunk_id(),
            )
            .unwrap();

        assert_eq!(rule_derive_result.statements.len(), 1);
        assert_eq!(
            rule_derive_result.statements.first().unwrap(),
            &Statement::Singular(
                PredicateLetter('B', Subscript(None), Degree(1)),
                vec![SingularTerm('a', Subscript(None))]
            )
        );
        match rule_derive_result.whatdo {
            ApplyRuleWhatdo::AddToExistingBranches => {}
            ApplyRuleWhatdo::AsNewBranches => assert!(false),
        }
    }

    #[test]
    fn apply_uq_rule() {
        let mut truth_tree_method = TruthTreeMethod::new(&vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]);

        // If no singular term appears on the branch yet, instantiates to random one
        // (algorithm dictates first one will be 'a')
        let rule_derive_result = truth_tree_method
            .apply_rule(
                Rule::UniversalQuantifier,
                &Statement::Universal(
                    Variable('x', Subscript(None)),
                    Box::new(Formula::Predicate(
                        PredicateLetter('B', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                ),
                &truth_tree_method.tree.main_trunk_id(),
            )
            .unwrap();

        assert_eq!(rule_derive_result.statements.len(), 1);
        assert_eq!(
            rule_derive_result.statements.first().unwrap(),
            &Statement::Singular(
                PredicateLetter('B', Subscript(None), Degree(1)),
                vec![SingularTerm('a', Subscript(None))]
            )
        );
        match rule_derive_result.whatdo {
            ApplyRuleWhatdo::AddToExistingBranches => {}
            ApplyRuleWhatdo::AsNewBranches => assert!(false),
        }

        // If singular terms already appear on the branch, instantiate to each one of them at a time
        truth_tree_method
            .tree
            .branch_from_id_mut(&truth_tree_method.tree.main_trunk_id())
            .append_statement(BranchNode {
                statement: Statement::Singular(
                    PredicateLetter('B', Subscript(None), Degree(2)),
                    vec![
                        SingularTerm('a', Subscript(None)),
                        SingularTerm('b', Subscript(None)),
                    ],
                ),
                derived_from: None,
            });

        let rule_derive_result = truth_tree_method
            .apply_rule(
                Rule::UniversalQuantifier,
                &Statement::Universal(
                    Variable('x', Subscript(None)),
                    Box::new(Formula::Predicate(
                        PredicateLetter('B', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                ),
                &truth_tree_method.tree.main_trunk_id(),
            )
            .unwrap();

        assert_eq!(rule_derive_result.statements.len(), 1);
        assert_eq!(
            rule_derive_result.statements.first().unwrap(),
            &Statement::Singular(
                PredicateLetter('B', Subscript(None), Degree(1)),
                vec![SingularTerm('a', Subscript(None))] // Bound to break if code changes, order may change
            )
        );

        match rule_derive_result.whatdo {
            ApplyRuleWhatdo::AddToExistingBranches => {}
            ApplyRuleWhatdo::AsNewBranches => assert!(false),
        }
    }

    #[test]
    fn apply_double_negation_rule() {
        let truth_tree_method = TruthTreeMethod::new(&vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]);

        let rule_derive_result = truth_tree_method
            .apply_rule(
                Rule::DoubleNegation,
                &Statement::LogicalNegation(Box::new(Statement::LogicalNegation(Box::new(
                    Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
                )))),
                &truth_tree_method.tree.main_trunk_id(),
            )
            .unwrap();

        assert_eq!(rule_derive_result.statements.len(), 1);
        assert_eq!(
            rule_derive_result.statements.first().unwrap(),
            &Statement::Simple(SimpleStatementLetter('B', Subscript(None)),)
        );
        match rule_derive_result.whatdo {
            ApplyRuleWhatdo::AddToExistingBranches => {}
            ApplyRuleWhatdo::AsNewBranches => assert!(false),
        }
    }

    #[test]
    fn apply_conjunction_rule() {
        let truth_tree_method = TruthTreeMethod::new(&vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]);

        let rule_derive_result = truth_tree_method
            .apply_rule(
                Rule::Conjunction,
                &Statement::LogicalConjunction(
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'B',
                        Subscript(None),
                    ))),
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'A',
                        Subscript(None),
                    ))),
                ),
                &truth_tree_method.tree.main_trunk_id(),
            )
            .unwrap();

        assert_eq!(rule_derive_result.statements.len(), 2);
        assert_eq!(
            rule_derive_result.statements.first().unwrap(),
            &Statement::Simple(SimpleStatementLetter('B', Subscript(None)),)
        );
        assert_eq!(
            rule_derive_result.statements.last().unwrap(),
            &Statement::Simple(SimpleStatementLetter('A', Subscript(None)))
        );
        match rule_derive_result.whatdo {
            ApplyRuleWhatdo::AddToExistingBranches => {}
            ApplyRuleWhatdo::AsNewBranches => assert!(false),
        }
    }

    #[test]
    fn apply_negation_of_conditional_rule() {
        let truth_tree_method = TruthTreeMethod::new(&vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]);

        let rule_derive_result = truth_tree_method
            .apply_rule(
                Rule::NegationOfConditional,
                &Statement::LogicalNegation(Box::new(Statement::LogicalConditional(
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'B',
                        Subscript(None),
                    ))),
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'A',
                        Subscript(None),
                    ))),
                ))),
                &truth_tree_method.tree.main_trunk_id(),
            )
            .unwrap();

        assert_eq!(rule_derive_result.statements.len(), 2);
        assert_eq!(
            rule_derive_result.statements.first().unwrap(),
            &Statement::Simple(SimpleStatementLetter('B', Subscript(None)),)
        );
        assert_eq!(
            rule_derive_result.statements.last().unwrap(),
            &Statement::LogicalNegation(Box::new(Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None)
            ))))
        );
        match rule_derive_result.whatdo {
            ApplyRuleWhatdo::AddToExistingBranches => {}
            ApplyRuleWhatdo::AsNewBranches => assert!(false),
        }
    }

    #[test]
    fn apply_negation_of_disjunction_rule() {
        let truth_tree_method = TruthTreeMethod::new(&vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]);

        let rule_derive_result = truth_tree_method
            .apply_rule(
                Rule::NegationOfDisjunction,
                &Statement::LogicalNegation(Box::new(Statement::LogicalDisjunction(
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'B',
                        Subscript(None),
                    ))),
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'A',
                        Subscript(None),
                    ))),
                ))),
                &truth_tree_method.tree.main_trunk_id(),
            )
            .unwrap();

        assert_eq!(rule_derive_result.statements.len(), 2);
        assert_eq!(
            rule_derive_result.statements.first().unwrap(),
            &Statement::LogicalNegation(Box::new(Statement::Simple(SimpleStatementLetter(
                'B',
                Subscript(None)
            ),)))
        );
        assert_eq!(
            rule_derive_result.statements.last().unwrap(),
            &Statement::LogicalNegation(Box::new(Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None)
            ))))
        );
        match rule_derive_result.whatdo {
            ApplyRuleWhatdo::AddToExistingBranches => {}
            ApplyRuleWhatdo::AsNewBranches => assert!(false),
        }
    }

    #[test]
    fn apply_conditional_rule() {
        let truth_tree_method = TruthTreeMethod::new(&vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]);

        let rule_derive_result = truth_tree_method
            .apply_rule(
                Rule::Conditional,
                &Statement::LogicalConditional(
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'B',
                        Subscript(None),
                    ))),
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'A',
                        Subscript(None),
                    ))),
                ),
                &truth_tree_method.tree.main_trunk_id(),
            )
            .unwrap();

        assert_eq!(rule_derive_result.statements.len(), 2);
        assert_eq!(
            rule_derive_result.statements.first().unwrap(),
            &Statement::LogicalNegation(Box::new(Statement::Simple(SimpleStatementLetter(
                'B',
                Subscript(None)
            ),)))
        );
        assert_eq!(
            rule_derive_result.statements.last().unwrap(),
            &Statement::Simple(SimpleStatementLetter('A', Subscript(None)))
        );
        match rule_derive_result.whatdo {
            ApplyRuleWhatdo::AddToExistingBranches => assert!(false),
            ApplyRuleWhatdo::AsNewBranches => {}
        }
    }

    #[test]
    fn apply_negation_of_conjunction_rule() {
        let truth_tree_method = TruthTreeMethod::new(&vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]);

        let rule_derive_result = truth_tree_method
            .apply_rule(
                Rule::NegationOfConjunction,
                &Statement::LogicalNegation(Box::new(Statement::LogicalConjunction(
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'B',
                        Subscript(None),
                    ))),
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'A',
                        Subscript(None),
                    ))),
                ))),
                &truth_tree_method.tree.main_trunk_id(),
            )
            .unwrap();

        assert_eq!(rule_derive_result.statements.len(), 2);
        assert_eq!(
            rule_derive_result.statements.first().unwrap(),
            &Statement::LogicalNegation(Box::new(Statement::Simple(SimpleStatementLetter(
                'B',
                Subscript(None)
            ),)))
        );
        assert_eq!(
            rule_derive_result.statements.last().unwrap(),
            &Statement::LogicalNegation(Box::new(Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None)
            ))))
        );
        match rule_derive_result.whatdo {
            ApplyRuleWhatdo::AddToExistingBranches => assert!(false),
            ApplyRuleWhatdo::AsNewBranches => {}
        }
    }

    #[test]
    fn apply_disjunction_rule() {
        let truth_tree_method = TruthTreeMethod::new(&vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]);

        let rule_derive_result = truth_tree_method
            .apply_rule(
                Rule::Disjunction,
                &Statement::LogicalDisjunction(
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'B',
                        Subscript(None),
                    ))),
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'A',
                        Subscript(None),
                    ))),
                ),
                &truth_tree_method.tree.main_trunk_id(),
            )
            .unwrap();

        assert_eq!(rule_derive_result.statements.len(), 2);
        assert_eq!(
            rule_derive_result.statements.first().unwrap(),
            &Statement::Simple(SimpleStatementLetter('B', Subscript(None)),)
        );
        assert_eq!(
            rule_derive_result.statements.last().unwrap(),
            &Statement::Simple(SimpleStatementLetter('A', Subscript(None)))
        );
        match rule_derive_result.whatdo {
            ApplyRuleWhatdo::AddToExistingBranches => assert!(false),
            ApplyRuleWhatdo::AsNewBranches => {}
        }
    }

    #[test]
    fn build_singular_term_stack_for_branch() {
        let mut truth_tree_method = TruthTreeMethod::new(&vec![Statement::Existential(
            Variable('z', Subscript(None)),
            Box::new(Formula::Conjunction(
                Box::new(Formula::Predicate(
                    PredicateLetter('A', Subscript(None), Degree(2)),
                    vec![
                        Term::Variable(Variable('z', Subscript(None))),
                        Term::SingularTerm(SingularTerm('a', Subscript(None))),
                    ],
                )),
                Box::new(Formula::Predicate(
                    PredicateLetter('B', Subscript(None), Degree(2)),
                    vec![
                        Term::SingularTerm(SingularTerm('b', Subscript(None))),
                        Term::Variable(Variable('x', Subscript(None))),
                    ],
                )),
            )),
        )]);

        let child_branch_id = truth_tree_method.tree.append_branch_at(
            Branch::new(vec![BranchNode {
                statement: Statement::LogicalConjunction(
                    Box::new(Statement::Simple(SimpleStatementLetter(
                        'A',
                        Subscript(None),
                    ))),
                    Box::new(Statement::Singular(
                        PredicateLetter('D', Subscript(None), Degree(1)),
                        vec![SingularTerm('d', Subscript(None))],
                    )),
                ),
                derived_from: None,
            }]),
            &truth_tree_method.tree.main_trunk_id(),
        );

        let stack = truth_tree_method.build_singular_term_stack_for_branch(&child_branch_id);

        assert_eq!(stack.len(), 3);
        assert!(stack.contains(&SingularTerm('d', Subscript(None))));
        assert!(stack.contains(&SingularTerm('b', Subscript(None))));
        assert!(stack.contains(&SingularTerm('a', Subscript(None))));
    }

    #[test]
    fn instantiate_quantified_statement() {
        // Quantified statement: ∀x((A¹x & B¹x) ⊃ ∀y((~C¹y) ⊃ ∃z(A²zy & B²zx)))
        // After instantiation:     (A¹a & B¹a) ⊃ ∀y((~C¹y) ⊃ ∃z(A²zy & B²za))
        /*let statement = Statement::Universal(
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
        );*/
    }

    #[test]
    fn instantiates_uq_one_singular_term_at_a_time() {
        let truth_tree_method = TruthTreeMethod::new(&vec![Statement::Universal(
            Variable('x', Subscript(None)),
            Box::new(Formula::Negation(Box::new(Formula::Negation(Box::new(
                Formula::Statement(Box::new(Statement::Existential(
                    Variable('y', Subscript(None)),
                    Box::new(Formula::Predicate(
                        PredicateLetter('A', Subscript(None), Degree(3)),
                        vec![
                            Term::Variable(Variable('x', Subscript(None))),
                            Term::SingularTerm(SingularTerm('a', Subscript(None))),
                            Term::SingularTerm(SingularTerm('b', Subscript(None))),
                        ],
                    )),
                ))),
            ))))),
        )]);

        // Should go UQ (term 'a') -> Double Negation -> Existential -> Singular -> repeat once (term 'b')
        // Relies on priority order for the order in which statements appear in tree
        let truth_tree = truth_tree_method.compute();

        let mut statements_iter = truth_tree
            .branch_from_id(&truth_tree.main_trunk_id())
            .statements();

        statements_iter.next(); // Skip initial statement of tree

        assert_eq!(
            statements_iter.next().unwrap().1.statement,
            Statement::LogicalNegation(Box::new(Statement::LogicalNegation(Box::new(
                Statement::Existential(
                    Variable('y', Subscript(None)),
                    Box::new(Formula::Predicate(
                        PredicateLetter('A', Subscript(None), Degree(3)),
                        vec![
                            Term::SingularTerm(SingularTerm('a', Subscript(None))),
                            Term::SingularTerm(SingularTerm('a', Subscript(None))),
                            Term::SingularTerm(SingularTerm('b', Subscript(None)))
                        ]
                    ))
                )
            ))))
        );

        assert_eq!(
            statements_iter.next().unwrap().1.statement,
            Statement::Existential(
                Variable('y', Subscript(None)),
                Box::new(Formula::Predicate(
                    PredicateLetter('A', Subscript(None), Degree(3)),
                    vec![
                        Term::SingularTerm(SingularTerm('a', Subscript(None))),
                        Term::SingularTerm(SingularTerm('a', Subscript(None))),
                        Term::SingularTerm(SingularTerm('b', Subscript(None))),
                    ]
                ))
            )
        );

        assert_eq!(
            statements_iter.next().unwrap().1.statement,
            Statement::Singular(
                PredicateLetter('A', Subscript(None), Degree(3)),
                vec![
                    SingularTerm('a', Subscript(None)),
                    SingularTerm('a', Subscript(None)),
                    SingularTerm('b', Subscript(None))
                ]
            )
        );

        assert_eq!(
            statements_iter.next().unwrap().1.statement,
            Statement::LogicalNegation(Box::new(Statement::LogicalNegation(Box::new(
                Statement::Existential(
                    Variable('y', Subscript(None)),
                    Box::new(Formula::Predicate(
                        PredicateLetter('A', Subscript(None), Degree(3)),
                        vec![
                            Term::SingularTerm(SingularTerm('b', Subscript(None))),
                            Term::SingularTerm(SingularTerm('a', Subscript(None))),
                            Term::SingularTerm(SingularTerm('b', Subscript(None)))
                        ]
                    ))
                )
            ))))
        );

        assert_eq!(
            statements_iter.next().unwrap().1.statement,
            Statement::Existential(
                Variable('y', Subscript(None)),
                Box::new(Formula::Predicate(
                    PredicateLetter('A', Subscript(None), Degree(3)),
                    vec![
                        Term::SingularTerm(SingularTerm('b', Subscript(None))),
                        Term::SingularTerm(SingularTerm('a', Subscript(None))),
                        Term::SingularTerm(SingularTerm('b', Subscript(None))),
                    ]
                ))
            )
        );

        assert_eq!(
            statements_iter.next().unwrap().1.statement,
            Statement::Singular(
                PredicateLetter('A', Subscript(None), Degree(3)),
                vec![
                    SingularTerm('b', Subscript(None)),
                    SingularTerm('a', Subscript(None)),
                    SingularTerm('b', Subscript(None))
                ]
            )
        );
    }

    // TEST TEST
    #[test]
    fn handles_some_potential_infinite_loops() {
        let truth_tree_method = TruthTreeMethod::new(&vec![Statement::LogicalConditional(
            Box::new(Statement::Universal(
                Variable('x', Subscript(None)),
                Box::new(Formula::Conditional(
                    Box::new(Formula::Predicate(
                        PredicateLetter('D', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                    Box::new(Formula::Predicate(
                        PredicateLetter('P', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                )),
            )),
            Box::new(Statement::Universal(
                Variable('x', Subscript(None)),
                Box::new(Formula::Conditional(
                    Box::new(Formula::Predicate(
                        PredicateLetter('S', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                    Box::new(Formula::Predicate(
                        PredicateLetter('P', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                )),
            )),
        )]);

        truth_tree_method.compute();
    }
}
