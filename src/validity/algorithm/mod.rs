mod rules;

use super::truth_tree::*;
use crate::parser::Statement;
use snowflake::ProcessUniqueId;
use std::cmp::Ordering;
use std::collections::BinaryHeap;

/// The ID of some derivation. A derivation is any application of some rule to some statement.
///
/// This ID is guaranteed to be unique for each different application of any rule. This ID
/// is useful to analyse the truth tree and know which statements are part of the same derivation,
/// since multiple statements may be derived from the same statement and rule, and yet not be
/// the result of the _same_ application of a rule (e.g. the universal quantifier rule
/// can be applied infinitely many times to the same statement).
///
/// **Serialization of this struct requires the feature `serde_support` to be enabled.**
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
pub struct DerivationId {
    pub(super) id: ProcessUniqueId, // Identifies a node that comes from the same derivation as another with the same ID
    // Note that this doesn't mean a node that is derived from the same statement, but rather that it is part of the same
    // vector of resulting statements from the application of ONE rule at ONE point in time
    // So e.g. deriving UQ twice leads to two statements that were derived from the same statement,
    // but they don't have the same derivation ID because they come from two different applications of a rule
    // On the other hand, deriving with the conditional rule leads to two statements, derived from the same statement,
    // and ALSO from the same application of the rule,
    //
    //
    /// A unique ID within this DerivationId. Two statements resulting from the same application of a rule
    /// added to the same branch will have different values for this field, but those two statements on a sibling branch
    /// will mirror the IDs from the previous branch. This is especially useful to construct a graphical representation
    /// of the truth tree: in order to group statements from the same derivation by level, it is necessary
    /// to actually know which statements match which. However, simply comparing the two statements is unreliable,
    /// since sometimes two different statements from the same derivation, as is the case of the result of the application of a
    /// conditional rule, _should_ be on the same line. If such a graphical representation is to not mix together
    /// on the same line different statements (other than the case mentioned and alike), even if from the same derivation,
    /// comparing against this field is required.
    ///
    /// **Note that comparing two instances of this field is not the same thing as comparing two `DerivationId`s.**
    pub index: u64,
}

/// A rule of the truth tree algorithm.
///
/// **Serialization of this enum requires the feature `serde_support` to be enabled.**
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
    statement_id: TreeId,
    statement: Statement,
    rule: Option<(Rule, bool)>,
    branch_id: TreeId,
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
        for (statement_id, branch_node) in self
            .tree
            .branch_from_id(&self.tree.main_trunk_id())
            .statements()
        {
            queue.push(QueueEntry {
                statement_id: statement_id.clone(),
                statement: branch_node.statement.clone(),
                rule: self.matches_some_rule(&branch_node.statement),
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
                        .traverse_downwards_branches_ids(&branch_id)
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

                                derivation_id.index = 0;

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

    fn statement_is_contradiction(&self, statement: &Statement, branch_id: &TreeId) -> bool {
        // Usually, you'd think to only iterate towards the root of the tree
        // to find a contradiction, however, it's easier if we accept the entire
        // branch the statement is on. After all, if there is a contradiction
        // anywhere in the branch, whether it be above or below (which can't really
        // happen in the current state of things of 'compute' anyway), the branch will
        // close.

        for (_, ancestor_branch) in self.tree.traverse_upwards_branches(&branch_id) {
            for (_, branch_node) in ancestor_branch.statements() {
                match (&branch_node.statement, statement) {
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{
        Degree, Formula, PredicateLetter, SimpleStatementLetter, SingularTerm, Statement,
        Subscript, Term, Variable,
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
