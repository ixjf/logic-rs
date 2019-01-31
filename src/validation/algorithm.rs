use super::statement_tree::*;
use crate::parser::{SimpleStatementLetter, Statement, Subscript}; // FIXME
use std::cmp::Ordering;
use std::collections::BinaryHeap;

#[derive(Clone, PartialEq, Eq, Debug)]
enum Rule {
    Conditional,
    NegationOfConjunction,
    Disjunction,
    DoubleNegation,
    Conjunction,
    NegationOfConditional,
    NegationOfDisjunction,
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
        let rule_priority_order: [Rule; 7] = [
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

#[derive(Clone, Debug)]
struct TreeNodeLocation(Id, Id); // Statement ID, Branch ID

#[derive(Clone, Debug)]
struct TreeNode {
    statement: Statement,
    derived_from: Option<(TreeNodeLocation, Rule)>,
}

pub struct TruthTreeMethod {
    tree: StatementTree<TreeNode>,
}

impl TruthTreeMethod {
    pub fn new(statements: &Vec<Statement>) -> Self {
        TruthTreeMethod {
            tree: StatementTree::new(Branch::new(
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

    pub fn compute(&mut self) {
        let mut queue = BinaryHeap::new();

        // Populate the queue with the main trunk
        let main_trunk_id = self.tree.main_trunk();

        {
            let main_trunk = self.tree.branch_from_id(&main_trunk_id);

            for statement_id in main_trunk.statement_id_iter() {
                let statement = main_trunk
                    .statement_from_id(&statement_id)
                    .statement
                    .clone();
                let rule = self.matches_some_rule(&statement);

                queue.push(QueueNode {
                    statement_id: statement_id.clone(),
                    statement,
                    rule,
                    branch_id: main_trunk_id.clone(),
                });
            }
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
                .branch_id_iter(&main_trunk_id)
                .filter(|x| {
                    !self.tree.branch_from_id(&x).is_closed() && self.tree.branch_is_last_child(&x)
                })
                .count()
                == 0
            {
                // There are no open branches in the entire tree, stop
                return;
            }

            match rule {
                Some(rule) => {
                    let result = self.apply_rule(rule.clone(), &statement);

                    // Open child branches of branch where original statement is
                    let open_branches_ids = self
                        .tree
                        .branch_id_iter(&branch_id)
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
                                            .append(TreeNode {
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
                                            new_branch.statement_id_iter().next().unwrap();

                                        let new_branch_id =
                                            self.tree.append_branch(new_branch, &child_branch_id);

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

                            // If the statement contradicts another in child_branch_id or
                            // any of its parents, keep the statement in the branch
                            // but close it and move on to the next one
                            if self.statement_is_contradiction(&x, &derived_statement_branch_id) {
                                self.tree
                                    .branch_from_id_mut(&derived_statement_branch_id)
                                    .close();
                                break;
                            }
                        }
                    }
                }
                None => {} // No rule was applied, statement is already on tree, so we do nothing here
            }

            // Original rules don't need to be marked done
            // The algorithm doesn't need it, and you can know which are 'done'
            // by checking the IDs that statements derive from
        }
    }

    fn statement_is_contradiction(&self, statement: &Statement, branch_id: &Id) -> bool {
        // Usually, you'd think to only iterate towards the root of the tree
        // to find a contradiction, however, it's easier if we accept the entire
        // branch the statement is on. After all, if there is a contradiction
        // anywhere in the branch, whether it be above or below (which can't really
        // happen in the current state of things of 'compute'), the branch will
        // close.

        for ancestor_branch_id in self.tree.reverse_branch_id_iter(&branch_id) {
            let ancestor_branch = self.tree.branch_from_id(&ancestor_branch_id);

            for statement_id in ancestor_branch.statement_id_iter() {
                let node = ancestor_branch.statement_from_id(&statement_id);

                match (&node.statement, statement) {
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
        if self.can_apply_conditional_rule(&statement) {
            Some(Rule::Conditional)
        } else if self.can_apply_negation_of_conjunction_rule(&statement) {
            Some(Rule::NegationOfConjunction)
        } else if self.can_apply_disjunction_rule(&statement) {
            Some(Rule::Disjunction)
        } else if self.can_apply_double_negation_rule(&statement) {
            Some(Rule::DoubleNegation)
        } else if self.can_apply_conjunction_rule(&statement) {
            Some(Rule::Conjunction)
        } else if self.can_apply_negation_of_conditional_rule(&statement) {
            Some(Rule::NegationOfConditional)
        } else if self.can_apply_negation_of_disjunction_rule(&statement) {
            Some(Rule::NegationOfDisjunction)
        } else {
            None
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

    fn apply_rule(&self, rule: Rule, statement: &Statement) -> RuleDeriveResult {
        match rule {
            Rule::Conditional => self.apply_conditional_rule(&statement),
            Rule::NegationOfConjunction => self.apply_negation_of_conjunction_rule(&statement),
            Rule::Disjunction => self.apply_disjunction_rule(&statement),
            Rule::DoubleNegation => self.apply_double_negation_rule(&statement),
            Rule::Conjunction => self.apply_conjunction_rule(&statement),
            Rule::NegationOfConditional => self.apply_negation_of_conditional_rule(&statement),
            Rule::NegationOfDisjunction => self.apply_negation_of_disjunction_rule(&statement),
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn queue_node_priority_order_correct() {
        let branch = Branch::new(vec!["mock"]);
        let mock_id = branch.statement_id_iter().next().unwrap();
        let mock_statement = Statement::Simple(SimpleStatementLetter('A', Subscript(None)));

        let mut queue = BinaryHeap::new();

        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::DoubleNegation),
            branch_id: mock_id.clone(),
        });

        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::Conjunction),
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
            rule: Some(Rule::NegationOfDisjunction),
            branch_id: mock_id.clone(),
        });

        queue.push(QueueNode {
            statement_id: mock_id.clone(),
            statement: mock_statement.clone(),
            rule: Some(Rule::Conditional),
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
            rule: Some(Rule::Disjunction),
            branch_id: mock_id.clone(),
        });

        assert_eq!(queue.pop().unwrap().rule, Some(Rule::DoubleNegation));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::Conjunction));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::NegationOfConditional));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::NegationOfDisjunction));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::Conditional));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::NegationOfConjunction));
        assert_eq!(queue.pop().unwrap().rule, Some(Rule::Disjunction));
    }

    #[test]
    fn truth_tree_method_works_more_or_less() {
        let mut algo = TruthTreeMethod::new(&vec![
            Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
            Statement::LogicalConjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'B',
                    Subscript(None),
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'C',
                    Subscript(None),
                ))),
            ),
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

        algo.compute();

        for branch_id in algo.tree.branch_id_iter(&algo.tree.main_trunk()) {
            println!("Branch ID: {:#?}", branch_id.clone());

            let branch = algo.tree.branch_from_id(&branch_id);

            println!("Branch is closed: {0}", branch.is_closed());

            for statement_id in branch.statement_id_iter() {
                let node = branch.statement_from_id(&statement_id);

                println!("- Statement ID: {:#?}", statement_id.clone());

                println!("- Statement data: ");

                println!("  - Statement: {:#?}", node.statement.clone());
                println!("  - Derived from: {:#?}", node.derived_from.clone());
            }
        }
    }
}
