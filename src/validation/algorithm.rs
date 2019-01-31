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
        let rule_priority_order: [Rule; 7] = [
            Rule::Conditional,
            Rule::NegationOfConjunction,
            Rule::Disjunction,
            Rule::DoubleNegation,
            Rule::Conjunction,
            Rule::NegationOfConditional,
            Rule::NegationOfDisjunction,
        ];

        match (&self.rule, &other.rule) {
            (Some(_), None) => Ordering::Greater,
            (None, Some(_)) => Ordering::Less,
            (Some(rule_a), Some(rule_b)) => rule_priority_order
                .iter()
                .position(|x| *x == *rule_a)
                .unwrap()
                .cmp(
                    &rule_priority_order
                        .iter()
                        .position(|x| *x == *rule_b)
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
    queue: BinaryHeap<QueueNode>,
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
            queue: BinaryHeap::new(),
        }
    }

    pub fn compute(&mut self) {
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

                self.queue.push(QueueNode {
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
        }) = self.queue.pop()
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

            // TODO: Because the queue is ordered, this won't be checked right when a contradiction appears
            // Fix this without duplicating code
            if self.statement_is_contradiction(&statement, &branch_id) {
                self.tree.branch_from_id_mut(&branch_id).close();
                continue;
            }

            match rule {
                Some(rule) => {
                    let result = self.apply_rule(rule.clone(), &statement);

                    let open_branches_ids = self
                        .tree
                        .branch_id_iter(&branch_id)
                        .filter(|x| {
                            !self.tree.branch_from_id(&x).is_closed()
                                && self.tree.branch_is_last_child(&x)
                        })
                        .collect::<Vec<_>>();

                    for child_branch_id in open_branches_ids {
                        match result.whatdo {
                            DerivedRuleWhatdo::AddToExistingBranches => {
                                // Add derived statements to all open branches at the end of the tree
                                // (i.e. branches that have no children)

                                result.statements.iter().for_each(|x| {
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

                                    // Add derived statement to queue for further processing
                                    let new_node = QueueNode {
                                        statement_id: new_statement_id.clone(),
                                        statement: x.clone(),
                                        rule: self.matches_some_rule(&x),
                                        branch_id: child_branch_id.clone(),
                                    };
                                    self.queue.push(new_node);
                                });
                            }
                            DerivedRuleWhatdo::AsNewBranches => {
                                // Each derived statement will be a new child branch on every open
                                // branch that is at the end of the tree
                                result.statements.iter().for_each(|x| {
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

                                    // Add derived statement to queue for further processing
                                    let new_node = QueueNode {
                                        statement_id: root_statement_id,
                                        statement: x.clone(),
                                        rule: self.matches_some_rule(&x),
                                        branch_id: new_branch_id,
                                    };

                                    self.queue.push(new_node);
                                });
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
