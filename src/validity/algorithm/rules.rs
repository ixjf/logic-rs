use super::{ApplyRuleWhatdo, Rule, RuleDeriveResult, TruthTreeMethod};
use crate::parser::{Formula, SingularTerm, Statement, Subscript, Term, Variable};
use crate::validity::truth_tree::TreeId;
use std::iter::once;

impl TruthTreeMethod {
    pub(super) fn matches_some_rule(&self, statement: &Statement) -> Option<(Rule, bool)> {
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

    pub(super) fn apply_rule(
        &self,
        rule: Rule,
        statement: &Statement,
        branch_id: &TreeId,
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

    fn apply_eq_rule(&self, statement: &Statement, branch_id: &TreeId) -> Option<RuleDeriveResult> {
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

    fn apply_uq_rule(&self, statement: &Statement, branch_id: &TreeId) -> Option<RuleDeriveResult> {
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
                            for (_, branch_node) in ancestor_branch.statements() {
                                if instantiated_statement == branch_node.statement {
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

    fn build_singular_term_stack_for_branch(&self, branch_id: &TreeId) -> Vec<SingularTerm> {
        let mut stack = Vec::new();

        for (_, ancestor_branch) in self.tree.traverse_upwards_branches(&branch_id) {
            for (_, branch_node) in ancestor_branch.statements() {
                self.find_singular_terms_in_statement(&mut stack, &branch_node.statement);
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
    use crate::parser::{Degree, PredicateLetter, SimpleStatementLetter};
    use crate::validity::truth_tree::{Branch, BranchNode};

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
}
