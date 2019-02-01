mod algorithm;
mod truth_tree;

use self::algorithm::{TreeNode, TruthTreeMethod};
use self::truth_tree::TruthTree;
use crate::parser::{Input, ParseTree, Statement};

// FIXME Algorithm doesn't check validity of input

pub enum Error {
    NotAStatementSet,
    NotAnArgument,
    MoreThanOneStatement,
}

pub fn is_statement_set_consistent(
    parse_tree: &ParseTree,
) -> Result<(bool, TruthTree<TreeNode>), Error> {
    match parse_tree.0 {
        Input::StatementSet(ref statements) => {
            let truth_tree = TruthTreeMethod::new(&statements).compute();

            // At least one branch must be open
            Ok((is_consistent(&truth_tree), truth_tree))
        }
        _ => Err(Error::NotAStatementSet),
    }
}

pub fn is_argument_valid(parse_tree: &ParseTree) -> Result<(bool, TruthTree<TreeNode>), Error> {
    match parse_tree.0 {
        Input::Argument(ref premises, ref conclusion) => {
            // Pass statement list as '<premise>, <premise>,..., negation of <conclusion>'
            let mut statements = premises.clone();
            statements.push(Statement::LogicalNegation(Box::new(conclusion.clone())));

            let truth_tree = TruthTreeMethod::new(&statements).compute();

            // All branches must close
            Ok((!is_consistent(&truth_tree), truth_tree))
        }
        _ => Err(Error::NotAnArgument),
    }
}

pub fn is_statement_contradiction(
    parse_tree: &ParseTree,
) -> Result<(bool, TruthTree<TreeNode>), Error> {
    match parse_tree.0 {
        Input::StatementSet(ref statements) => {
            if statements.len() > 1 {
                return Err(Error::MoreThanOneStatement);
            }

            let truth_tree =
                TruthTreeMethod::new(&vec![statements.first().unwrap().clone()]).compute();

            // All branches must close
            Ok((!is_consistent(&truth_tree), truth_tree))
        }
        _ => Err(Error::NotAStatementSet),
    }
}

pub fn is_statement_tautology(
    parse_tree: &ParseTree,
) -> Result<(bool, TruthTree<TreeNode>), Error> {
    match parse_tree.0 {
        Input::StatementSet(ref statements) => {
            if statements.len() > 1 {
                return Err(Error::MoreThanOneStatement);
            }

            // If a statement is a tautology, its negation is a contradiction
            // Hence same logic as is_statement_contradiction, except
            // we pass the negation of the input rather than the input as is
            let truth_tree = TruthTreeMethod::new(&vec![Statement::LogicalNegation(Box::new(
                statements.first().unwrap().clone(),
            ))])
            .compute();

            // All branches must close
            Ok((!is_consistent(&truth_tree), truth_tree))
        }
        _ => Err(Error::NotAStatementSet),
    }
}

pub fn is_statement_contingency(
    parse_tree: &ParseTree,
) -> Result<(bool, TruthTree<TreeNode>, TruthTree<TreeNode>), Error> {
    let truth_tree_contradiction = is_statement_contradiction(&parse_tree)?;
    let truth_tree_tautology = is_statement_tautology(&parse_tree)?;

    // If a statement is a contingency, then it is neither a contradiction
    // nor a tautology
    Ok((
        !truth_tree_contradiction.0 && !truth_tree_tautology.0,
        truth_tree_contradiction.1,
        truth_tree_tautology.1,
    ))
}

fn is_consistent(truth_tree: &TruthTree<TreeNode>) -> bool {
    truth_tree
        .branch_id_iter(&truth_tree.main_trunk_id())
        .filter(|x| {
            truth_tree.branch_is_last_child(&x) && !truth_tree.branch_from_id(&x).is_closed()
        })
        .count()
        > 0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{
        Degree, Predicate, PredicateLetter, SimpleStatementLetter, SingularTerm, Statement,
        Subscript, Term, Variable,
    };

    #[test]
    fn is_statement_set_consistent() {
        // Returns true when statement set is consistent
        let parse_tree_1 = ParseTree(Input::StatementSet(vec![
            Statement::LogicalDisjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'A',
                    Subscript(None),
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'D',
                    Subscript(None),
                ))),
            ),
            Statement::LogicalNegation(Box::new(Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None),
            )))),
        ]));

        match super::is_statement_set_consistent(&parse_tree_1) {
            Ok((is_consistent, _)) => {
                assert!(is_consistent, "returned set 1 is not consistent but it is")
            }
            Err(_) => assert!(false, "failed but set 1 is a statement set"),
        }

        // Returns false when statement set is not consistent
        let parse_tree_2 = ParseTree(Input::StatementSet(vec![
            Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
            Statement::LogicalNegation(Box::new(Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None),
            )))),
        ]));

        match super::is_statement_set_consistent(&parse_tree_2) {
            Ok((is_consistent, _)) => {
                assert!(!is_consistent, "returned set 2 is consistent but it isn't")
            }
            Err(_) => assert!(false, "failed but set 2 is a statement set"),
        }

        // Fails when input is an argument
        let parse_tree_4 = ParseTree(Input::Argument(
            vec![Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None),
            ))],
            Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
        ));

        match super::is_statement_set_consistent(&parse_tree_4) {
            Ok(_) => assert!(false, "returned Ok when input is invalid"),
            Err(e) => match e {
                Error::NotAStatementSet => {}
                _ => assert!(false, "returned wrong Error"),
            },
        }
    }

    #[test]
    fn is_argument_valid() {
        // Returns false when argument is invalid
        let parse_tree_1 = ParseTree(Input::Argument(
            vec![
                Statement::LogicalConjunction(
                    Box::new(Statement::Singular(
                        PredicateLetter('K', Subscript(None), Degree(1)),
                        vec![SingularTerm('a', Subscript(None))],
                    )),
                    Box::new(Statement::Singular(
                        PredicateLetter('B', Subscript(None), Degree(1)),
                        vec![SingularTerm('b', Subscript(None))],
                    )),
                ),
                Statement::Existential(
                    Variable('x', Subscript(None)),
                    Predicate::Conjunctive(
                        Box::new(Predicate::Simple(
                            PredicateLetter('K', Subscript(None), Degree(1)),
                            vec![Term::Variable(Variable('x', Subscript(None)))],
                        )),
                        Box::new(Predicate::Simple(
                            PredicateLetter('D', Subscript(None), Degree(1)),
                            vec![Term::Variable(Variable('x', Subscript(None)))],
                        )),
                    ),
                ),
                Statement::Universal(
                    Variable('x', Subscript(None)),
                    Predicate::Conditional(
                        Box::new(Predicate::Simple(
                            PredicateLetter('D', Subscript(None), Degree(1)),
                            vec![Term::Variable(Variable('x', Subscript(None)))],
                        )),
                        Box::new(Predicate::Simple(
                            PredicateLetter('B', Subscript(None), Degree(1)),
                            vec![Term::Variable(Variable('x', Subscript(None)))],
                        )),
                    ),
                ),
            ],
            Statement::LogicalDisjunction(
                Box::new(Statement::Singular(
                    PredicateLetter('B', Subscript(None), Degree(1)),
                    vec![SingularTerm('a', Subscript(None))],
                )),
                Box::new(Statement::Singular(
                    PredicateLetter('K', Subscript(None), Degree(1)),
                    vec![SingularTerm('b', Subscript(None))],
                )),
            ),
        ));

        match super::is_argument_valid(&parse_tree_1) {
            Ok((is_valid, _)) => assert!(!is_valid, "returned argument 1 is valid but it isn't"),
            Err(_) => assert!(false, "failed but parse tree is argument"),
        }

        // Returns true when argument is valid
        let parse_tree_2 = ParseTree(Input::Argument(
            vec![Statement::LogicalNegation(Box::new(
                Statement::Existential(
                    Variable('x', Subscript(None)),
                    Predicate::Conjunctive(
                        Box::new(Predicate::Simple(
                            PredicateLetter('A', Subscript(None), Degree(1)),
                            vec![Term::Variable(Variable('x', Subscript(None)))],
                        )),
                        Box::new(Predicate::Negative(Box::new(Predicate::Simple(
                            PredicateLetter('B', Subscript(None), Degree(1)),
                            vec![Term::Variable(Variable('x', Subscript(None)))],
                        )))),
                    ),
                ),
            ))],
            Statement::Existential(
                Variable('x', Subscript(None)),
                Predicate::Conditional(
                    Box::new(Predicate::Simple(
                        PredicateLetter('A', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                    Box::new(Predicate::Simple(
                        PredicateLetter('B', Subscript(None), Degree(1)),
                        vec![Term::Variable(Variable('x', Subscript(None)))],
                    )),
                ),
            ),
        ));

        match super::is_argument_valid(&parse_tree_2) {
            Ok((is_valid, _)) => assert!(is_valid, "returned argument 2 is not valid but it is"),
            Err(_) => assert!(false, "failed but parse tree is argument"),
        }

        // Fails when input is not an argument
        let parse_tree_3 = ParseTree(Input::StatementSet(vec![Statement::Simple(
            SimpleStatementLetter('A', Subscript(None)),
        )]));

        match super::is_argument_valid(&parse_tree_3) {
            Ok(_) => assert!(false, "returned Ok when input is invalid"),
            Err(e) => match e {
                Error::NotAnArgument => {}
                _ => assert!(false, "returned wrong Error"),
            },
        }
    }

    #[test]
    fn is_statement_contradiction() {
        // Returns true when statement is a contradiction
        let parse_tree_1 = ParseTree(Input::StatementSet(vec![Statement::LogicalConjunction(
            Box::new(Statement::LogicalConjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'P',
                    Subscript(None),
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'Q',
                    Subscript(None),
                ))),
            )),
            Box::new(Statement::LogicalConjunction(
                Box::new(Statement::LogicalNegation(Box::new(Statement::Simple(
                    SimpleStatementLetter('P', Subscript(None)),
                )))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'Q',
                    Subscript(None),
                ))),
            )),
        )]));

        match super::is_statement_contradiction(&parse_tree_1) {
            Ok((is_contradiction, _)) => assert!(
                is_contradiction,
                "returned statement 1 is not a contradiction but it is"
            ),
            Err(_) => assert!(false, "failed but parse tree is valid"),
        }

        // Returns false when statement is not a contradiction
        let parse_tree_2 = ParseTree(Input::StatementSet(vec![Statement::LogicalDisjunction(
            Box::new(Statement::Simple(SimpleStatementLetter(
                'P',
                Subscript(None),
            ))),
            Box::new(Statement::LogicalNegation(Box::new(Statement::Simple(
                SimpleStatementLetter('Q', Subscript(None)),
            )))),
        )]));

        match super::is_statement_contradiction(&parse_tree_2) {
            Ok((is_contradiction, _)) => assert!(
                !is_contradiction,
                "returned statement 1 is a contradiction but it is"
            ),
            Err(_) => assert!(false, "failed but parse tree is valid"),
        }

        // Fails when input is not a single statement in a statement set
        let parse_tree_3 = ParseTree(Input::StatementSet(vec![
            Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
            Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
        ]));

        match super::is_statement_contradiction(&parse_tree_3) {
            Ok(_) => assert!(false, "returned Ok when input is invalid"),
            Err(e) => match e {
                Error::MoreThanOneStatement => {}
                _ => assert!(false, "returned wrong Error"),
            },
        }

        // Fails when input is an argument
        let parse_tree_4 = ParseTree(Input::Argument(
            vec![Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None),
            ))],
            Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
        ));

        match super::is_statement_contradiction(&parse_tree_4) {
            Ok(_) => assert!(false, "returned Ok when input is invalid"),
            Err(e) => match e {
                Error::NotAStatementSet => {}
                _ => assert!(false, "returned wrong Error"),
            },
        }
    }

    #[test]
    fn is_statement_tautology() {
        // Returns true when statement is a tautology
        let parse_tree_1 = ParseTree(Input::StatementSet(vec![Statement::LogicalConditional(
            Box::new(Statement::LogicalConjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'P',
                    Subscript(None),
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'Q',
                    Subscript(None),
                ))),
            )),
            Box::new(Statement::LogicalDisjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'P',
                    Subscript(None),
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'Q',
                    Subscript(None),
                ))),
            )),
        )]));

        match super::is_statement_tautology(&parse_tree_1) {
            Ok((is_tautology, _)) => assert!(
                is_tautology,
                "returned statement 1 is not a tautology but it is"
            ),
            Err(_) => assert!(false, "failed but parse tree 1 is a statement"),
        }

        // Returns false when statement is not a tautology
        let parse_tree_2 = ParseTree(Input::StatementSet(vec![Statement::LogicalConjunction(
            Box::new(Statement::Simple(SimpleStatementLetter(
                'P',
                Subscript(None),
            ))),
            Box::new(Statement::LogicalConditional(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'Q',
                    Subscript(None),
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'P',
                    Subscript(None),
                ))),
            )),
        )]));

        match super::is_statement_tautology(&parse_tree_2) {
            Ok((is_tautology, _)) => assert!(
                !is_tautology,
                "returned statement 1 is a tautology but it isn't"
            ),
            Err(_) => assert!(false, "failed but parse tree 1 is a statement"),
        }

        // Fails when input is not a single statement in a statement set
        let parse_tree_3 = ParseTree(Input::StatementSet(vec![
            Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
            Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
        ]));

        match super::is_statement_tautology(&parse_tree_3) {
            Ok(_) => assert!(false, "returned Ok when input is invalid"),
            Err(e) => match e {
                Error::MoreThanOneStatement => {}
                _ => assert!(false, "returned wrong Error"),
            },
        }

        // Fails when input is an argument
        let parse_tree_4 = ParseTree(Input::Argument(
            vec![Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None),
            ))],
            Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
        ));

        match super::is_statement_tautology(&parse_tree_4) {
            Ok(_) => assert!(false, "returned Ok when input is invalid"),
            Err(e) => match e {
                Error::NotAStatementSet => {}
                _ => assert!(false, "returned wrong Error"),
            },
        }
    }

    #[test]
    fn is_statement_contingency() {
        // Returns true when statement is a contingency
        let parse_tree_1 = ParseTree(Input::StatementSet(vec![Statement::LogicalConjunction(
            Box::new(Statement::Simple(SimpleStatementLetter(
                'P',
                Subscript(None),
            ))),
            Box::new(Statement::LogicalConditional(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'Q',
                    Subscript(None),
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'P',
                    Subscript(None),
                ))),
            )),
        )]));

        match super::is_statement_contingency(&parse_tree_1) {
            Ok((is_contingency, _, _)) => assert!(
                is_contingency,
                "returned statement 1 is not a contingency but it is"
            ),
            Err(_) => assert!(false, "failed but parse tree 1 is a statement"),
        }

        // Returns false when statement is not a contingency
        let parse_tree_2 = ParseTree(Input::StatementSet(vec![Statement::LogicalConditional(
            Box::new(Statement::LogicalConjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'P',
                    Subscript(None),
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'Q',
                    Subscript(None),
                ))),
            )),
            Box::new(Statement::LogicalDisjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'P',
                    Subscript(None),
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'Q',
                    Subscript(None),
                ))),
            )),
        )]));

        match super::is_statement_contingency(&parse_tree_2) {
            Ok((is_contingency, _, _)) => assert!(
                !is_contingency,
                "returned statement 2 is a contingency but it isn't"
            ),
            Err(_) => assert!(false, "failed but parse tree 2 is a statement"),
        }

        // Fails when input is not a single statement in a statement set
        let parse_tree_3 = ParseTree(Input::StatementSet(vec![
            Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
            Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
        ]));

        match super::is_statement_contingency(&parse_tree_3) {
            Ok(_) => assert!(false, "returned Ok when input is invalid"),
            Err(e) => match e {
                Error::MoreThanOneStatement => {}
                _ => assert!(false, "returned wrong Error"),
            },
        }

        // Fails when input is an argument
        let parse_tree_4 = ParseTree(Input::Argument(
            vec![Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None),
            ))],
            Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
        ));

        match super::is_statement_contingency(&parse_tree_4) {
            Ok(_) => assert!(false, "returned Ok when input is invalid"),
            Err(e) => match e {
                Error::NotAStatementSet => {}
                _ => assert!(false, "returned wrong Error"),
            },
        }
    }
}
