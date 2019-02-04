extern crate pest;
#[macro_use]
extern crate pest_derive;

extern crate id_tree;

#[cfg(feature = "serde_support")]
#[macro_use]
extern crate serde;

mod parser;
mod validity;

use parser::Error;
use parser::Parser;
use parser::{Input, Statement};
use validity::TruthTreeMethod;

pub use validity::TruthTree;

pub struct StatementSet {
    statements: Vec<Statement>,
}

impl StatementSet {
    pub fn is_consistent(&self) -> (bool, TruthTree) {
        let truth_tree = TruthTreeMethod::new(&self.statements).compute();

        // A statement set is consistent if at least one branch stays open,
        // i.e. it's possible for all the statements to be true at the same
        // time
        (truth_tree.is_open(), truth_tree)
    }
}

pub struct Argument {
    premises: Vec<Statement>,
    conclusion: Statement,
}

impl Argument {
    pub fn is_valid(&self) -> (bool, TruthTree) {
        // Transform into statement list of form '<premise>, <premise>,...,negation of <conclusion>'
        let mut statements = self.premises.clone();
        statements.push(Statement::LogicalNegation(Box::new(
            self.conclusion.clone(),
        )));

        let truth_tree = TruthTreeMethod::new(&statements).compute();

        // An argument is valid if all branches close, i.e. it's not possible for
        // the premises to be true while the conclusion is false
        (!truth_tree.is_open(), truth_tree)
    }
}

pub struct SingleStatement {
    statement: Statement,
}

impl SingleStatement {
    pub fn is_contradiction(&self) -> (bool, TruthTree) {
        let truth_tree = TruthTreeMethod::new(&vec![self.statement.clone()]).compute();

        // A statement is a contradiction if all branches for a truth tree
        // with that single initial statement close
        // i.e. it's not possible for the statement to be true
        (!truth_tree.is_open(), truth_tree)
    }

    pub fn is_tautology(&self) -> (bool, TruthTree) {
        // A statement is a tautology if its negation is a contradiction
        // So we negate self.statement and do the same thing as in
        // is_contradiction
        let statement = Statement::LogicalNegation(Box::new(self.statement.clone()));
        let truth_tree = TruthTreeMethod::new(&vec![statement]).compute();

        (!truth_tree.is_open(), truth_tree)
    }

    pub fn is_contingency(&self) -> (bool, TruthTree, TruthTree) {
        // A statement is a contingency if it's neither a contradiction
        // nor a tautology
        let is_contradiction = self.is_contradiction();
        let is_tautology = self.is_tautology();

        (
            !is_contradiction.0 && !is_tautology.0,
            is_contradiction.1,
            is_tautology.1,
        )
    }
}

pub enum InputKind {
    StatementSet(StatementSet),
    Argument(Argument),
    Statement(SingleStatement),
}

pub fn parse_input(input: &str) -> Result<InputKind, Error> {
    let parser = Parser::new();

    match parser.parse(input) {
        Ok(parse_tree) => match parse_tree.0 {
            Input::StatementSet(statements) => {
                Ok(InputKind::StatementSet(StatementSet { statements }))
            }
            Input::Argument(premises, conclusion) => Ok(InputKind::Argument(Argument {
                premises,
                conclusion,
            })),
            Input::Statement(statement) => Ok(InputKind::Statement(SingleStatement { statement })),
        },
        Err(e) => Err(e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::{SimpleStatementLetter, Subscript};

    #[test]
    fn parse_input() {
        // Succeeds when input is valid
        match super::parse_input("{A}") {
            Ok(_) => {}
            Err(_) => assert!(false),
        }

        match super::parse_input("A ∴ B") {
            Ok(_) => {}
            Err(_) => assert!(false),
        }

        // Fails when input is invalid
        match super::parse_input("") {
            Ok(_) => assert!(false),
            Err(_) => {}
        }
    }

    #[test]
    fn statement_set_is_consistent() {
        // Succeeds when statement set is consistent
        let statement_set = StatementSet {
            statements: vec![Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None),
            ))],
        };

        assert_eq!(statement_set.is_consistent().0, true);

        // Fails when statement set is not consistent
        let statement_set = StatementSet {
            statements: vec![
                Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
                Statement::LogicalNegation(Box::new(Statement::Simple(SimpleStatementLetter(
                    'A',
                    Subscript(None),
                )))),
            ],
        };

        assert_eq!(statement_set.is_consistent().0, false);
    }

    #[test]
    fn argument_is_valid() {
        // Succeeds when argument is valid
        let argument = Argument {
            premises: vec![Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None),
            ))],
            conclusion: Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
        };

        assert_eq!(argument.is_valid().0, true);

        // Fails when argument is invalid
        let argument = Argument {
            premises: vec![Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None),
            ))],
            conclusion: Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
        };

        assert_eq!(argument.is_valid().0, false);
    }

    #[test]
    fn single_statement_is_contradiction() {
        // Succeeds when statement is a contradiction
        let single_statement = SingleStatement {
            statement: Statement::LogicalConjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'A',
                    Subscript(None),
                ))),
                Box::new(Statement::LogicalNegation(Box::new(Statement::Simple(
                    SimpleStatementLetter('A', Subscript(None)),
                )))),
            ),
        };

        assert_eq!(single_statement.is_contradiction().0, true);

        // Fails when statement is not a contradiction
        let single_statement = SingleStatement {
            statement: Statement::LogicalConjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'A',
                    Subscript(None),
                ))),
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'B',
                    Subscript(None),
                ))),
            ),
        };

        assert_eq!(single_statement.is_contradiction().0, false);
    }

    #[test]
    fn single_statement_is_tautology() {
        // Succeeds when statement is a tautology
        let single_statement = SingleStatement {
            statement: Statement::LogicalDisjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'A',
                    Subscript(None),
                ))),
                Box::new(Statement::LogicalNegation(Box::new(Statement::Simple(
                    SimpleStatementLetter('A', Subscript(None)),
                )))),
            ),
        };

        assert_eq!(single_statement.is_tautology().0, true);

        // Fails when statement is not a tautology
        let single_statement = SingleStatement {
            statement: Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
        };

        assert_eq!(single_statement.is_tautology().0, false);
    }

    #[test]
    fn single_statement_is_contingency() {
        // Succeeds when statement is contingent
        let single_statement = SingleStatement {
            statement: Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
        };

        assert_eq!(single_statement.is_contingency().0, true);

        // Fails when statement is not contingent
        let single_statement = SingleStatement {
            statement: Statement::LogicalConjunction(
                Box::new(Statement::Simple(SimpleStatementLetter(
                    'A',
                    Subscript(None),
                ))),
                Box::new(Statement::LogicalNegation(Box::new(Statement::Simple(
                    SimpleStatementLetter('A', Subscript(None)),
                )))),
            ),
        };

        assert_eq!(single_statement.is_contingency().0, false);
    }
}
