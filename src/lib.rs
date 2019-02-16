#![doc(html_root_url = "https://docs.rs/logic_rs/0.1.0")]

//! # Overview
//! logic_rs is a parser of relational predicate logic (better known as 'first-order logic') 
//! and truth tree solver. It is heavily influenced by the book _Meaning and Argument: 
//! An Introduction to Logic Through Language_, by Ernest Lepore and Sam Cumming, trying
//! to follow as closely as possible its grammar and rules.
//! 
//! It uses separate syntax for statement sets ('{' <statements..> '}'), arguments
//! (<statements..> '∴' < statement >), and sole statements (< statement >), and so
//! can automatically generate and analyse truth trees accordingly.
//! 
//! The website for this library, which can be found [here](https://ixjf.github.io/logic-rs/),
//! provides a full-featured demo of it.
//! 
//! # Usage
//! Validating some formula is as simple as:
//! 
//! ```
//! # use std::error::Error;
//! # use logic_rs::parse_input;
//! # fn main() -> Result<(), Box<Error>> {
//! match parse_input("(∀x)(B¹x ⊃ (L²xm ⊃ L²bx))") {
//!     Ok(input_kind) => {
//!         // Input is a well-formed formula
//!     },
//!     Err(parse_err) => {
//!         // Input is **not** a well-formed formula
//!     }
//! }
//! #
//! #     Ok(())
//! # }
//! ```
//! 
//! And proving that the input above is a sole statement and that that statement is a contingency
//! is just as simple:
//! 
//! ```
//! # use std::error::Error;
//! # use logic_rs::{parse_input, InputKind};
//! # fn main() -> Result<(), Box<Error>> {
//! match parse_input("(∀x)(B¹x ⊃ (L²xm ⊃ L²bx))") {
//!     Ok(input_kind) => match input_kind {
//!         InputKind::Statement(st) => {
//!             let (
//!                 is_contingency,
//!                 truth_tree_statement, 
//!                 truth_tree_negation_of_stmt
//!                 ) = st.is_contingency();
//!             
//!             assert_eq!(is_contingency, true);
//!         },
//!         _ => assert!(false)
//!     },
//!     Err(parse_err) => assert!(false)
//! }
//! #
//! #     Ok(())
//! # }
//! ```
//! 
//! Essentially, the flow is always:
//!  - Call [parse_input](fn.parse_input.html)
//!  - Match on the result of the call to know the kind of the input (statement set, argument, single statement)
//!  - Call one of the is_* methods of the input kind to run the truth tree algorithm
//!  - Do something with the truth tree or the analysis of it
//! 
//! # Language and Truth Tree Algorithm
//! A specification of the language can be found [here](https://github.com/ixjf/logic-rs/wiki/Language).
//! 
//! Note that because the language as specified allows an infinite number of different
//! constants, the truth tree algorithm **will** get stuck if the set of initial statements
//! lead to an infinite tree. There are ways around this, but they involve either limiting
//! the universe of discourse and still generating needlessly huge truth trees, or
//! allowing one to stop the moment one branch is finished and open, which is still unreliable
//! and leads to half-done trees.
//! 
//! It is obviously not intended that this stay this way - no library should do such a thing.
//! However, either way it goes, because predicate logic **is** undecidable, any change
//! will always be a workaround and will always be unreliable.
//! 
//! One thing can be guaranteed, though, and that is that, _unless there is a bug_, the algorithm
//! will **always** correctly classify an unsatisfiable set of statements, which also implies that
//! if it does get stuck in an infinite loop, then the initial set of statements is satisfiable.

extern crate pest;
#[macro_use]
extern crate pest_derive;

extern crate id_tree;

#[cfg(feature = "serde_support")]
#[macro_use]
extern crate serde;

extern crate snowflake;

mod parser;
mod validity;

use parser::Input;
use parser::Parser;
use validity::TruthTreeMethod;

pub use parser::ParseError;
pub use parser::{
    Degree, Formula, PredicateLetter, SimpleStatementLetter, SingularTerm, Statement, Subscript,
    Term, Variable,
};
pub use validity::{
    Branch, BranchImmediateChildrenIdsIter, BranchNode, BranchNodeLocation, DerivationId, IdsIter,
    Rule, StatementsIter, TreeId, TruthTree, UpwardsBranchIdsIter, UpwardsBranchesIter,
};

/// Represents the statement set parsed from the input, through which one can
/// check its consistency.
pub struct StatementSet {
    statements: Vec<Statement>,
}

impl StatementSet {
    /// Returns as the first field of the tuple a boolean representing whether
    /// the statement set is consistent, and the proof truth tree as the second.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # use std::error::Error;
    /// # use logic_rs::{parse_input, InputKind};
    /// # fn main() -> Result<(), Box<Error>> {
    /// let parsed_input = parse_input("{(A & B)}")?;
    /// 
    /// match parsed_input {
    ///     InputKind::StatementSet(st_set) => {
    ///         let (is_consistent, truth_tree) = st_set.is_consistent();
    ///         assert_eq!(is_consistent, true);
    ///     },
    ///     _ => assert!(false)
    /// }
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    /// 
    pub fn is_consistent(&self) -> (bool, TruthTree) {
        let truth_tree = TruthTreeMethod::new(&self.statements).compute();

        // A statement set is consistent if at least one branch stays open,
        // i.e. it's possible for all the statements to be true at the same
        // time
        (truth_tree.is_open(), truth_tree)
    }
}

/// Represents the argument parsed from the input, through which one can
/// check its formal validity.
pub struct Argument {
    premises: Vec<Statement>,
    conclusion: Statement,
}

impl Argument {
    /// Returns as the first field of the tuple a boolean representing whether
    /// the argument is formally valid, and the proof truth tree as the second.
    ///
    /// # Examples
    /// 
    /// ```
    /// # use std::error::Error;
    /// # use logic_rs::{parse_input, InputKind};
    /// # fn main() -> Result<(), Box<Error>> {
    /// let parsed_input = parse_input("A ∴ B")?;
    /// 
    /// match parsed_input {
    ///     InputKind::Argument(arg) => {
    ///         let (is_valid, truth_tree) = arg.is_valid();
    ///         assert_eq!(is_valid, false);
    ///     },
    ///     _ => assert!(false)
    /// }
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    /// 
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

/// Represents the one statement parsed from the input, through which one can
/// check its logical properties, such as whether it is a contradiction,
/// tautology, or contingency.
pub struct SingleStatement {
    statement: Statement,
}

impl SingleStatement {
    /// Returns as the first field of the tuple a boolean representing whether
    /// the statement is a contradiction, and the proof truth tree as the second.
    ///
    /// # Examples
    /// 
    /// ```
    /// # use std::error::Error;
    /// # use logic_rs::{parse_input, InputKind};
    /// # fn main() -> Result<(), Box<Error>> {
    /// let parsed_input = parse_input("(A & ~A)")?;
    /// 
    /// match parsed_input {
    ///     InputKind::Statement(st) => {
    ///         let (is_contradiction, truth_tree) = st.is_contradiction();
    ///         assert_eq!(is_contradiction, true);
    ///     },
    ///     _ => assert!(false)
    /// }
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    /// 
    pub fn is_contradiction(&self) -> (bool, TruthTree) {
        let truth_tree = TruthTreeMethod::new(&vec![self.statement.clone()]).compute();

        // A statement is a contradiction if all branches for a truth tree
        // with that single initial statement close
        // i.e. it's not possible for the statement to be true
        (!truth_tree.is_open(), truth_tree)
    }

    /// Returns as the first field of the tuple a boolean representing whether
    /// the statement is a tautology, and the proof truth tree as the second.
    ///
    /// # Examples
    /// 
    /// ```
    /// # use std::error::Error;
    /// # use logic_rs::{parse_input, InputKind};
    /// # fn main() -> Result<(), Box<Error>> {
    /// let parsed_input = parse_input("(A ∨ ~A)")?;
    /// 
    /// match parsed_input {
    ///     InputKind::Statement(st) => {
    ///         let (is_tautology, truth_tree) = st.is_tautology();
    ///         assert_eq!(is_tautology, true);
    ///     },
    ///     _ => assert!(false)
    /// }
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    /// 
    pub fn is_tautology(&self) -> (bool, TruthTree) {
        // A statement is a tautology if its negation is a contradiction
        // So we negate self.statement and do the same thing as in
        // is_contradiction
        let statement = Statement::LogicalNegation(Box::new(self.statement.clone()));
        let truth_tree = TruthTreeMethod::new(&vec![statement]).compute();

        (!truth_tree.is_open(), truth_tree)
    }

    /// Returns as the first field of the tuple a boolean representing whether
    /// the statement is a contingency, and the proof truth trees as second and third.
    /// The first truth tree proves that the statement is not a contradiction,
    /// while the second truth tree proves that the statement is also not
    /// a tautology.
    ///
    /// # Examples
    /// 
    /// ```
    /// # use std::error::Error;
    /// # use logic_rs::{parse_input, InputKind};
    /// # fn main() -> Result<(), Box<Error>> {
    /// let parsed_input = parse_input("A")?;
    /// 
    /// match parsed_input {
    ///     InputKind::Statement(st) => {
    ///         let (
    ///             is_contingency,
    ///             truth_tree_contradiction,
    ///             truth_tree_tautology
    ///             ) = st.is_contingency();
    ///         assert_eq!(is_contingency, true);
    ///     },
    ///     _ => assert!(false)
    /// }
    /// #
    /// #    Ok(())
    /// # }
    /// ```
    /// 
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

/// Represents the parsed input.
pub enum InputKind {
    StatementSet(StatementSet),
    Argument(Argument),
    Statement(SingleStatement),
}

/// Parses some statement set, argument, or statement. Takes in input
/// as a string following the specification from 
/// [here](https://github.com/ixjf/logic-rs/wiki/Language).
/// 
/// Fails if the input is syntatically or semantically invalid.
pub fn parse_input(input: &str) -> Result<InputKind, ParseError> {
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
