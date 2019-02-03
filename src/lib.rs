extern crate pest;
#[macro_use]
extern crate pest_derive;

extern crate id_tree;

mod parser;
mod validity;

use parser::Error;
use parser::Parser;
use parser::{Input, Statement};
use validity::TruthTreeMethod;

pub use validity::TruthTree;

// An empty string will lead to a parse error
pub fn validate(input: &str) -> Result<TruthTree, Error> {
    let parser = Parser::new();

    match parser.parse(input) {
        Ok(parse_tree) => match parse_tree.0 {
            Input::StatementSet(ref statements) => Ok(TruthTreeMethod::new(&statements).compute()),

            Input::Argument(ref premises, ref conclusion) => {
                // Transform into statement list '<premise>, <premise>,..., negation of <conclusion>'
                let mut statements = premises.clone();
                statements.push(Statement::LogicalNegation(Box::new(conclusion.clone())));

                Ok(TruthTreeMethod::new(&statements).compute())
            }
        },
        Err(e) => Err(e),
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn validate() {
        // Doesn't fail when input is valid
        match super::validate("{A, B}") {
            Ok(_) => {}
            Err(e) => assert!(false, e),
        }

        match super::validate("A, B ∴ C") {
            Ok(_) => {}
            Err(e) => assert!(false, e),
        }

        // Fails when input is invalid
        match super::validate("A") {
            Ok(_) => assert!(false),
            Err(_) => {}
        }
    }
}
