extern crate pest;
#[macro_use]
extern crate pest_derive;

mod parser;

pub use parser::ast::{
    Degree, ParseTree, Predicate, Relation, RelationLetter, SimpleStatementLetter, SingularTerm,
    Statement, Term, Variable,
};

pub use parser::parser::parse;

#[cfg(test)]
mod tests {
    use super::parser::*;

    #[test]
    fn test() {
        match parser::parse("∃x(D³xa & D¹y), (~B & A) .:. (~C ∨ A₂)") {
            Ok(parse_tree) => println!("{:?}", parse_tree),
            Err(e) => println!("{}", e.formatted_message),
        }
    }
}
