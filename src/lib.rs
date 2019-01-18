extern crate pest;
#[macro_use]
extern crate pest_derive;

mod interpreter;

pub use interpreter::ast::{
    Degree, ParseTree, Predicate, Relation, RelationLetter, SimpleStatementLetter, SingularTerm,
    Statement, Term, Variable,
};

pub use interpreter::parser::parse;

#[cfg(test)]
mod tests {
    use super::interpreter::*;

    #[test]
    fn test() {
        match interpreter::parse("∃x(D³xba & D¹x), (~B & A) .:. (~C ∨ A₂)") {
            Ok(parse_tree) => println!("{:?}", parse_tree),
            Err(e) => println!("{}", e.formatted_message),
        }
    }
}
