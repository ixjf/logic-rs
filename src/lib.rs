extern crate pest;
#[macro_use]
extern crate pest_derive;

mod parser;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let parser = parser::Parser::new();
        match parser.parse("∃x(D³xaa & D¹x), (~B & A) .:. (~C ∨ A₂)") {
            Ok(parse_tree) => println!("{:?}", parse_tree),
            Err(e) => println!("{}", e.decorated_message),
        }
    }
}
