use super::ast::*;
use pest::{error::LineColLocation, Parser};

#[derive(Parser)]
#[grammar = "GRAMMAR.pest"]
struct MyParser;

pub struct Error {
    pub pos: (usize, usize),
    pub formatted_message: String,
}

pub fn parse<'a>(input: &'a str) -> Result<ParseTree, Error> {
    match MyParser::parse(Rule::input, input) {
        Ok(pairs) => {
            let parse_tree = into_ast(pairs);

            match &parse_tree {
                ParseTree::StatementSet(statements) => {
                    statements.iter().for_each(|x| analyse_semantics(x))
                }
                ParseTree::Argument(premises, conclusion) => {
                    premises.iter().for_each(|x| analyse_semantics(x));
                    analyse_semantics(&conclusion);
                }
            };

            Ok(parse_tree)
        }
        Err(e) => {
            let pos = match e.line_col {
                LineColLocation::Pos((line, col)) => (line, col),
                _ => unreachable!(),
            };

            Err(Error {
                pos,
                formatted_message: format!("{}", e),
            })
        }
    }
}

fn analyse_semantics(statement: &Statement) -> () {
    //match statement {

    //}
}
