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
    match statement {
        st @ Statement::SingularStatement(_, _) => {
            validate_singular_statement(st);
        }
        st @ Statement::ExistentialQuantifierStatement(_, _)
        | st @ Statement::UniversalQuantifierStatement(_, _) => {
            validate_quantifier_statement(st);
        }
        _ => {}
    }
}

fn validate_singular_statement(statement: &Statement) -> () {
    match statement {
        Statement::SingularStatement(relation, terms) => {
            if relation.1 != terms.len() {
                unimplemented!();
            }
        }
        _ => unreachable!(),
    }
}

fn validate_quantifier_statement(statement: &Statement) -> () {
    match statement {
        Statement::ExistentialQuantifierStatement(var, predicate)
        | Statement::UniversalQuantifierStatement(var, predicate) => {
            let mut var_stack = Vec::new();
            var_stack.push(var.clone());
            validate_predicate(predicate, &mut var_stack);
        }
        _ => unreachable!(),
    }
}

fn validate_predicate(predicate: &Predicate, var_stack: &mut Vec<Variable>) -> () {
    match predicate {
        Predicate::SimplePredicate(relation, terms) => {
            if relation.1 != terms.len() {
                unimplemented!();
            }

            if !terms.iter().any(|x| match x {
                Term::Variable(var) => var_stack.contains(var),
                _ => false,
            }) {
                unimplemented!();
            }
        }
        Predicate::ConjunctivePredicate(lpredicate, rpredicate) => {
            validate_predicate(lpredicate, &mut var_stack.clone());
            validate_predicate(rpredicate, &mut var_stack.clone());
            unimplemented!();
        }
        Predicate::NegativePredicate(rpredicate) => {
            validate_predicate(rpredicate, &mut var_stack.clone());
            unimplemented!();
        }
        Predicate::DisjunctivePredicate(lpredicate, rpredicate) => {
            validate_predicate(lpredicate, &mut var_stack.clone());
            validate_predicate(rpredicate, &mut var_stack.clone());
            unimplemented!();
        }
        Predicate::ConditionalPredicate(lpredicate, rpredicate) => {
            validate_predicate(lpredicate, &mut var_stack.clone());
            validate_predicate(rpredicate, &mut var_stack.clone());
            unimplemented!();
        }
    }
}
