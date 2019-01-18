use super::parser::Rule;
use pest::iterators::{Pair, Pairs};
use std::str::Chars;

// TODO: Should these borrow instead? &str instead of String
// Make pair args pass as reference
// I'm not sure I like masking all these types
pub type SimpleStatementLetter = String;

pub type SingularTerm = String;

pub type Variable = String;

pub type Degree = usize;

pub type RelationLetter = String;

#[derive(Debug)]
pub struct Relation(pub RelationLetter, pub Degree);

#[derive(Debug)]
pub enum Term {
    SingularTerm(SingularTerm),
    Variable(Variable),
}

#[derive(Debug)]
pub enum Predicate {
    SimplePredicate(Relation, Vec<Term>),
    ConjunctivePredicate(Box<Predicate>, Box<Predicate>),
    NegativePredicate(Box<Predicate>),
    DisjunctivePredicate(Box<Predicate>, Box<Predicate>),
    ConditionalPredicate(Box<Predicate>, Box<Predicate>),
}

#[derive(Debug)]
pub enum Statement {
    SimpleStatement(SimpleStatementLetter),
    SingularStatement(Relation, Vec<SingularTerm>),
    LogicalConjunction(Box<Statement>, Box<Statement>),
    LogicalNegation(Box<Statement>),
    LogicalDisjunction(Box<Statement>, Box<Statement>),
    LogicalConditional(Box<Statement>, Box<Statement>),
    ExistentialQuantifierStatement(Variable, Predicate),
    UniversalQuantifierStatement(Variable, Predicate),
}

#[derive(Debug)]
pub enum ParseTree {
    StatementSet(Vec<Statement>),
    Argument(Vec<Statement>, Statement),
}

pub fn into_ast(mut pairs: Pairs<'_, Rule>) -> ParseTree {
    let inner = pairs.next().unwrap().into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::statement_set => parse_statement_set(inner),
        Rule::argument => parse_argument(inner),
        _ => unreachable!(),
    }
}

fn parse_statement_set(pair: Pair<Rule>) -> ParseTree {
    let mut statements: Vec<Statement> = Vec::new();

    for statement_pair in pair.into_inner() {
        statements.push(parse_statement(statement_pair));
    }

    ParseTree::StatementSet(statements)
}

fn parse_argument(pair: Pair<Rule>) -> ParseTree {
    let mut statements: Vec<Statement> = Vec::new();

    for statement_pair in pair.into_inner() {
        match statement_pair.as_rule() {
            Rule::premise | Rule::conclusion => {
                statements.push(parse_statement(statement_pair.into_inner().next().unwrap()))
            }
            _ => unreachable!(),
        }
    }

    // The grammar guarantees us that the conclusion comes last.
    // So, the back of the vector will be the conclusion.
    let conclusion = statements.pop().unwrap();

    ParseTree::Argument(statements, conclusion)
}

fn parse_statement(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().next().unwrap();

    match inner.as_rule() {
        Rule::complex_statement => parse_complex_statement(inner),
        Rule::simple_statement => parse_simple_statement(inner),
        _ => unreachable!(),
    }
}

fn parse_complex_statement(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().next().unwrap();

    match inner.as_rule() {
        Rule::logical_conjunction => parse_logical_conjunction(inner),
        Rule::logical_negation => parse_logical_negation(inner),
        Rule::logical_disjunction => parse_logical_disjunction(inner),
        Rule::logical_conditional => parse_logical_conditional(inner),
        Rule::existential_quantifier_statement => parse_existential_quantifier_statement(inner),
        Rule::universal_quantifier_statement => parse_universal_quantifier_statement(inner),
        _ => unreachable!(),
    }
}

fn parse_logical_conjunction(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let lstatement = parse_statement(inner.next().unwrap());
    let rstatement = parse_statement(inner.next().unwrap());

    Statement::LogicalConjunction(Box::new(lstatement), Box::new(rstatement))
}

fn parse_logical_negation(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let rstatement = parse_statement(inner.next().unwrap());

    Statement::LogicalNegation(Box::new(rstatement))
}

fn parse_logical_disjunction(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let lstatement = parse_statement(inner.next().unwrap());
    let rstatement = parse_statement(inner.next().unwrap());

    Statement::LogicalDisjunction(Box::new(lstatement), Box::new(rstatement))
}

fn parse_logical_conditional(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let lstatement = parse_statement(inner.next().unwrap());
    let rstatement = parse_statement(inner.next().unwrap());

    Statement::LogicalConditional(Box::new(lstatement), Box::new(rstatement))
}

fn parse_existential_quantifier_statement(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let variable = inner.next().unwrap().as_str().to_owned();
    let predicate = parse_predicate(inner.next().unwrap());

    Statement::ExistentialQuantifierStatement(variable, predicate)
}

fn parse_universal_quantifier_statement(pair: Pair<Rule>) -> Statement {
    let mut inner = pair.into_inner();

    let variable = inner.next().unwrap().as_str().to_owned();
    let predicate = parse_predicate(inner.next().unwrap());

    Statement::UniversalQuantifierStatement(variable, predicate)
}

fn parse_simple_statement(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().next().unwrap();

    match inner.as_rule() {
        Rule::singular_statement => {
            let mut sng_st_inner = inner.into_inner();

            let mut bound_relation = sng_st_inner.next().unwrap().into_inner();

            let mut relation_inner = bound_relation.next().unwrap().into_inner();

            let relation_letter = relation_inner.next().unwrap().as_str().to_owned();
            let superscript_number =
                superscript_to_number(relation_inner.next().unwrap().as_str().chars());

            let terms = relation_inner.map(|x| x.as_str().to_owned()).collect(); // remaining children are singular terms

            Statement::SingularStatement(Relation(relation_letter, superscript_number), terms)
        }
        Rule::simple_statement_letter => Statement::SimpleStatement(inner.as_str().to_owned()),
        _ => unreachable!(),
    }
}

fn parse_predicate(pair: Pair<Rule>) -> Predicate {
    let inner = pair.into_inner().next().unwrap();

    match inner.as_rule() {
        Rule::compound_predicate => parse_compound_predicate(inner),
        Rule::simple_predicate => parse_simple_predicate(inner),
        _ => unreachable!(),
    }
}

fn parse_compound_predicate(pair: Pair<Rule>) -> Predicate {
    let inner = pair.into_inner().next().unwrap();

    match inner.as_rule() {
        Rule::conjunctive_predicate => parse_conjunctive_predicate(inner),
        Rule::negative_predicate => parse_negative_predicate(inner),
        Rule::disjunctive_predicate => parse_disjunctive_predicate(inner),
        Rule::conditional_predicate => parse_conditional_predicate(inner),
        _ => unreachable!(),
    }
}

fn parse_conjunctive_predicate(pair: Pair<Rule>) -> Predicate {
    let mut inner = pair.into_inner();

    let lpredicate = parse_predicate(inner.next().unwrap());
    let rpredicate = parse_predicate(inner.next().unwrap());

    Predicate::ConjunctivePredicate(Box::new(lpredicate), Box::new(rpredicate))
}

fn parse_negative_predicate(pair: Pair<Rule>) -> Predicate {
    let mut inner = pair.into_inner();

    let rpredicate = parse_predicate(inner.next().unwrap());

    Predicate::NegativePredicate(Box::new(rpredicate))
}

fn parse_disjunctive_predicate(pair: Pair<Rule>) -> Predicate {
    let mut inner = pair.into_inner();

    let lpredicate = parse_predicate(inner.next().unwrap());
    let rpredicate = parse_predicate(inner.next().unwrap());

    Predicate::DisjunctivePredicate(Box::new(lpredicate), Box::new(rpredicate))
}

fn parse_conditional_predicate(pair: Pair<Rule>) -> Predicate {
    let mut inner = pair.into_inner();

    let lpredicate = parse_predicate(inner.next().unwrap());
    let rpredicate = parse_predicate(inner.next().unwrap());

    Predicate::ConditionalPredicate(Box::new(lpredicate), Box::new(rpredicate))
}

fn parse_simple_predicate(pair: Pair<Rule>) -> Predicate {
    let inner = pair.into_inner().next().unwrap();

    let mut unbound_relation = inner.into_inner();

    let mut relation_inner = unbound_relation.next().unwrap().into_inner();

    let relation_letter = relation_inner.next().unwrap().as_str().to_owned();

    let superscript_number = superscript_to_number(relation_inner.next().unwrap().as_str().chars());

    let terms = unbound_relation
        .map(|x| match x.as_rule() {
            Rule::singular_term => Term::SingularTerm(x.as_str().to_owned()),
            Rule::variable => Term::Variable(x.as_str().to_owned()),
            _ => unreachable!(),
        })
        .collect(); // remaining children are terms

    Predicate::SimplePredicate(Relation(relation_letter, superscript_number), terms)
}

fn superscript_to_number(iter: Chars) -> usize {
    iter.map(|x| match x {
        '\u{2070}' => '0',
        '\u{00B9}' => '1',
        '\u{00B2}' => '2',
        '\u{00B3}' => '3',
        '\u{2074}' => '4',
        '\u{2075}' => '5',
        '\u{2076}' => '6',
        '\u{2077}' => '7',
        '\u{2078}' => '8',
        '\u{2079}' => '9',
        _ => unreachable!(),
    })
    .collect::<String>()
    .parse()
    .unwrap()
}
