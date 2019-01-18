#[derive(Debug, PartialEq, Eq)]
pub struct SimpleStatementLetter(pub char);

impl PartialEq<char> for SimpleStatementLetter {
    fn eq(&self, rhs: &char) -> bool {
        self.0 == *rhs
    }
}
#[derive(Debug, PartialEq, Eq)]
pub struct SingularTerm(pub char);

impl PartialEq<char> for SingularTerm {
    fn eq(&self, rhs: &char) -> bool {
        self.0 == *rhs
    }
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Variable(pub char);

impl PartialEq<char> for Variable {
    fn eq(&self, rhs: &char) -> bool {
        self.0 == *rhs
    }
}
#[derive(Debug, PartialEq, Eq)]
pub struct Degree(pub usize);

impl PartialEq<usize> for Degree {
    fn eq(&self, rhs: &usize) -> bool {
        self.0 == *rhs
    }
}
#[derive(Debug, PartialEq, Eq)]
pub struct PredicateLetter(pub char, pub Degree);

#[derive(Debug)]
pub enum ParseTree {
    StatementSet(Vec<Statement>),
    Argument(Vec<Statement>, Statement),
}

#[derive(Debug)]
pub enum Term {
    SingularTerm(SingularTerm),
    Variable(Variable),
}

#[derive(Debug)]
pub enum Statement {
    Simple(SimpleStatementLetter),
    Singular(PredicateLetter, Vec<SingularTerm>),
    LogicalConjunction(Box<Statement>, Box<Statement>),
    LogicalNegation(Box<Statement>),
    LogicalDisjunction(Box<Statement>, Box<Statement>),
    LogicalConditional(Box<Statement>, Box<Statement>),
    Existential(Variable, Predicate),
    Universal(Variable, Predicate),
}

#[derive(Debug)]
pub enum Predicate {
    Simple(PredicateLetter, Vec<Term>),
    Conjunctive(Box<Predicate>, Box<Predicate>),
    Negative(Box<Predicate>),
    Disjunctive(Box<Predicate>, Box<Predicate>),
    Conditional(Box<Predicate>, Box<Predicate>),
}

mod pest_parser {
    use pest::Parser;

    #[derive(Parser)]
    #[grammar = "GRAMMAR.pest"]
    pub struct GeneratedParser;
}

use self::pest_parser::Rule;
use pest::error::Error as pest_error;
use pest::error::ErrorVariant as pest_error_variant;
use pest::iterators::{Pair, Pairs};
use pest::Span;

pub struct Error {
    pub decorated_message: String,
    pub position: (usize, usize),
}

impl Error {
    pub(in parser) fn new_from_custom_error(span: Span, decorated_message: &str) -> Self {
        let e: pest_error<Rule> = pest_error::new_from_span(
            pest_error_variant::CustomError {
                message: decorated_message.to_owned(),
            },
            span.clone(),
        );

        Error {
            position: span.start_pos().line_col(),
            decorated_message: format!("{}", e),
        }
    }

    pub(in parser) fn new_from_parsing_error(e: pest_error<Rule>) -> Error {
        use pest::error::LineColLocation;

        let position = match e.line_col {
            LineColLocation::Pos((line, col)) => (line, col),
            _ => unreachable!(), // is this actually unreachable? it's not documented
        };

        Error {
            position,
            decorated_message: format!("{}", e),
        }
    }
}

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Parser {}
    }

    pub fn parse<'a>(&self, input: &'a str) -> Result<ParseTree, Error> {
        use pest::Parser;

        match pest_parser::GeneratedParser::parse(Rule::input, input) {
            Ok(p) => self.into_ast(p),
            Err(e) => Err(Error::new_from_parsing_error(e)),
        }
    }

    fn into_ast(&self, mut pairs: Pairs<'_, Rule>) -> Result<ParseTree, Error> {
        let inner = pairs.next().unwrap().into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::statement_set => self.statement_set_into_ast(inner),
            Rule::argument => self.argument_into_ast(inner),
            _ => unreachable!("should never reach here"),
        }
    }

    fn statement_set_into_ast(&self, pair: Pair<Rule>) -> Result<ParseTree, Error> {
        assert!(pair.as_rule() == Rule::statement_set);

        let mut statements = Vec::new();

        for st_pair in pair.into_inner() {
            match self.statement_into_ast(st_pair) {
                Ok(st) => statements.push(st),
                Err(e) => return Err(e),
            }
        }

        Ok(ParseTree::StatementSet(statements))
    }

    fn statement_into_ast(&self, pair: Pair<Rule>) -> Result<Statement, Error> {
        assert!(pair.as_rule() == Rule::statement);

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::complex_statement => self.complex_statement_into_ast(inner),
            Rule::simple_statement => self.simple_statement_into_ast(inner),
            _ => unreachable!("should never reach here"),
        }
    }

    fn complex_statement_into_ast(&self, pair: Pair<Rule>) -> Result<Statement, Error> {
        assert!(pair.as_rule() == Rule::complex_statement);

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::logical_conjunction => self.logical_conjunction_into_ast(inner),
            Rule::logical_negation => self.logical_negation_into_ast(inner),
            Rule::logical_disjunction => self.logical_disjunction_into_ast(inner),
            Rule::logical_conditional => self.logical_conditional_into_ast(inner),
            Rule::existential_statement => self.existential_statement_into_ast(inner),
            Rule::universal_statement => self.universal_statement_into_ast(inner),
            _ => unreachable!("should never reach here"),
        }
    }

    fn logical_conjunction_into_ast(&self, pair: Pair<Rule>) -> Result<Statement, Error> {
        assert!(pair.as_rule() == Rule::logical_conjunction);

        let mut inner = pair.into_inner();

        let lstatement = self.statement_into_ast(inner.next().unwrap())?;
        let rstatement = self.statement_into_ast(inner.next().unwrap())?;

        Ok(Statement::LogicalConjunction(
            Box::new(lstatement),
            Box::new(rstatement),
        ))
    }

    fn logical_negation_into_ast(&self, pair: Pair<Rule>) -> Result<Statement, Error> {
        assert!(pair.as_rule() == Rule::logical_negation);

        let mut inner = pair.into_inner();

        let rstatement = self.statement_into_ast(inner.next().unwrap())?;

        Ok(Statement::LogicalNegation(Box::new(rstatement)))
    }

    fn logical_disjunction_into_ast(&self, pair: Pair<Rule>) -> Result<Statement, Error> {
        assert!(pair.as_rule() == Rule::logical_disjunction);

        let mut inner = pair.into_inner();

        let lstatement = self.statement_into_ast(inner.next().unwrap())?;
        let rstatement = self.statement_into_ast(inner.next().unwrap())?;

        Ok(Statement::LogicalDisjunction(
            Box::new(lstatement),
            Box::new(rstatement),
        ))
    }

    fn logical_conditional_into_ast(&self, pair: Pair<Rule>) -> Result<Statement, Error> {
        assert!(pair.as_rule() == Rule::logical_conditional);

        let mut inner = pair.into_inner();

        let lstatement = self.statement_into_ast(inner.next().unwrap())?;
        let rstatement = self.statement_into_ast(inner.next().unwrap())?;

        Ok(Statement::LogicalConditional(
            Box::new(lstatement),
            Box::new(rstatement),
        ))
    }

    fn existential_statement_into_ast(&self, pair: Pair<Rule>) -> Result<Statement, Error> {
        assert!(pair.as_rule() == Rule::existential_statement);

        let mut inner = pair.into_inner();

        let variable = Variable(inner.next().unwrap().as_str().chars().next().unwrap());

        let mut stack = Vec::new();
        stack.push(variable.clone());
        let predicate = self.predicate_into_ast(inner.next().unwrap(), &mut stack)?;

        Ok(Statement::Existential(variable, predicate))
    }

    fn universal_statement_into_ast(&self, pair: Pair<Rule>) -> Result<Statement, Error> {
        assert!(pair.as_rule() == Rule::universal_statement);

        let mut inner = pair.into_inner();

        let variable = Variable(inner.next().unwrap().as_str().chars().next().unwrap());

        let mut stack = Vec::new();
        stack.push(variable.clone());
        let predicate = self.predicate_into_ast(inner.next().unwrap(), &mut stack)?;

        Ok(Statement::Universal(variable, predicate))
    }

    fn simple_statement_into_ast(&self, pair: Pair<Rule>) -> Result<Statement, Error> {
        assert!(pair.as_rule() == Rule::simple_statement);

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::singular_statement => self.singular_statement_into_ast(inner),
            Rule::simple_statement_letter => Ok(Statement::Simple(SimpleStatementLetter(
                inner.as_str().chars().next().unwrap(),
            ))),
            _ => unreachable!("should never reach here"),
        }
    }

    fn singular_statement_into_ast(&self, pair: Pair<Rule>) -> Result<Statement, Error> {
        assert!(pair.as_rule() == Rule::singular_statement);

        let mut inner = pair.clone().into_inner();

        let predicate_letter = self.predicate_letter_into_ast(inner.next().unwrap());

        let terms = inner
            .map(|x| match x.as_rule() {
                Rule::singular_term => SingularTerm(x.as_str().chars().next().unwrap()),
                _ => unreachable!("should never reach here"),
            })
            .collect::<Vec<SingularTerm>>();

        if predicate_letter.1 != terms.len() {
            return Err(Error::new_from_custom_error(
                pair.as_span(),
                "degree doesn't match number of terms specified",
            ));
        }

        Ok(Statement::Singular(predicate_letter, terms))
    }

    fn predicate_letter_into_ast(&self, pair: Pair<Rule>) -> PredicateLetter {
        assert!(pair.as_rule() == Rule::predicate_letter);

        let mut inner = pair.into_inner();

        let predicate_letter_inner = inner.next().unwrap().as_str().chars().next().unwrap();

        let superscript_number = Degree(
            inner
                .next()
                .unwrap()
                .as_str()
                .chars()
                .map(|x| match x {
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
                    _ => unreachable!("should never reach here"),
                })
                .collect::<String>()
                .parse()
                .unwrap(),
        );

        PredicateLetter(predicate_letter_inner, superscript_number)
    }

    fn predicate_into_ast(
        &self,
        pair: Pair<Rule>,
        mut stack: &mut Vec<Variable>,
    ) -> Result<Predicate, Error> {
        assert!(pair.as_rule() == Rule::predicate);

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::compound_predicate => self.compound_predicate_into_ast(inner, &mut stack),
            Rule::simple_predicate => self.simple_predicate_into_ast(inner, &mut stack),
            _ => unreachable!("should never reach here"),
        }
    }

    fn compound_predicate_into_ast(
        &self,
        pair: Pair<Rule>,
        mut stack: &mut Vec<Variable>,
    ) -> Result<Predicate, Error> {
        assert!(pair.as_rule() == Rule::compound_predicate);

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::conjunctive_predicate => self.conjunctive_predicate_into_ast(inner, &mut stack),
            Rule::negative_predicate => self.negative_predicate_into_ast(inner, &mut stack),
            Rule::disjunctive_predicate => self.disjunctive_predicate_into_ast(inner, &mut stack),
            Rule::conditional_predicate => self.conditional_predicate_into_ast(inner, &mut stack),
            _ => unreachable!("should never reach here"),
        }
    }

    fn conjunctive_predicate_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &mut Vec<Variable>,
    ) -> Result<Predicate, Error> {
        assert!(pair.as_rule() == Rule::conjunctive_predicate);

        let mut inner = pair.into_inner();

        let lpredicate = self.predicate_into_ast(inner.next().unwrap(), &mut stack.clone())?;
        let rpredicate = self.predicate_into_ast(inner.next().unwrap(), &mut stack.clone())?;

        Ok(Predicate::Conjunctive(
            Box::new(lpredicate),
            Box::new(rpredicate),
        ))
    }

    fn negative_predicate_into_ast(
        &self,
        pair: Pair<Rule>,
        mut stack: &mut Vec<Variable>,
    ) -> Result<Predicate, Error> {
        assert!(pair.as_rule() == Rule::negative_predicate);

        let mut inner = pair.into_inner();

        let rpredicate = self.predicate_into_ast(inner.next().unwrap(), &mut stack)?;

        Ok(Predicate::Negative(Box::new(rpredicate)))
    }

    fn disjunctive_predicate_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &mut Vec<Variable>,
    ) -> Result<Predicate, Error> {
        assert!(pair.as_rule() == Rule::disjunctive_predicate);

        let mut inner = pair.into_inner();

        let lpredicate = self.predicate_into_ast(inner.next().unwrap(), &mut stack.clone())?;
        let rpredicate = self.predicate_into_ast(inner.next().unwrap(), &mut stack.clone())?;

        Ok(Predicate::Disjunctive(
            Box::new(lpredicate),
            Box::new(rpredicate),
        ))
    }

    fn conditional_predicate_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &mut Vec<Variable>,
    ) -> Result<Predicate, Error> {
        assert!(pair.as_rule() == Rule::conditional_predicate);

        let mut inner = pair.into_inner();

        let lpredicate = self.predicate_into_ast(inner.next().unwrap(), &mut stack.clone())?;
        let rpredicate = self.predicate_into_ast(inner.next().unwrap(), &mut stack.clone())?;

        Ok(Predicate::Conditional(
            Box::new(lpredicate),
            Box::new(rpredicate),
        ))
    }

    fn simple_predicate_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &mut Vec<Variable>,
    ) -> Result<Predicate, Error> {
        assert!(pair.as_rule() == Rule::simple_predicate);

        let mut inner = pair.clone().into_inner();

        let predicate_letter = self.predicate_letter_into_ast(inner.next().unwrap());

        let terms = inner
            .map(|x| match x.as_rule() {
                Rule::singular_term => {
                    Term::SingularTerm(SingularTerm(x.as_str().chars().next().unwrap()))
                }
                Rule::variable => Term::Variable(Variable(x.as_str().chars().next().unwrap())),
                _ => unreachable!("should never reach here"),
            })
            .collect::<Vec<Term>>();

        if predicate_letter.1 != terms.len() {
            return Err(Error::new_from_custom_error(
                pair.as_span(),
                "degree doesn't match number of terms specified",
            ));
        }

        if !terms.iter().all(|x| match x {
            Term::Variable(var) => stack.contains(var),
            _ => true,
        }) {
            return Err(Error::new_from_custom_error(
                pair.as_span(),
                "predicate binds to variable that isn't in scope",
            ));
        }

        Ok(Predicate::Simple(predicate_letter, terms))
    }

    fn argument_into_ast(&self, pair: Pair<Rule>) -> Result<ParseTree, Error> {
        assert!(pair.as_rule() == Rule::argument);

        let mut statements = Vec::new();

        for st_pair in pair.into_inner() {
            match st_pair.as_rule() {
                Rule::premise | Rule::conclusion => {
                    match self.statement_into_ast(st_pair.into_inner().next().unwrap()) {
                        Ok(st) => statements.push(st),
                        Err(e) => return Err(e),
                    }
                }
                _ => unreachable!("should never reach here"),
            }
        }

        // The grammar guarantees us that the conclusion comes last.
        // That means that the conclusion will be at the back of the vector
        let conclusion = statements.pop().unwrap();

        Ok(ParseTree::Argument(statements, conclusion))
    }
}

/*#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn 
}*/