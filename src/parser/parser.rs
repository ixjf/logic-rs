use super::ast::{
    Degree, Formula, Input, ParseTree, PredicateLetter, SimpleStatementLetter, SingularTerm,
    Statement, Subscript, Term, Variable,
};
use super::error::ParseError;
use pest::iterators::{Pair, Pairs};

mod pest_parser {
    #[derive(Parser)]
    #[grammar = "parser/grammar/GRAMMAR.pest"]
    pub struct GeneratedParser;
}

use pest_parser::GeneratedParser;
pub use pest_parser::Rule;

pub struct Parser {}

impl Parser {
    pub fn new() -> Self {
        Parser {}
    }

    pub fn parse<'a>(&self, input: &'a str) -> Result<ParseTree, ParseError> {
        use pest::Parser;

        match GeneratedParser::parse(Rule::input, input) {
            Ok(p) => self.into_ast(p),
            Err(e) => Err(ParseError::new_from_parsing_error(e)),
        }
    }

    fn into_ast(&self, mut pairs: Pairs<'_, Rule>) -> Result<ParseTree, ParseError> {
        let inner = pairs.next().unwrap().into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::statement_set => self.statement_set_into_ast(inner),

            Rule::argument => self.argument_into_ast(inner),

            Rule::statement => match self.statement_into_ast(inner, &Vec::new()) {
                Ok(st) => Ok(ParseTree(Input::Statement(st))),
                Err(e) => Err(e),
            },

            _ => unreachable!(),
        }
    }

    fn statement_set_into_ast(&self, pair: Pair<Rule>) -> Result<ParseTree, ParseError> {
        assert!(pair.as_rule() == Rule::statement_set);

        let mut statements = Vec::new();

        for st_pair in pair.into_inner() {
            match self.statement_into_ast(st_pair, &Vec::new()) {
                Ok(st) => statements.push(st),
                Err(e) => return Err(e),
            }
        }

        Ok(ParseTree(Input::StatementSet(statements)))
    }

    fn argument_into_ast(&self, pair: Pair<Rule>) -> Result<ParseTree, ParseError> {
        assert!(pair.as_rule() == Rule::argument);

        let mut statements = Vec::new();

        for st_pair in pair.into_inner() {
            match st_pair.as_rule() {
                Rule::premise | Rule::conclusion => {
                    match self.statement_into_ast(st_pair.into_inner().next().unwrap(), &Vec::new())
                    {
                        Ok(st) => statements.push(st),
                        Err(e) => return Err(e),
                    }
                }
                _ => unreachable!(),
            }
        }

        // The grammar guarantees us that the conclusion comes last.
        // That means that the conclusion will be at the back of the vector
        let conclusion = statements.pop().unwrap();

        Ok(ParseTree(Input::Argument(statements, conclusion)))
    }

    fn statement_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Statement, ParseError> {
        assert!(pair.as_rule() == Rule::statement);

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::complex_statement => self.complex_statement_into_ast(inner, &stack),
            Rule::simple_statement => self.simple_statement_into_ast(inner),
            _ => {
                // Statement inside grouper
                self.statement_into_ast(inner, &stack)
            }
        }
    }

    fn complex_statement_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Statement, ParseError> {
        assert!(pair.as_rule() == Rule::complex_statement);

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::logical_conjunction => self.logical_conjunction_into_ast(inner, &stack),
            Rule::logical_negation => self.logical_negation_into_ast(inner, &stack),
            Rule::logical_disjunction => self.logical_disjunction_into_ast(inner, &stack),
            Rule::logical_conditional => self.logical_conditional_into_ast(inner, &stack),
            Rule::existential_statement => self.existential_statement_into_ast(inner, &stack),
            Rule::universal_statement => self.universal_statement_into_ast(inner, &stack),
            _ => unreachable!(),
        }
    }

    fn logical_conjunction_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Statement, ParseError> {
        assert!(pair.as_rule() == Rule::logical_conjunction);

        let mut inner = pair.into_inner();

        let lstatement = self.statement_into_ast(inner.next().unwrap(), &stack.clone())?;
        let rstatement = self.statement_into_ast(inner.next().unwrap(), &stack.clone())?;

        Ok(Statement::LogicalConjunction(
            Box::new(lstatement),
            Box::new(rstatement),
        ))
    }

    fn logical_negation_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Statement, ParseError> {
        assert!(pair.as_rule() == Rule::logical_negation);

        let mut inner = pair.into_inner();

        let rstatement = self.statement_into_ast(inner.next().unwrap(), &stack)?;

        Ok(Statement::LogicalNegation(Box::new(rstatement)))
    }

    fn logical_disjunction_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Statement, ParseError> {
        assert!(pair.as_rule() == Rule::logical_disjunction);

        let mut inner = pair.into_inner();

        let lstatement = self.statement_into_ast(inner.next().unwrap(), &stack.clone())?;
        let rstatement = self.statement_into_ast(inner.next().unwrap(), &stack.clone())?;

        Ok(Statement::LogicalDisjunction(
            Box::new(lstatement),
            Box::new(rstatement),
        ))
    }

    fn logical_conditional_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Statement, ParseError> {
        assert!(pair.as_rule() == Rule::logical_conditional);

        let mut inner = pair.into_inner();

        let lstatement = self.statement_into_ast(inner.next().unwrap(), &stack.clone())?;
        let rstatement = self.statement_into_ast(inner.next().unwrap(), &stack.clone())?;

        Ok(Statement::LogicalConditional(
            Box::new(lstatement),
            Box::new(rstatement),
        ))
    }

    fn existential_statement_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Statement, ParseError> {
        assert!(pair.as_rule() == Rule::existential_statement);

        let mut inner = pair.clone().into_inner();

        let variable = self.variable_into_ast(inner.next().unwrap());

        if stack.iter().any(|x| x == &variable) {
            return Err(ParseError::new_from_custom_error(
                pair.as_span(),
                "variable is already bound to another quantifier",
            ));
        }

        let mut stack = stack.clone();
        stack.push(variable.clone());
        let formula = self.formula_into_ast(inner.next().unwrap(), &stack)?;

        Ok(Statement::Existential(variable, Box::new(formula)))
    }

    fn universal_statement_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Statement, ParseError> {
        assert!(pair.as_rule() == Rule::universal_statement);

        let mut inner = pair.clone().into_inner();

        let variable = self.variable_into_ast(inner.next().unwrap());

        if stack.iter().any(|x| x == &variable) {
            return Err(ParseError::new_from_custom_error(
                pair.as_span(),
                "variable is already bound to another quantifier",
            ));
        }

        let mut stack = stack.clone();
        stack.push(variable.clone());
        let formula = self.formula_into_ast(inner.next().unwrap(), &stack)?;

        Ok(Statement::Universal(variable, Box::new(formula)))
    }

    fn simple_statement_into_ast(&self, pair: Pair<Rule>) -> Result<Statement, ParseError> {
        assert!(pair.as_rule() == Rule::simple_statement);

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::singular_statement => self.singular_statement_into_ast(inner),
            Rule::simple_statement_letter => {
                let mut inner_inner = inner.into_inner();
                let letter = inner_inner.next().unwrap().as_str().chars().next().unwrap();

                let subscript = match inner_inner.peek() {
                    Some(_) => self.subscript_into_ast(inner_inner.next().unwrap()),
                    None => Subscript(None),
                };

                Ok(Statement::Simple(SimpleStatementLetter(letter, subscript)))
            }
            _ => unreachable!(),
        }
    }

    fn singular_statement_into_ast(&self, pair: Pair<Rule>) -> Result<Statement, ParseError> {
        assert!(pair.as_rule() == Rule::singular_statement);

        let mut inner = pair.clone().into_inner();

        let predicate_letter = self.predicate_letter_into_ast(inner.next().unwrap());

        let terms = inner
            .map(|x| match x.as_rule() {
                Rule::singular_term => self.singular_term_into_ast(x),
                _ => unreachable!(),
            })
            .collect::<Vec<SingularTerm>>();

        if predicate_letter.2 != terms.len() as u64 {
            return Err(ParseError::new_from_custom_error(
                pair.as_span(),
                "degree doesn't match number of terms specified",
            ));
        }

        Ok(Statement::Singular(predicate_letter, terms))
    }

    fn singular_term_into_ast(&self, pair: Pair<Rule>) -> SingularTerm {
        assert!(pair.as_rule() == Rule::singular_term);

        let mut inner = pair.into_inner();

        let alpha = inner.next().unwrap().as_str().chars().next().unwrap();

        let subscript = match inner.peek() {
            Some(_) => self.subscript_into_ast(inner.next().unwrap()),
            None => Subscript(None),
        };

        SingularTerm(alpha, subscript)
    }

    fn predicate_letter_into_ast(&self, pair: Pair<Rule>) -> PredicateLetter {
        assert!(pair.as_rule() == Rule::predicate_letter);

        let mut inner = pair.into_inner();

        let predicate_letter_alpha = inner.next().unwrap().as_str().chars().next().unwrap();

        let predicate_letter_subscript = match inner.peek().unwrap().as_rule() {
            Rule::subscript_number => self.subscript_into_ast(inner.next().unwrap()),
            _ => Subscript(None),
        };

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
                    _ => unreachable!(),
                })
                .collect::<String>()
                .parse()
                .unwrap(),
        );

        PredicateLetter(
            predicate_letter_alpha,
            predicate_letter_subscript,
            superscript_number,
        )
    }

    fn subscript_into_ast(&self, pair: Pair<Rule>) -> Subscript {
        assert!(pair.as_rule() == Rule::subscript_number);

        Subscript(Some(
            pair.as_str()
                .chars()
                .map(|x| match x {
                    '\u{2080}' => '0',
                    '\u{2081}' => '1',
                    '\u{2082}' => '2',
                    '\u{2083}' => '3',
                    '\u{2084}' => '4',
                    '\u{2085}' => '5',
                    '\u{2086}' => '6',
                    '\u{2087}' => '7',
                    '\u{2088}' => '8',
                    '\u{2089}' => '9',
                    _ => unreachable!(),
                })
                .collect::<String>()
                .parse::<u64>()
                .unwrap(),
        ))
    }

    fn formula_into_ast(&self, pair: Pair<Rule>, stack: &Vec<Variable>) -> Result<Formula, ParseError> {
        assert!(pair.as_rule() == Rule::formula);

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::compound_formula => self.compound_formula_into_ast(inner, &stack),
            Rule::atomic_formula => self.atomic_formula_into_ast(inner, &stack),
            _ => {
                // Formula inside grouper
                self.formula_into_ast(inner, &stack)
            }
        }
    }

    fn compound_formula_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Formula, ParseError> {
        assert!(pair.as_rule() == Rule::compound_formula);

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::complex_statement => self
                .complex_statement_into_ast(inner, &stack)
                .map(|x| Formula::Statement(Box::new(x))),
            Rule::compound_formula_conjunction => {
                self.compound_formula_conjunction_into_ast(inner, &stack)
            }
            Rule::compound_formula_negation => {
                self.compound_formula_negation_into_ast(inner, &stack)
            }
            Rule::compound_formula_disjunction => {
                self.compound_formula_disjunction_into_ast(inner, &stack)
            }
            Rule::compound_formula_conditional => {
                self.compound_formula_conditional_into_ast(inner, &stack)
            }
            _ => unreachable!(),
        }
    }

    fn compound_formula_conjunction_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Formula, ParseError> {
        assert!(pair.as_rule() == Rule::compound_formula_conjunction);

        let mut inner = pair.into_inner();

        let lformula = self.formula_into_ast(inner.next().unwrap(), &stack.clone())?;
        let rformula = self.formula_into_ast(inner.next().unwrap(), &stack.clone())?;

        Ok(Formula::Conjunction(Box::new(lformula), Box::new(rformula)))
    }

    fn compound_formula_negation_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Formula, ParseError> {
        assert!(pair.as_rule() == Rule::compound_formula_negation);

        let mut inner = pair.into_inner();

        let rformula = self.formula_into_ast(inner.next().unwrap(), &stack)?;

        Ok(Formula::Negation(Box::new(rformula)))
    }

    fn compound_formula_disjunction_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Formula, ParseError> {
        assert!(pair.as_rule() == Rule::compound_formula_disjunction);

        let mut inner = pair.into_inner();

        let lformula = self.formula_into_ast(inner.next().unwrap(), &stack.clone())?;
        let rformula = self.formula_into_ast(inner.next().unwrap(), &stack.clone())?;

        Ok(Formula::Disjunction(Box::new(lformula), Box::new(rformula)))
    }

    fn compound_formula_conditional_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Formula, ParseError> {
        assert!(pair.as_rule() == Rule::compound_formula_conditional);

        let mut inner = pair.into_inner();

        let lformula = self.formula_into_ast(inner.next().unwrap(), &stack.clone())?;
        let rformula = self.formula_into_ast(inner.next().unwrap(), &stack.clone())?;

        Ok(Formula::Conditional(Box::new(lformula), Box::new(rformula)))
    }

    fn atomic_formula_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<Formula, ParseError> {
        assert!(pair.as_rule() == Rule::atomic_formula);

        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::simple_statement => self
                .simple_statement_into_ast(inner)
                .map(|x| Formula::Statement(Box::new(x))),
            Rule::simple_predicate => self
                .simple_predicate_into_ast(inner, &stack)
                .map(|(pred_letter, terms)| Formula::Predicate(pred_letter, terms)),
            _ => unreachable!(),
        }
    }

    fn simple_predicate_into_ast(
        &self,
        pair: Pair<Rule>,
        stack: &Vec<Variable>,
    ) -> Result<(PredicateLetter, Vec<Term>), ParseError> {
        assert!(pair.as_rule() == Rule::simple_predicate);

        let mut inner = pair.clone().into_inner();

        let predicate_letter = self.predicate_letter_into_ast(inner.next().unwrap());

        let terms = inner
            .map(|x| match x.as_rule() {
                Rule::singular_term => Term::SingularTerm(self.singular_term_into_ast(x)),
                Rule::variable => Term::Variable(self.variable_into_ast(x)),
                _ => unreachable!(),
            })
            .collect::<Vec<Term>>();

        if predicate_letter.2 != terms.len() as u64 {
            return Err(ParseError::new_from_custom_error(
                pair.as_span(),
                "degree doesn't match number of terms specified",
            ));
        }

        if !terms.iter().all(|x| match x {
            Term::Variable(var) => stack.contains(var),
            _ => true,
        }) {
            return Err(ParseError::new_from_custom_error(
                pair.as_span(),
                "predicate binds to variable that isn't in scope",
            ));
        }

        Ok((predicate_letter, terms))
    }

    fn variable_into_ast(&self, pair: Pair<Rule>) -> Variable {
        assert!(pair.as_rule() == Rule::variable);

        let mut inner = pair.into_inner();

        let alpha = inner.next().unwrap().as_str().chars().next().unwrap();

        let subscript = match inner.peek() {
            Some(_) => self.subscript_into_ast(inner.next().unwrap()),
            None => Subscript(None),
        };

        Variable(alpha, subscript)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Span;

    #[test]
    fn custom_error_provides_correct_location_info() {
        let e =
            ParseError::new_from_custom_error(Span::new("Hello world!", 0, 4).unwrap(), "missing comma");
        assert!(e.location.0 == 1 && e.location.1 == 1);
    }

    #[test]
    fn parses_statement_set() {
        let parser = Parser::new();

        match parser.parse("{A, B, C, D, F, G}") {
            Ok(ref parse_tree) => match parse_tree.0 {
                Input::StatementSet(_) => {}
                _ => assert!(false),
            },
            Err(e) => assert!(false, format!("{}", e)),
        };
    }

    #[test]
    fn parses_argument() {
        let parser = Parser::new();

        match parser.parse("A, B, C, D, F ∴ G") {
            Ok(ref parse_tree) => match parse_tree.0 {
                Input::Argument(_, _) => {}
                _ => assert!(false),
            },
            Err(e) => assert!(false, format!("{}", e)),
        }
    }

    #[test]
    fn parses_single_statement() {
        let parser = Parser::new();

        match parser.parse("A") {
            Ok(ref parse_tree) => match parse_tree.0 {
                Input::Statement(_) => {}
                _ => assert!(false),
            },
            Err(e) => assert!(false, format!("{}", e)),
        }
    }

    #[test]
    fn parses_logical_conjunction() {
        let parser = Parser::new();

        match parser.parse("{(A & B)}") {
            Ok(parse_tree) => match parse_tree.0 {
                Input::StatementSet(mut statements) => {
                    assert!(statements.len() == 1);
                    match statements.pop().unwrap() {
                        Statement::LogicalConjunction(a, b) => match (*a, *b) {
                            (Statement::Simple(_), Statement::Simple(_)) => {}
                            _ => assert!(false),
                        },
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            },
            Err(e) => assert!(false, format!("{}", e)),
        }
    }

    #[test]
    fn parses_logical_negation() {
        let parser = Parser::new();

        match parser.parse("{~A}") {
            Ok(parse_tree) => match parse_tree.0 {
                Input::StatementSet(mut statements) => {
                    assert!(statements.len() == 1);
                    match statements.pop().unwrap() {
                        Statement::LogicalNegation(a) => match *a {
                            Statement::Simple(_) => {}
                            _ => assert!(false),
                        },
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            },
            Err(e) => assert!(false, format!("{}", e)),
        }
    }

    #[test]
    fn parses_logical_disjunction() {
        let parser = Parser::new();

        match parser.parse("{(A ∨ B)}") {
            Ok(parse_tree) => match parse_tree.0 {
                Input::StatementSet(mut statements) => {
                    assert!(statements.len() == 1);
                    match statements.pop().unwrap() {
                        Statement::LogicalDisjunction(a, b) => match (*a, *b) {
                            (Statement::Simple(_), Statement::Simple(_)) => {}
                            _ => assert!(false),
                        },
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            },
            Err(e) => assert!(false, format!("{}", e)),
        }
    }

    #[test]
    fn parses_logical_conditional() {
        let parser = Parser::new();

        match parser.parse("{(A ⊃ B)}") {
            Ok(parse_tree) => match parse_tree.0 {
                Input::StatementSet(mut statements) => {
                    assert!(statements.len() == 1);
                    match statements.pop().unwrap() {
                        Statement::LogicalConditional(a, b) => match (*a, *b) {
                            (Statement::Simple(_), Statement::Simple(_)) => {}
                            _ => assert!(false),
                        },
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            },
            Err(e) => assert!(false, format!("{}", e)),
        }
    }

    #[test]
    fn parses_existential_statement() {
        let parser = Parser::new();

        match parser.parse("{∃z(A¹z & B¹z)}") {
            Ok(parse_tree) => match parse_tree.0 {
                Input::StatementSet(mut statements) => {
                    assert!(statements.len() == 1);
                    match statements.pop().unwrap() {
                        Statement::Existential(a, b) => match (a, *b) {
                            (Variable(_, _), Formula::Conjunction(_, _)) => {}
                            _ => assert!(false),
                        },
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            },
            Err(e) => assert!(false, format!("{}", e)),
        }
    }

    #[test]
    fn understands_that_degree_means_number_of_terms() {
        let parser = Parser::new();

        match parser.parse("{∃zA¹zs}") {
            Ok(_) => assert!(false),
            _ => {}
        }
    }

    #[test]
    fn keeps_track_of_variable_stack() {
        let parser = Parser::new();

        match parser.parse("{∃zA¹y}") {
            Ok(_) => assert!(false),
            _ => {}
        }
    }

    #[test]
    fn parses_universal_statement() {
        let parser = Parser::new();

        match parser.parse("{∀z(A¹z & B¹z)}") {
            Ok(parse_tree) => match parse_tree.0 {
                Input::StatementSet(mut statements) => {
                    assert!(statements.len() == 1);
                    match statements.pop().unwrap() {
                        Statement::Universal(a, b) => match (a, *b) {
                            (Variable(_, _), Formula::Conjunction(_, _)) => {}
                            _ => assert!(false),
                        },
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            },
            Err(e) => assert!(false, format!("{}", e)),
        }
    }

    #[test]
    fn parses_simple_statement_with_subscript() {
        let parser = Parser::new();

        match parser.parse("{A₂}") {
            Ok(parse_tree) => match parse_tree.0 {
                Input::StatementSet(mut statements) => {
                    assert!(statements.len() == 1);
                    match statements.pop().unwrap() {
                        Statement::Simple(st_letter) => {
                            assert!(st_letter.0 == 'A');
                            assert!(st_letter.1 == Subscript(Some(2)));
                        }
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            },
            Err(e) => assert!(false, format!("{}", e)),
        }
    }

    #[test]
    fn parses_singular_statement() {
        let parser = Parser::new();

        match parser.parse("{A₂¹b}") {
            Ok(parse_tree) => match parse_tree.0 {
                Input::StatementSet(mut statements) => {
                    assert!(statements.len() == 1);
                    match statements.pop().unwrap() {
                        Statement::Singular(predicate_letter, mut terms) => {
                            assert!(predicate_letter.0 == 'A');
                            assert!(predicate_letter.1 == 2);
                            assert!(predicate_letter.2 == 1);
                            assert!(terms.len() == 1);
                            assert!(terms.pop().unwrap() == SingularTerm('b', Subscript(None)));
                        }
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            },
            Err(e) => assert!(false, format!("{}", e)),
        }
    }

    #[test]
    fn singular_statement_doesnt_allow_variables() {
        let parser = Parser::new();

        match parser.parse("{A₂¹x}") {
            Ok(_) => assert!(false),
            _ => {}
        }
    }

    #[test]
    fn predicates_with_same_letter_and_different_degree_are_allowed() {
        let parser = Parser::new();

        match parser.parse("{∀z(A¹z & A²za)}") {
            Ok(_) => {}
            Err(e) => assert!(false, format!("{}", e)),
        }
    }

    #[test]
    fn parses_multiply_general_statements() {
        let parser = Parser::new();

        match parser.parse("{∀x((A¹x & B¹x) ⊃ ∀y(~C¹y ⊃ ∃z(A²zy & B¹z)))}") {
            Ok(_) => {}
            Err(e) => assert!(false, format!("{}", e)),
        }
    }

    #[test]
    fn cannot_bind_bound_variable_to_nested_quantifier() {
        let parser = Parser::new();

        match parser.parse("{∃x(A¹x & ∀x((A²xx & B¹x))}") {
            Ok(_) => assert!(false),
            Err(_) => {}
        }
    }

    #[test]
    fn understands_scope_of_variables() {
        let parser = Parser::new();

        match parser.parse("{(∀x(D¹x ⊃ P¹x) ⊃ ∀x(S¹x ⊃ P¹x))}") {
            Ok(_) => {}
            Err(_) => assert!(false),
        }
    }
}
