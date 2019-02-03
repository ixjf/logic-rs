#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Subscript(pub Option<u64>);

impl PartialEq<u64> for Subscript {
    fn eq(&self, rhs: &u64) -> bool {
        match self.0 {
            Some(ref lhs) => lhs == rhs,
            None => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SimpleStatementLetter(pub char, pub Subscript);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SingularTerm(pub char, pub Subscript);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Variable(pub char, pub Subscript);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Degree(pub u64);

impl PartialEq<u64> for Degree {
    fn eq(&self, rhs: &u64) -> bool {
        self.0 == *rhs
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PredicateLetter(pub char, pub Subscript, pub Degree);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Input {
    StatementSet(Vec<Statement>),
    Argument(Vec<Statement>, Statement),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseTree(pub Input);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    SingularTerm(SingularTerm),
    Variable(Variable),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Simple(SimpleStatementLetter),
    Singular(PredicateLetter, Vec<SingularTerm>),
    LogicalConjunction(Box<Statement>, Box<Statement>),
    LogicalNegation(Box<Statement>),
    LogicalDisjunction(Box<Statement>, Box<Statement>),
    LogicalConditional(Box<Statement>, Box<Statement>),
    Existential(Variable, Box<Formula>),
    Universal(Variable, Box<Formula>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Formula {
    Statement(Box<Statement>),
    Predicate(PredicateLetter, Vec<Term>),
    Conjunction(Box<Formula>, Box<Formula>),
    Negation(Box<Formula>),
    Disjunction(Box<Formula>, Box<Formula>),
    Conditional(Box<Formula>, Box<Formula>),
}