/// An optional positive integer that is part of the identifier for a simple
/// statement letter, simple predicate letter, or term.
/// 
/// **Serialization of this struct requires the feature `serde_support` to be enabled.**
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
pub struct Subscript(pub Option<u64>);

impl PartialEq<u64> for Subscript {
    fn eq(&self, rhs: &u64) -> bool {
        match self.0 {
            Some(ref lhs) => lhs == rhs,
            None => false,
        }
    }
}

/// The identifier of a simple statement.
/// 
/// **Serialization of this struct requires the feature `serde_support` to be enabled.**
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
pub struct SimpleStatementLetter(pub char, pub Subscript);

/// The identifier of a singular term.
/// 
/// **Serialization of this struct requires the feature `serde_support` to be enabled.**
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
pub struct SingularTerm(pub char, pub Subscript);

/// The identifier of a variable.
/// 
/// **Serialization of this struct requires the feature `serde_support` to be enabled.**
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
pub struct Variable(pub char, pub Subscript);

/// A positive integer that denotes the degree (the arity or number of terms
/// attached) of a simple predicate.
/// 
/// **Serialization of this struct requires the feature `serde_support` to be enabled.**
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
pub struct Degree(pub u64);

impl PartialEq<u64> for Degree {
    fn eq(&self, rhs: &u64) -> bool {
        self.0 == *rhs
    }
}

/// The identifier of a simple predicate.
/// 
/// **Serialization of this struct requires the feature `serde_support` to be enabled.**
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
pub struct PredicateLetter(pub char, pub Subscript, pub Degree);

/// A term.
/// 
/// **Serialization of this enum requires the feature `serde_support` to be enabled.**
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
pub enum Term {
    SingularTerm(SingularTerm),
    Variable(Variable),
}

/// A statement.
/// 
/// **Serialization of this enum requires the feature `serde_support` to be enabled.**
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
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

/// A formula. It can be any statement, predicate, or a compound formula made of either of these.
/// 
/// **Serialization of this enum requires the feature `serde_support` to be enabled.**
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
pub enum Formula {
    Statement(Box<Statement>),
    Predicate(PredicateLetter, Vec<Term>),
    Conjunction(Box<Formula>, Box<Formula>),
    Negation(Box<Formula>),
    Disjunction(Box<Formula>, Box<Formula>),
    Conditional(Box<Formula>, Box<Formula>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Input {
    StatementSet(Vec<Statement>),
    Argument(Vec<Statement>, Statement),
    Statement(Statement),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseTree(pub Input);