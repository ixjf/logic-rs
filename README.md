# logic-rs
[![Build Status](https://travis-ci.org/ixjf/logic-rs.svg?branch=master)](https://travis-ci.org/ixjf/logic-rs)
[![codecov](https://codecov.io/gh/ixjf/logic-rs/branch/master/graph/badge.svg)](https://codecov.io/gh/ixjf/logic-rs)

A parser of relational predicate logic and truth tree solver.

logic_rs is heavily influenced by the book _Meaning and Argument:
An Introduction to Logic Through Language_, by Ernest Lepore and Sam Cumming, trying
to follow as closely as possible its grammar and rules.

It uses separate syntax for statement sets, arguments, and sole statements, and so
can automatically generate and analyse truth trees accordingly.

The library powers its website, which you can find [here](https://ixjf.github.io/logic-rs/),
serving only that purpose, but it is not tied to it at all, and can be used
completely independently.

**Note: logic_rs _currently_ doesn't support identity statements.**

# Usage
Validating some formula is as simple as:

```rust
match parse_input("(∀x)(B¹x ⊃ (L²xm ⊃ L²bx))") {
    Ok(input_kind) => {
        // Input is a well-formed formula
    },
    Err(parse_err) => {
        // Input is **not** a well-formed formula
    }
}
```

And proving that the input above is a sole statement and that that statement is a contingency
is just as simple:
```rust
match parse_input("(∀x)(B¹x ⊃ (L²xm ⊃ L²bx))") {
    Ok(input_kind) => match input_kind {
        InputKind::Statement(st) => {
            let (
                is_contingency,
                truth_tree_statement,
                truth_tree_negation_of_stmt
                ) = st.is_contingency();
            
            assert_eq!(is_contingency, true);
        },
        _ => assert!(false)
    },
    Err(parse_err) => assert!(false)
}
```

The same process would be used to test whether the statement is a contradiction or a contingency, as well
as to test the consistency of a statement set, or the formal validity of an argument. 


`truth_tree_statement` and `truth_tree_negation_of_stmt` are the resulting truth trees generated by the algorithm for some initial statement, and for the negation of that statement, respectively. If the truth tree
for the statement closed, it would be proved that the statement is a contradiction, since that would mean it's not possible
for it to be true in any case; if, on the other hand, the truth tree for the negation of the statement closed, it would be
proved that the statement is a tautology, since the negation of a tautology is a contradiction.In this case, the initial statement is a contingency, so if we called the method `is_open` on each
truth tree, we would find that both were open. 

`truth_tree_statement` and `truth_tree_negation_of_stmt` are
`TruthTree`s, ID-based trees containing the entire generated tree and additional information to identify
each derivation.


For details on how to use, see the documentation at [docs.rs](https://docs.rs/logic_rs/0.1.0).

# Language and Truth Tree Algorithm
logic_rs allows an infinite universe of discourse and does not place any other restrictions on the set
of allowed input sets. This means that inputs that can lead to infinite trees _are_ allowed,
and will make the algorithm get stuck in an infinite loop. Enforcing a finite universe of discourse
would solve the problem, but it would lead to the generation of unnecessarily massive truth trees. Ideally,
a solution to this problem would:
1) not generate huge truth trees,
2) maintain the correctness of the classification of the allowed input sets

But it might be that it would necessarily have to involve limiting the set of allowed input sets. Work is still
being made about this.

It is guaranteed, however, _unless there is some bug_, that the algorithm will always correctly classify
_all_ unsatisfiable set of statements. So, if the algorithm _does_ get into an infinite loop, then
it is certain that the initial set of statements is satisfiable.

# Source code

**master** branch - source code for Rust crate logic-rs
**www** branch - source code for website
**gh-pages** branch - production code for website
**wasm-layer** branch - wasm layer for integrating the Rust crate into the website
