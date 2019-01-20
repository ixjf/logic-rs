#![allow(dead_code)]

use crate::parser::*;

use rctree::Node;

// Separate truth tree method logic from Node's? i.e. functions return Option<Statement>,
// not Option<Node<Statement>>
pub struct TruthTreeMethod {
    tree: Node<Statement>,
}

// THIS DOES NOT VALIDATE INPUT
impl<'a> TruthTreeMethod {
    pub fn new(statements: &Vec<Statement>) -> Self {
        let mut iter = statements.iter();
        let root = Node::new(iter.next().unwrap().clone());

        iter.cloned().fold(root.clone(), |mut parent, x| {
            let child = Node::new(x);
            parent.append(child.clone());
            child
        });

        TruthTreeMethod { tree: root }
    }

    pub fn compute(&self) {
        let result_qe = self
            .tree
            .descendants()
            .map(|x| self.apply_qe_rule(x))
            .filter(|x| x.is_some())
            .map(|x| x.unwrap());

        for new_node in result_qe {
            self.find_end_of_branch(self.tree.clone()).append(new_node);
        }

        // TEST
        let mut singular_terms_stack =
            self.build_singular_term_stack(self.find_end_of_branch(self.tree.clone()));

        let result_eq = self
            .tree
            .descendants()
            .map(|x| self.apply_eq_rule(x, &mut singular_terms_stack))
            .filter(|x| x.is_some())
            .map(|x| x.unwrap());

        for new_node in result_eq {
            self.find_end_of_branch(self.tree.clone()).append(new_node);
        }
    }

    fn find_end_of_branch(&self, node: Node<Statement>) -> Node<Statement> {
        match node.children().count() {
            c if c > 1 => panic!("branch diverges"),
            0 => node,
            1 => self.find_end_of_branch(node.children().next().unwrap()),
            _ => unreachable!(),
        }
    }

    fn apply_qe_rule(&self, node: Node<Statement>) -> Option<Node<Statement>> {
        // TODO: The new node has to store the rule that was applied
        // and it has to mark the original node as being done
        match *node.borrow() {
            Statement::LogicalNegation(ref inner) => match **inner {
                Statement::Existential(ref var, ref pred) => Some(Node::new(Statement::Universal(
                    var.clone(),
                    Predicate::Negative(Box::new(pred.clone())),
                ))),
                Statement::Universal(ref var, ref pred) => Some(Node::new(Statement::Existential(
                    var.clone(),
                    Predicate::Negative(Box::new(pred.clone())),
                ))),
                _ => None,
            },
            _ => None,
        }
    }

    fn apply_eq_rule(
        &self,
        node: Node<Statement>,
        stack: &mut Vec<SingularTerm>,
    ) -> Option<Node<Statement>> {
        match *node.borrow() {
            // TODO: No support for nested quantifiers yet
            Statement::Existential(ref var, ref pred) => {
                fn instantiate_predicate(
                    pred: &Predicate,
                    var: &Variable,
                    replace_with: &SingularTerm,
                ) -> Statement {
                    match pred {
                        Predicate::Simple(ref pred_letter, ref terms) => {
                            let singular_terms = terms
                                .iter()
                                .map(|x| match x {
                                    Term::Variable(ref v) => {
                                        if v != var {
                                            panic!("unknown variable in existential quantifier (nested quantifiers are not yet supported)");
                                        }
                                        Term::SingularTerm(replace_with.clone())
                                    }
                                    t @ Term::SingularTerm(_) => t.clone(),
                                })
                                .map(|x| match x {
                                    Term::SingularTerm(t) => t,
                                    _ => panic!(),
                                })
                                .collect::<Vec<SingularTerm>>();

                            Statement::Singular(pred_letter.clone(), singular_terms)
                        }
                        Predicate::Conjunctive(ref lpred, ref rpred) => {
                            Statement::LogicalConjunction(
                                Box::new(instantiate_predicate(lpred, var, replace_with)),
                                Box::new(instantiate_predicate(rpred, var, replace_with)),
                            )
                        }
                        Predicate::Negative(ref rpred) => Statement::LogicalNegation(Box::new(
                            instantiate_predicate(rpred, var, replace_with),
                        )),
                        Predicate::Disjunctive(ref lpred, ref rpred) => {
                            Statement::LogicalDisjunction(
                                Box::new(instantiate_predicate(lpred, var, replace_with)),
                                Box::new(instantiate_predicate(rpred, var, replace_with)),
                            )
                        }
                        Predicate::Conditional(ref lpred, ref rpred) => {
                            Statement::LogicalConditional(
                                Box::new(instantiate_predicate(lpred, var, replace_with)),
                                Box::new(instantiate_predicate(rpred, var, replace_with)),
                            )
                        }
                    }
                };

                let singular_term = self.new_unused_in_singular_term_stack(&stack);

                stack.push(singular_term.clone());

                Some(Node::new(instantiate_predicate(pred, var, &singular_term)))
            }
            _ => None,
        }
    }

    fn apply_uq_rule(
        &self,
        node: Node<Statement>,
        stack: &Vec<SingularTerm>,
    ) -> Option<Vec<Node<Statement>>> {
        // TODO: Basically, do the same thing as in EQ rule, except instead of instantiating
        // these to a new singular term, we instantiate them with EVERY term on the stack
        // Meaning we return a Vec of Nodes, not just one
        None
    }

    fn new_unused_in_singular_term_stack(&self, stack: &Vec<SingularTerm>) -> SingularTerm {
        for c in 'a' as u8..='w' as u8 {
            if !stack.iter().any(|x| x.0 == c as char) {
                return SingularTerm(c as char, Subscript(None));
            }
        }

        for subscript in 1.. {
            match stack
                .iter()
                .find_map(|x| if x.1 != subscript { Some(x) } else { None })
            {
                Some(s) => return SingularTerm(s.0, Subscript(Some(subscript))),
                None => {}
            }
        }

        unreachable!()
    }

    fn build_singular_term_stack(&self, branch: Node<Statement>) -> Vec<SingularTerm> {
        let mut stack = Vec::new();

        fn find_in_statement(mut stack: &mut Vec<SingularTerm>, x: &Statement) {
            match x {
                Statement::Singular(_, terms) => terms.iter().for_each(|t| {
                    if !stack.contains(t) {
                        stack.push(t.clone());
                    }
                }),
                Statement::LogicalConjunction(lst, rst)
                | Statement::LogicalDisjunction(lst, rst)
                | Statement::LogicalConditional(lst, rst) => {
                    find_in_statement(&mut stack, lst);
                    find_in_statement(&mut stack, rst);
                }
                Statement::LogicalNegation(rst) => {
                    find_in_statement(&mut stack, rst);
                }
                Statement::Existential(_, pred) => {
                    find_in_predicate(&mut stack, pred);
                }
                _ => {}
            };
        }

        fn find_in_predicate(mut stack: &mut Vec<SingularTerm>, x: &Predicate) {
            match x {
                Predicate::Simple(_, terms) => terms.iter().for_each(|t| match t {
                    Term::SingularTerm(singular_term) => {
                        if !stack.contains(singular_term) {
                            stack.push(singular_term.clone());
                        }
                    }
                    _ => {}
                }),
                Predicate::Conjunctive(lpred, rpred)
                | Predicate::Disjunctive(lpred, rpred)
                | Predicate::Conditional(lpred, rpred) => {
                    find_in_predicate(&mut stack, lpred);
                    find_in_predicate(&mut stack, rpred);
                }
                Predicate::Negative(rpred) => {
                    find_in_predicate(&mut stack, rpred);
                }
            };
        }

        for ancestor in branch.ancestors() {
            find_in_statement(&mut stack, &*ancestor.borrow());
        }

        stack
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn applies_qe_rule() {
        let mut statements = Vec::new();
        statements.push(Statement::LogicalNegation(Box::new(Statement::Universal(
            Variable('y', Subscript(None)),
            Predicate::Simple(PredicateLetter('F', Subscript(None), Degree(1)), Vec::new()),
        ))));
        statements.push(Statement::LogicalNegation(Box::new(
            Statement::Existential(
                Variable('x', Subscript(None)),
                Predicate::Simple(PredicateLetter('C', Subscript(None), Degree(1)), Vec::new()),
            ),
        )));

        let truth_tree_method = TruthTreeMethod::new(&statements);

        truth_tree_method.compute();

        for node in truth_tree_method.tree.descendants() {
            //println!("{:#?}", node);
        }

        // TODO
    }

    #[test]
    fn applies_eq_rule() {
        let mut statements = Vec::new();
        statements.push(Statement::Existential(
            Variable('x', Subscript(None)),
            Predicate::Simple(
                PredicateLetter('A', Subscript(None), Degree(1)),
                vec![Term::Variable(Variable('x', Subscript(None)))],
            ),
        ));
        statements.push(Statement::Existential(
            Variable('x', Subscript(None)),
            Predicate::Conjunctive(
                Box::new(Predicate::Simple(
                    PredicateLetter('A', Subscript(None), Degree(1)),
                    vec![Term::Variable(Variable('x', Subscript(None)))],
                )),
                Box::new(Predicate::Simple(
                    PredicateLetter('B', Subscript(None), Degree(2)),
                    vec![
                        Term::Variable(Variable('x', Subscript(None))),
                        Term::SingularTerm(SingularTerm('c', Subscript(None))),
                    ],
                )),
            ),
        ));
        statements.push(Statement::Existential(
            Variable('x', Subscript(None)),
            Predicate::Negative(Box::new(Predicate::Simple(
                PredicateLetter('A', Subscript(None), Degree(1)),
                vec![Term::Variable(Variable('x', Subscript(None)))],
            ))),
        ));
        statements.push(Statement::Existential(
            Variable('z', Subscript(None)),
            Predicate::Disjunctive(
                Box::new(Predicate::Simple(
                    PredicateLetter('D', Subscript(None), Degree(1)),
                    vec![Term::Variable(Variable('z', Subscript(None)))],
                )),
                Box::new(Predicate::Simple(
                    PredicateLetter('F', Subscript(None), Degree(2)),
                    vec![
                        Term::Variable(Variable('z', Subscript(None))),
                        Term::Variable(Variable('z', Subscript(None))),
                    ],
                )),
            ),
        ));
        statements.push(Statement::Existential(
            Variable('x', Subscript(None)),
            Predicate::Conditional(
                Box::new(Predicate::Simple(
                    PredicateLetter('Z', Subscript(None), Degree(1)),
                    vec![Term::Variable(Variable('x', Subscript(None)))],
                )),
                Box::new(Predicate::Simple(
                    PredicateLetter('Y', Subscript(None), Degree(2)),
                    vec![
                        Term::Variable(Variable('x', Subscript(None))),
                        Term::SingularTerm(SingularTerm('f', Subscript(None))),
                    ],
                )),
            ),
        ));

        let truth_tree_method = TruthTreeMethod::new(&statements);

        truth_tree_method.compute();

        for node in truth_tree_method.tree.descendants() {
            println!("{:#?}", node);
        }

        // TODO
    }

    #[test]
    fn build_singular_term_stack() {
        let mut statements = Vec::new();
        statements.push(Statement::Singular(
            PredicateLetter('A', Subscript(None), Degree(1)),
            vec![SingularTerm('a', Subscript(None))],
        ));
        statements.push(Statement::Existential(
            Variable('y', Subscript(None)),
            Predicate::Simple(
                PredicateLetter('B', Subscript(None), Degree(2)),
                vec![
                    Term::Variable(Variable('y', Subscript(None))),
                    Term::SingularTerm(SingularTerm('b', Subscript(None))),
                ],
            ),
        ));

        let truth_tree_method = TruthTreeMethod::new(&statements);

        for term in truth_tree_method
            .build_singular_term_stack(truth_tree_method.tree.last_child().unwrap())
        {
            println!("{:#?}", term);
        }

        // TODO
    }
}
