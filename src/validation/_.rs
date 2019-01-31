#![allow(dead_code)]

use super::statement_tree::*;
use crate::parser::*;

#[derive(Clone)]
struct StatementData {
    pub is_processed: bool,
    pub applied_to: Vec<(SingularTerm, Id)>,
    pub derived_from: Option<((Id, Id), String)>, // ((StatementId, BranchId), RuleName)
    pub statement: Statement,
}

enum RuleApplyResultKind {
    AddNewBranches,
    MergeIntoExisting,
}

struct RuleApplyResult {
    pub kind: RuleApplyResultKind,
    pub is_processed: bool,
    pub rule_name: String,
    pub statements: Vec<Statement>,
}

pub struct TruthTreeMethod {
    tree: StatementTree<StatementData>,
}

// THIS DOES NOT VALIDATE INPUT
impl<'a> TruthTreeMethod {
    pub fn new(statements: &Vec<Statement>) -> Self {
        assert!(statements.len() > 0);

        TruthTreeMethod {
            tree: StatementTree::new(Branch::new(
                statements
                    .iter()
                    .map(|x| StatementData {
                        is_processed: false,
                        applied_to: Vec::new(),
                        derived_from: None,
                        statement: x.clone(),
                    })
                    .collect(),
            )),
        }
    }

    pub fn compute(&mut self) {
        let mut stack = self.build_singular_term_stack();

        let main_trunk = self.tree.main_trunk();

        self.compute_branch(&main_trunk, &mut stack);
    }

    fn compute_branch(&mut self, branch_id: &Id, mut stack: &mut Vec<SingularTerm>) {
        // Priority is important, can't have a parent branch changing after
        // a child branch has been created (i.e. new rules must always go to the end of the tree)
        // Hence, branching rules must ALWAYS be applied last
        let rules: Vec<Box<Fn(&Statement, &mut Vec<SingularTerm>) -> Option<RuleApplyResult>>> = vec![
            // can this be self.apply_*_rule?
            Box::new(|statement, _| self.apply_qe_rule(&statement)),
            Box::new(|statement, mut singular_terms_stack| {
                self.apply_eq_rule(&statement, &mut singular_terms_stack)
            }),
            Box::new(|statement, mut singular_terms_stack| {
                self.apply_uq_rule(&statement, &mut singular_terms_stack)
            }),
            Box::new(|statement, _| self.apply_conjunction_rule(&statement)),
            Box::new(|statement, _| self.apply_double_negation_rule(&statement)),
            Box::new(|statement, _| self.apply_negation_of_disjunction_rule(&statement)),
            Box::new(|statement, _| self.apply_negation_of_conditional_rule(&statement)),
            Box::new(|statement, _| self.apply_disjunction_rule(&statement)),
            Box::new(|statement, _| self.apply_conditional_rule(&statement)),
            Box::new(|statement, _| self.apply_negation_of_conjunction_rule(&statement)),
        ];

        let mut branch_closed = false;

        'outer: loop {
            let mut new_children_queue = Vec::new();

            for rule_fn in &rules {
                for ancestor_branch_id in self
                    .tree
                    .reverse_branch_id_iter(&branch_id)
                    .collect::<Vec<_>>()
                    .iter()
                {
                    let ancestor_branch = self.tree.branch_from_id_mut(&ancestor_branch_id);

                    for statement_data_id in ancestor_branch.statement_id_iter() {
                        let mut statement_data =
                            ancestor_branch.statement_from_id_mut(&statement_data_id);

                        if !statement_data.is_processed {
                            continue;
                        }

                        if self.is_atomic_formula(&statement_data.statement)
                            && self.compare_upwards_is_contradiction(
                                &ancestor_branch_id,
                                &statement_data,
                            )
                        {
                            branch_closed = true;
                            break 'outer;
                        }

                        let mut statement_data = self
                            .tree
                            .branch_from_id_mut(&branch_id)
                            .statement_from_id_mut(&statement_data_id);

                        match rule_fn(&statement_data.statement, &mut stack) {
                            Some(result) => {
                                statement_data.is_processed = result.is_processed;

                                let statement_data_vec = result
                                    .statements
                                    .iter()
                                    .map(|x| StatementData {
                                        is_processed: false,
                                        applied_to: Vec::new(),
                                        derived_from: Some((
                                            (statement_data_id.clone(), branch_id.clone()),
                                            result.rule_name,
                                        )),
                                        statement: x.clone(),
                                    })
                                    .collect();

                                match result.kind {
                                    RuleApplyResultKind::AddNewBranches => {
                                        for new_statement_data in statement_data_vec.iter() {
                                            let new_branch = Branch::new(vec![new_statement_data]);

                                            self.compute_branch(
                                                &self.tree.append_branch(new_branch, &branch_id),
                                                &mut stack.clone(),
                                            );
                                            // See a.txt first
                                            // The call above will apply all remaining rules on this new branch
                                            // So other rules won't be applied to the current branch anymore
                                            // In this case, only branching rules remain, so these are the ones
                                            // I'm talking about
                                        }
                                    }
                                    RuleApplyResultKind::MergeIntoExisting => {
                                        new_children_queue.append(statement_data_vec);
                                    }
                                }
                            }
                            None => {}
                        }
                    }
                }

                /*for mut ancestor in branch.ancestors() {
                    for statement_data in &mut (*ancestor.borrow_mut())
                        .children
                        .iter_mut()
                        .filter(|x| !x.is_processed)
                    {
                        if Self::is_atomic_formula(&statement_data.statement)
                            && Self::compare_upwards_is_contradiction(
                                branch.clone(),
                                &statement_data,
                            )
                        {
                            branch_closed = true;
                            break 'outer;
                        }

                        match rule_fn(&mut statement_data, &mut stack, branch.clone()) {
                            Some(result) => match result.kind {
                                RuleApplyResultKind::AddNewBranches => {
                                    for new_statement_data in result.statements {
                                        let new_branch: Node<Branch<'a>> = Node::new(Branch {
                                            children: vec![new_statement_data],
                                            status: BranchStatus::Open,
                                        });
                                        branch.append(new_branch.clone());

                                        self.compute_branch(new_branch, &mut stack.clone());
                                        // See a.txt first
                                        // The call above will apply all remaining rules on this new branch
                                        // So other rules won't be applied to the current branch anymore
                                        // In this case, only branching rules remain, so these are the ones
                                        // I'm talking about
                                    }
                                }
                                RuleApplyResultKind::MergeIntoExisting => {
                                    new_children_queue.push(statement_data.clone());
                                }
                            },
                            None => {}
                        }
                    }
                }*/
            }

            if new_children_queue.is_empty() {
                break 'outer;
            } else {
                for statement_data in new_children_queue.drain(..) {
                    self.tree
                        .branch_from_id_mut(&branch_id)
                        .append(statement_data);
                }
            }
        }

        if branch_closed {
            self.tree.branch_from_id_mut(&branch_id).close();
        }
    }

    fn compare_upwards_is_contradiction(
        &self,
        branch_id: &Id,
        statement_data: &StatementData,
    ) -> bool {
        assert!(self.is_atomic_formula(&statement_data.statement));

        for ancestor_branch_id in self.tree.reverse_branch_id_iter(&branch_id) {
            let ancestor_branch = self.tree.branch_from_id_mut(&ancestor_branch_id);

            for statement_data_id in ancestor_branch.statement_id_iter() {
                let upwards_statement_data =
                    ancestor_branch.statement_from_id_mut(&statement_data_id);

                if !self.is_atomic_formula(&upwards_statement_data.statement) {
                    continue;
                }

                match (&upwards_statement_data.statement, &statement_data.statement) {
                    (Statement::LogicalNegation(ref rst), Statement::Simple(_))
                    | (Statement::LogicalNegation(ref rst), Statement::Singular(_, _))
                    | (Statement::Simple(_), Statement::LogicalNegation(ref rst))
                    | (Statement::Singular(_, _), Statement::LogicalNegation(ref rst)) => {
                        return **rst == statement_data.statement;
                    }
                    _ => unreachable!(),
                }
            }
        }

        false
    }

    // Non-branching rules - each returned statement is added to the current branch
    fn apply_qe_rule(&mut self, statement: &Statement) -> Option<RuleApplyResult> {
        match statement {
            Statement::LogicalNegation(ref inner) => match **inner {
                Statement::Existential(ref var, ref pred) => Some(RuleApplyResult {
                    kind: RuleApplyResultKind::MergeIntoExisting,
                    statements: vec![Statement::Universal(
                        var.clone(),
                        Predicate::Negative(Box::new(pred.clone())),
                    )],
                    is_processed: true,
                    rule_name: "QE".into(),
                }),
                Statement::Universal(ref var, ref pred) => Some(RuleApplyResult {
                    kind: RuleApplyResultKind::MergeIntoExisting,
                    statements: vec![Statement::Existential(
                        var.clone(),
                        Predicate::Negative(Box::new(pred.clone())),
                    )],
                    is_processed: true,
                    rule_name: "QE".into(),
                }),
                _ => None,
            },
            _ => None,
        }
    }

    fn apply_eq_rule(
        &self,
        statement: &Statement,
        stack: &mut Vec<SingularTerm>,
    ) -> Option<RuleApplyResult> {
        match statement {
            Statement::Existential(ref var, ref pred) => {
                let singular_term = Self::new_unused_in_singular_term_stack(&stack);

                stack.push(singular_term.clone());

                Some(RuleApplyResult {
                    kind: RuleApplyResultKind::MergeIntoExisting,
                    statements: vec![self.instantiate_predicate(pred, var, &singular_term)],
                    is_processed: true,
                    rule_name: "EQ".into(),
                })
            }
            _ => None,
        }
    }

    fn apply_uq_rule(
        &self,
        statement: &Statement,
        stack: &Vec<SingularTerm>,
    ) -> Option<RuleApplyResult> {
        match statement {
            Statement::Universal(ref var, ref pred) => {
                let mut statements = Vec::new();

                for term in stack.iter() {
                    // UQ rule already applied to this term
                    if !statement_data.applied_to.is_empty()
                        && statement_data.applied_to.iter().any(|x| {
                            x.0 == *term
                                && self
                                    .tree
                                    .reverse_branch_id_iter(&branch_id)
                                    .any(|y| x.1 == y)
                        })
                    {
                        continue;
                    }

                    statements.push(self.instantiate_predicate(pred, var, &term));

                    statement_data
                        .applied_to
                        .push((term.clone(), branch_id.clone()));
                }

                if statements.is_empty() {
                    None
                } else {
                    let mut statement_datas = Vec::new();

                    for statement in &statements {
                        statement_datas.push(StatementData {
                            is_processed: false,
                            applied_to: Vec::new(),
                            derived_from: Some((
                                (statement_id.clone(), branch_id.clone()),
                                "UQ".into(),
                            )),
                            statement: statement.clone(),
                        });
                    }

                    Some(RuleApplyResult {
                        kind: RuleApplyResultKind::MergeIntoExisting,
                        statements: statement_datas,
                    })
                }
            }
            _ => None,
        }
    }

    fn apply_double_negation_rule(
        &self,
        statement_id: &Id,
        branch_id: &Id,
    ) -> Option<RuleApplyResult> {
        let mut statement_data = self
            .tree
            .branch_from_id_mut(&branch_id)
            .statement_from_id_mut(&statement_id);

        match statement_data.statement {
            Statement::LogicalNegation(ref comp_st) => match **comp_st {
                Statement::LogicalNegation(ref inner_comp_st) => {
                    statement_data.is_processed = true;

                    Some(RuleApplyResult {
                        kind: RuleApplyResultKind::MergeIntoExisting,
                        statements: vec![StatementData {
                            is_processed: false,
                            applied_to: Vec::new(),
                            derived_from: Some((
                                (statement_id.clone(), branch_id.clone()),
                                "~~".into(),
                            )),
                            statement: *inner_comp_st.clone(),
                        }],
                    })
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn apply_conjunction_rule(&self, statement_id: &Id, branch_id: &Id) -> Option<RuleApplyResult> {
        let mut statement_data = self
            .tree
            .branch_from_id_mut(&branch_id)
            .statement_from_id_mut(&statement_id);

        match statement_data.statement {
            Statement::LogicalConjunction(ref lst, ref rst) => {
                statement_data.is_processed = true;

                Some(RuleApplyResult {
                    kind: RuleApplyResultKind::MergeIntoExisting,
                    statements: vec![
                        StatementData {
                            is_processed: false,
                            applied_to: Vec::new(),
                            derived_from: Some((
                                (statement_id.clone(), branch_id.clone()),
                                "&".into(),
                            )),
                            statement: *lst.clone(),
                        },
                        StatementData {
                            is_processed: false,
                            applied_to: Vec::new(),
                            derived_from: Some((
                                (statement_id.clone(), branch_id.clone()),
                                "&".into(),
                            )),
                            statement: *rst.clone(),
                        },
                    ],
                })
            }
            _ => None,
        }
    }

    fn apply_negation_of_conditional_rule(
        &self,
        statement_id: &Id,
        branch_id: &Id,
    ) -> Option<RuleApplyResult> {
        let mut statement_data = self
            .tree
            .branch_from_id_mut(&branch_id)
            .statement_from_id_mut(&statement_id);

        match statement_data.statement {
            Statement::LogicalNegation(ref comp_st) => match **comp_st {
                Statement::LogicalConditional(ref lst, ref rst) => {
                    statement_data.is_processed = true;

                    Some(RuleApplyResult {
                        kind: RuleApplyResultKind::MergeIntoExisting,
                        statements: vec![
                            StatementData {
                                is_processed: false,
                                applied_to: Vec::new(),
                                derived_from: Some((
                                    (statement_id.clone(), branch_id.clone()),
                                    "~->".into(),
                                )),
                                statement: *lst.clone(),
                            },
                            StatementData {
                                is_processed: false,
                                applied_to: Vec::new(),
                                derived_from: Some((
                                    (statement_id.clone(), branch_id.clone()),
                                    "~->".into(),
                                )),
                                statement: Statement::LogicalNegation(rst.clone()),
                            },
                        ],
                    })
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn apply_negation_of_disjunction_rule(
        &self,
        statement_id: &Id,
        branch_id: &Id,
    ) -> Option<RuleApplyResult> {
        let mut statement_data = self
            .tree
            .branch_from_id_mut(&branch_id)
            .statement_from_id_mut(&statement_id);

        match statement_data.statement {
            Statement::LogicalNegation(ref comp_st) => match **comp_st {
                Statement::LogicalDisjunction(ref lst, ref rst) => {
                    statement_data.is_processed = true;

                    Some(RuleApplyResult {
                        kind: RuleApplyResultKind::MergeIntoExisting,
                        statements: vec![
                            StatementData {
                                is_processed: false,
                                applied_to: Vec::new(),
                                derived_from: Some((
                                    (statement_id.clone(), branch_id.clone()),
                                    "~+".into(),
                                )),
                                statement: Statement::LogicalNegation(lst.clone()),
                            },
                            StatementData {
                                is_processed: false,
                                applied_to: Vec::new(),
                                derived_from: Some((
                                    (statement_id.clone(), branch_id.clone()),
                                    "~+".into(),
                                )),
                                statement: Statement::LogicalNegation(rst.clone()),
                            },
                        ],
                    })
                }
                _ => None,
            },
            _ => None,
        }
    }

    // TODO: No support for nested quantifiers yet
    fn instantiate_predicate(
        &self,
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
                                            panic!("unknown variable in quantifier statement (nested quantifiers are not yet supported)");
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
            Predicate::Conjunctive(ref lpred, ref rpred) => Statement::LogicalConjunction(
                Box::new(self.instantiate_predicate(lpred, var, replace_with)),
                Box::new(self.instantiate_predicate(rpred, var, replace_with)),
            ),
            Predicate::Negative(ref rpred) => Statement::LogicalNegation(Box::new(
                self.instantiate_predicate(rpred, var, replace_with),
            )),
            Predicate::Disjunctive(ref lpred, ref rpred) => Statement::LogicalDisjunction(
                Box::new(self.instantiate_predicate(lpred, var, replace_with)),
                Box::new(self.instantiate_predicate(rpred, var, replace_with)),
            ),
            Predicate::Conditional(ref lpred, ref rpred) => Statement::LogicalConditional(
                Box::new(self.instantiate_predicate(lpred, var, replace_with)),
                Box::new(self.instantiate_predicate(rpred, var, replace_with)),
            ),
        }
    }

    // Branching rules - each returned statement starts a new branch
    fn apply_conditional_rule(&self, statement_id: &Id, branch_id: &Id) -> Option<RuleApplyResult> {
        let mut statement_data = self
            .tree
            .branch_from_id_mut(&branch_id)
            .statement_from_id_mut(&statement_id);

        match statement_data.statement {
            Statement::LogicalConditional(ref lst, ref rst) => {
                statement_data.is_processed = true;

                Some(RuleApplyResult {
                    kind: RuleApplyResultKind::AddNewBranches,
                    statements: vec![
                        StatementData {
                            is_processed: false,
                            applied_to: Vec::new(),
                            derived_from: Some((
                                (statement_id.clone(), branch_id.clone()),
                                "->".into(),
                            )),
                            statement: Statement::LogicalNegation(lst.clone()),
                        },
                        StatementData {
                            is_processed: false,
                            applied_to: Vec::new(),
                            derived_from: Some((
                                (statement_id.clone(), branch_id.clone()),
                                "->".into(),
                            )),
                            statement: *rst.clone(),
                        },
                    ],
                })
            }
            _ => None,
        }
    }

    fn apply_negation_of_conjunction_rule(
        &self,
        statement_id: &Id,
        branch_id: &Id,
    ) -> Option<RuleApplyResult> {
        let mut statement_data = self
            .tree
            .branch_from_id_mut(&branch_id)
            .statement_from_id_mut(&statement_id);

        match statement_data.statement {
            Statement::LogicalNegation(ref rst) => match **rst {
                Statement::LogicalConjunction(ref inner_lst, ref inner_rst) => {
                    statement_data.is_processed = true;

                    Some(RuleApplyResult {
                        kind: RuleApplyResultKind::AddNewBranches,
                        statements: vec![
                            StatementData {
                                is_processed: false,
                                applied_to: Vec::new(),
                                derived_from: Some((
                                    (statement_id.clone(), branch_id.clone()),
                                    "~&".into(),
                                )),
                                statement: Statement::LogicalNegation(inner_lst.clone()),
                            },
                            StatementData {
                                is_processed: false,
                                applied_to: Vec::new(),
                                derived_from: Some((
                                    (statement_id.clone(), branch_id.clone()),
                                    "~&".into(),
                                )),
                                statement: Statement::LogicalNegation(inner_rst.clone()),
                            },
                        ],
                    })
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn apply_disjunction_rule(&self, statement_id: &Id, branch_id: &Id) -> Option<RuleApplyResult> {
        let mut statement_data = self
            .tree
            .branch_from_id_mut(&branch_id)
            .statement_from_id_mut(&statement_id);

        match statement_data.statement {
            Statement::LogicalDisjunction(ref lst, ref rst) => {
                statement_data.is_processed = true;

                Some(RuleApplyResult {
                    kind: RuleApplyResultKind::AddNewBranches,
                    statements: vec![
                        StatementData {
                            is_processed: false,
                            applied_to: Vec::new(),
                            derived_from: Some((
                                (statement_id.clone(), branch_id.clone()),
                                "+".into(),
                            )),
                            statement: *lst.clone(),
                        },
                        StatementData {
                            is_processed: false,
                            applied_to: Vec::new(),
                            derived_from: Some((
                                (statement_id.clone(), branch_id.clone()),
                                "+".into(),
                            )),
                            statement: *rst.clone(),
                        },
                    ],
                })
            }
            _ => None,
        }
    }

    // TODO: NO TEST
    fn new_unused_in_singular_term_stack(stack: &Vec<SingularTerm>) -> SingularTerm {
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

    fn build_singular_term_stack(&self) -> Vec<SingularTerm> {
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

        let main_trunk = self.tree.branch_from_id_mut(&self.tree.main_trunk());

        for statement_data_id in main_trunk.statement_id_iter() {
            find_in_statement(
                &mut stack,
                &main_trunk
                    .statement_from_id_mut(&statement_data_id)
                    .statement,
            );
        }

        stack
    }

    fn is_atomic_formula(&self, statement: &Statement) -> bool {
        match statement {
            Statement::Simple(_) | Statement::Singular(_, _) => true,
            Statement::LogicalNegation(ref lst) => match **lst {
                Statement::Simple(_) | Statement::Singular(_, _) => true,
                _ => false,
            },
            _ => false,
        }
    }
}
/*
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn applies_qe_rule() {
        let statement1 = Statement::LogicalNegation(Box::new(Statement::Universal(
            Variable('y', Subscript(None)),
            Predicate::Simple(PredicateLetter('F', Subscript(None), Degree(1)), Vec::new()),
        )));
        let statement2 = Statement::LogicalNegation(Box::new(Statement::Existential(
            Variable('x', Subscript(None)),
            Predicate::Simple(PredicateLetter('C', Subscript(None), Degree(1)), Vec::new()),
        )));

        let truth_tree_method = TruthTreeMethod::new(&vec![statement1.clone(), statement2.clone()]);

        match truth_tree_method.apply_qe_rule(&statement1) {
            Some(result) => match result {
                Statement::Existential(_, ref pred) => match pred {
                    Predicate::Negative(_) => {}
                    _ => assert!(false, "result is not of correct kind"),
                },
                _ => assert!(false, "result is not of correct kind"),
            },
            None => assert!(false, "failed"),
        }

        match truth_tree_method.apply_qe_rule(&statement2) {
            Some(result) => match result {
                Statement::Universal(_, ref pred) => match pred {
                    Predicate::Negative(_) => {}
                    _ => assert!(false, "result is not of correct kind"),
                },
                _ => assert!(false, "result is not of correct kind"),
            },
            None => assert!(false, "failed"),
        }
    }

    #[test]
    fn applies_eq_rule() {
        let statement = Statement::Existential(
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
        );

        let truth_tree_method = TruthTreeMethod::new(&vec![statement.clone()]);

        match truth_tree_method.apply_eq_rule(
            &statement,
            &mut vec![
                SingularTerm('a', Subscript(None)),
                SingularTerm('b', Subscript(None)),
            ],
        ) {
            Some(result) => match result {
                Statement::LogicalConditional(_, _) => {}
                _ => assert!(false, "result not a conditional statement"),
            },
            None => assert!(false, "failed"),
        }
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

        let result = truth_tree_method.build_singular_term_stack(
            truth_tree_method.find_end_of_branch(truth_tree_method.tree.clone()),
        );

        assert!(
            result.len() == 2 as usize,
            "number of singular terms is not 2"
        );

        assert!(
            result.contains(&SingularTerm('a', Subscript(None))),
            "result does not contain singular term 'a'"
        );

        assert!(
            result.contains(&SingularTerm('b', Subscript(None))),
            "result does not contain singular term 'a'"
        );
    }

    #[test]
    fn applies_uq_rule() {
        let statement = Statement::Universal(
            Variable('x', Subscript(None)),
            Predicate::Simple(
                PredicateLetter('B', Subscript(None), Degree(1)),
                vec![Term::Variable(Variable('x', Subscript(None)))],
            ),
        );

        let truth_tree_method = TruthTreeMethod::new(&vec![statement.clone()]);

        match truth_tree_method.apply_uq_rule(
            &statement,
            &vec![
                SingularTerm('a', Subscript(None)),
                SingularTerm('b', Subscript(None)),
            ],
        ) {
            Some(result) => {
                assert!(
                    result.len() == 2 as usize,
                    "number of resulting statements is not 2"
                );

                // TODO
            }
            None => assert!(false, "failed"),
        }
    }

    #[test]
    fn applies_double_negation_rule() {
        let statement = Statement::LogicalNegation(Box::new(Statement::LogicalNegation(Box::new(
            Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
        ))));

        let truth_tree_method = TruthTreeMethod::new(&vec![statement.clone()]);

        match truth_tree_method.apply_double_negation_rule(&statement) {
            Some(result) => match result {
                Statement::Simple(_) => {}
                _ => assert!(false, "resulting statement is not of correct kind"),
            },
            None => assert!(false, "failed"),
        }
    }

    #[test]
    fn applies_conjunction_rule() {
        let statement = Statement::LogicalConjunction(
            Box::new(Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None),
            ))),
            Box::new(Statement::Simple(SimpleStatementLetter(
                'B',
                Subscript(None),
            ))),
        );

        let truth_tree_method = TruthTreeMethod::new(&vec![statement.clone()]);

        match truth_tree_method.apply_conjunction_rule(&statement) {
            Some((lst, rst)) => {
                match lst {
                    Statement::Simple(_) => {}
                    _ => assert!(false, "left statement is not of correct kind"),
                }

                match rst {
                    Statement::Simple(_) => {}
                    _ => assert!(false, "right statement is not of correct kind"),
                }
            }
            None => assert!(false, "failed"),
        }
    }

    #[test]
    fn applies_negation_of_conditional_rule() {
        let statement = Statement::LogicalNegation(Box::new(Statement::LogicalConditional(
            Box::new(Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None),
            ))),
            Box::new(Statement::Simple(SimpleStatementLetter(
                'B',
                Subscript(None),
            ))),
        )));

        let truth_tree_method = TruthTreeMethod::new(&vec![statement.clone()]);

        match truth_tree_method.apply_negation_of_conditional_rule(&statement) {
            Some((first, second)) => {
                match first {
                    Statement::Simple(_) => {}
                    _ => assert!(false, "first statement is not of correct kind"),
                }

                match second {
                    Statement::LogicalNegation(ref inner) => match **inner {
                        Statement::Simple(_) => {}
                        _ => assert!(false, "second statement is not of correct kind"),
                    },
                    _ => assert!(false, "second statement is not of correct kind"),
                }
            }
            None => assert!(false, "failed"),
        }
    }

    #[test]
    fn applies_negation_of_disjunction_rule() {
        let statement = Statement::LogicalNegation(Box::new(Statement::LogicalDisjunction(
            Box::new(Statement::Simple(SimpleStatementLetter(
                'A',
                Subscript(None),
            ))),
            Box::new(Statement::Simple(SimpleStatementLetter(
                'B',
                Subscript(None),
            ))),
        )));

        let truth_tree_method = TruthTreeMethod::new(&vec![statement.clone()]);

        match truth_tree_method.apply_negation_of_disjunction_rule(&statement) {
            Some((first, second)) => {
                match first {
                    Statement::LogicalNegation(ref inner) => match **inner {
                        Statement::Simple(_) => {}
                        _ => assert!(false, "first statement is not of correct kind"),
                    },
                    _ => assert!(false, "first statement is not of correct kind"),
                }

                match second {
                    Statement::LogicalNegation(ref inner) => match **inner {
                        Statement::Simple(_) => {}
                        _ => assert!(false, "second statement is not of correct kind"),
                    },
                    _ => assert!(false, "second statement is not of correct kind"),
                }
            }
            None => assert!(false, "failed"),
        }
    }
}
*/
