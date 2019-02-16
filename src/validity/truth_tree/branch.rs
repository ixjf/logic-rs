use super::{
    iter::{IdsIter, StatementIdsIter, StatementsIter},
    TreeId,
};
use crate::parser::Statement;
use crate::validity::algorithm::{DerivationId, Rule};
use id_tree::InsertBehavior::*;
use id_tree::*;

/// A location to some node in the truth tree.
#[derive(Clone, Debug)]
pub struct BranchNodeLocation {
    pub node_id: TreeId,
    pub branch_id: TreeId,
}

/// A statement of a branch, including information about where it was derived from.
#[derive(Clone, Debug)]
pub struct BranchNode {
    /// The statement.
    pub statement: Statement,
    /// If this statement was derived, contains a reference to the statement
    /// that it was derived from and which rule was applied, along with the identifier
    /// of the derivation.
    pub derived_from: Option<(BranchNodeLocation, Rule, DerivationId)>,
}

/// A branch of a truth tree. It is composed
/// of [BranchNode](struct.BranchNode.html)s.
///
/// The nodes of the branch are guaranteed to be in order of derivation.
pub struct Branch {
    children: Tree<BranchNode>,
    closed: bool,
}

impl Branch {
    pub(in crate::validity) fn new(trunk: Vec<BranchNode>) -> Self {
        assert!(trunk.len() > 0);

        let mut tree = TreeBuilder::new().build();

        let mut iter = trunk.iter();

        let root_id = tree
            .insert(Node::new(iter.next().unwrap().clone()), AsRoot)
            .unwrap();

        iter.fold(root_id, |parent_id, statement| {
            tree.insert(Node::new(statement.clone()), UnderNode(&parent_id))
                .unwrap()
        });

        Branch {
            children: tree,
            closed: false,
        }
    }

    pub(in crate::validity) fn close(&mut self) {
        self.closed = true;
    }

    pub fn is_closed(&self) -> bool {
        self.closed
    }

    pub(in crate::validity) fn append_statement(&mut self, statement: BranchNode) -> TreeId {
        assert!(!self.closed, "attempt to append statement to closed branch");

        let last_child_id = self.statement_ids().last().unwrap();

        TreeId(
            self.children
                .insert(Node::new(statement), UnderNode(&last_child_id.0))
                .unwrap(),
        )
    }

    /// Returns an Iterator over the IDs of this branch's nodes.
    pub fn statement_ids(&self) -> StatementIdsIter {
        StatementIdsIter {
            iter: IdsIter {
                tree: &self.children,
                stack: vec![self.children.root_node_id().unwrap()],
            },
        }
    }

    /// Returns a reference to a specific node of this branch.
    ///
    /// # Panics
    /// Panics if the ID provided does not represent a node from this branch.
    pub fn statement_from_id(&self, id: &TreeId) -> &BranchNode {
        self.children.get(&id.0).expect("invalid id").data()
    }

    /// Returns an Iterator over the nodes of this branch.
    pub fn statements(&self) -> StatementsIter {
        StatementsIter {
            tree: &self.children,
            iter: self.statement_ids(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{SimpleStatementLetter, Subscript};

    static BRANCH_NODE_1: BranchNode = BranchNode {
        statement: Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
        derived_from: None,
    };
    static BRANCH_NODE_2: BranchNode = BranchNode {
        statement: Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
        derived_from: None,
    };

    #[test]
    fn branch_new() {
        let branch = Branch::new(vec![BRANCH_NODE_1.clone(), BRANCH_NODE_2.clone()]);

        assert!(!branch.closed, "branch was closed");

        let mut root_children_iter = branch
            .children
            .children(branch.children.root_node_id().expect("no root node"))
            .unwrap();

        assert_eq!(
            branch
                .children
                .get(branch.children.root_node_id().expect("no root node"))
                .unwrap()
                .data()
                .statement,
            BRANCH_NODE_1.statement
        );

        assert_eq!(
            root_children_iter
                .next()
                .expect("no first child")
                .data()
                .statement,
            BRANCH_NODE_2.statement
        );

        assert_eq!(root_children_iter.count(), 0); // Because id_tree::Node doesn't implement PartialEq
                                                   // and so we can't compare next() with None
    }

    #[test]
    fn branch_is_closed() {
        let branch = Branch::new(vec![BRANCH_NODE_1.clone()]);

        assert!(!branch.is_closed(), "branch was closed");
    }

    #[test]
    fn branch_close() {
        let mut branch = Branch::new(vec![BRANCH_NODE_1.clone()]);

        branch.close();

        assert!(branch.closed, "branch was not closed");
    }

    #[test]
    fn branch_append_statement() {
        let mut branch = Branch::new(vec![BRANCH_NODE_1.clone()]);

        branch.append_statement(BRANCH_NODE_2.clone());

        assert_eq!(
            branch
                .children
                .get(&branch.statement_ids().last().unwrap().0)
                .expect("no child")
                .data()
                .statement,
            BRANCH_NODE_2.statement
        );
    }

    #[test]
    fn branch_statement_ids() {
        let branch = Branch::new(vec![BRANCH_NODE_1.clone(), BRANCH_NODE_2.clone()]);

        let mut statements_iter = branch.statement_ids();

        assert_eq!(
            branch
                .statement_from_id(&statements_iter.next().unwrap())
                .statement,
            BRANCH_NODE_1.statement
        );
        assert_eq!(
            branch
                .statement_from_id(&statements_iter.next().unwrap())
                .statement,
            BRANCH_NODE_2.statement
        );
        assert_eq!(statements_iter.next(), None);
    }

    #[test]
    fn branch_statements() {
        let branch = Branch::new(vec![BRANCH_NODE_1.clone(), BRANCH_NODE_2.clone()]);

        let mut statements_iter = branch.statements();

        let (_, first_statement) = statements_iter.next().unwrap();

        assert_eq!(first_statement.statement, BRANCH_NODE_1.statement);

        let (_, second_statement) = statements_iter.next().unwrap();

        assert_eq!(second_statement.statement, BRANCH_NODE_2.statement);

        assert_eq!(statements_iter.count(), 0); // equivalent to next() == None
    }

    #[test]
    fn branch_statement_from_id() {
        let branch = Branch::new(vec![BRANCH_NODE_1.clone()]);

        let root_id = branch.statement_ids().next().unwrap();

        assert_eq!(
            branch.statement_from_id(&root_id).statement,
            BRANCH_NODE_1.statement
        );
    }
}
