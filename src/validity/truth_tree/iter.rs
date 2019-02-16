use id_tree::{AncestorIds, ChildrenIds, NodeId, Tree};

use super::branch::{Branch, BranchNode};
use super::TreeId;

use std::iter::{Chain, Once};

/// An Iterator over the nodes of a branch. It starts at the root and traverses it
/// until the end of the branch (it does not iterate over children branches).
#[derive(Clone)]
pub struct StatementsIter<'a> {
    pub(in crate::validity::truth_tree) tree: &'a Tree<BranchNode>,
    pub(in crate::validity::truth_tree) iter: StatementIdsIter<'a>,
}

impl<'a> Iterator for StatementsIter<'a> {
    type Item = (TreeId, &'a BranchNode);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|x| (x.clone(), self.tree.get(&x.0).unwrap().data()))
    }
}

/// An Iterator over the IDs of the nodes of a branch. It starts at the root and traverses
/// the branch until its end (it does not iterate over children branches).
#[derive(Clone)]
pub struct StatementIdsIter<'a> {
    pub(in crate::validity::truth_tree) iter: IdsIter<'a, BranchNode>,
}

impl<'a> Iterator for StatementIdsIter<'a> {
    type Item = TreeId;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

/// An iterator over all of a branch's descendants' IDs (i.e. including children of children). It
/// includes the origin branch as well.
#[derive(Clone)]
pub struct DownwardsBranchesIdsIter<'a> {
    pub(in crate::validity::truth_tree) iter: IdsIter<'a, Branch>,
}

impl<'a> Iterator for DownwardsBranchesIdsIter<'a> {
    type Item = TreeId;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

/// An Iterator over all of a branch's descendants (i.e. including children of children). It includes
/// the origin branch as well.
#[derive(Clone)]
pub struct DownwardsBranchesIter<'a> {
    pub(in crate::validity::truth_tree) tree: &'a Tree<Branch>,
    pub(in crate::validity::truth_tree) iter: DownwardsBranchesIdsIter<'a>,
}

impl<'a> Iterator for DownwardsBranchesIter<'a> {
    type Item = (TreeId, &'a Branch);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|x| (x.clone(), self.tree.get(&x.0).unwrap().data()))
    }
}

/// An Iterator over a branch's ancestors' IDs. It includes the origin branch as well.
#[derive(Clone)]
pub struct UpwardsBranchesIdsIter<'a> {
    pub(in crate::validity::truth_tree) iter: Chain<Once<&'a NodeId>, AncestorIds<'a, Branch>>,
}

impl<'a> Iterator for UpwardsBranchesIdsIter<'a> {
    type Item = TreeId;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| TreeId(x.clone()))
    }
}

/// An Iterator over a branch's ancestors. It includes the origin branch as well.
#[derive(Clone)]
pub struct UpwardsBranchesIter<'a> {
    pub(in crate::validity::truth_tree) tree: &'a Tree<Branch>,
    pub(in crate::validity::truth_tree) iter: UpwardsBranchesIdsIter<'a>,
}

impl<'a> Iterator for UpwardsBranchesIter<'a> {
    type Item = (TreeId, &'a Branch);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|x| (x.clone(), self.tree.get(&x.0).unwrap().data()))
    }
}

/// An Iterator over the IDs of the direct descendants of a branch.
#[derive(Clone)]
pub struct BranchDirectDescendantsIdsIter<'a> {
    pub(in crate::validity::truth_tree) iter: ChildrenIds<'a>,
}

impl<'a> Iterator for BranchDirectDescendantsIdsIter<'a> {
    type Item = TreeId;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| TreeId(x.clone()))
    }
}

/// An Iterator over the direct descendants of a branch.
#[derive(Clone)]
pub struct BranchDirectDescendantsIter<'a> {
    pub(in crate::validity::truth_tree) tree: &'a Tree<Branch>,
    pub(in crate::validity::truth_tree) iter: BranchDirectDescendantsIdsIter<'a>,
}

impl<'a> Iterator for BranchDirectDescendantsIter<'a> {
    type Item = (TreeId, &'a Branch);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|x| (x.clone(), self.tree.get(&x.0).unwrap().data()))
    }
}

pub(in crate::validity::truth_tree) struct IdsIter<'a, T> {
    pub tree: &'a Tree<T>,
    pub stack: Vec<&'a NodeId>,
}

impl<'a, T> Iterator for IdsIter<'a, T> {
    type Item = TreeId;

    fn next(&mut self) -> Option<Self::Item> {
        // self.stack.push(root_item);

        // Implementation of preorder traversal over the tree

        if self.stack.is_empty() {
            return None;
        }

        let id = self.stack.pop().unwrap();

        for child_id in self.tree.children_ids(&id).unwrap() {
            self.stack.push(&child_id);
        }

        // We can clone IDs only because branches/statement trees do not allow
        // removing any nodes, hence there will never
        // be references to non-existing nodes
        Some(TreeId(id.clone()))
    }
}

// Note: Implemented manually because #[derive(Clone)] requires that T be Clone, but we only
// ever hold a reference to some data of type T
impl<'a, T> Clone for IdsIter<'a, T> {
    fn clone(&self) -> Self {
        IdsIter {
            tree: &self.tree,
            stack: self.stack.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{SimpleStatementLetter, Statement, Subscript};
    use id_tree::{InsertBehavior::*, Node, TreeBuilder};
    use std::iter::once;

    static BRANCH_NODE_1: BranchNode = BranchNode {
        statement: Statement::Simple(SimpleStatementLetter('A', Subscript(None))),
        derived_from: None,
    };
    static BRANCH_NODE_2: BranchNode = BranchNode {
        statement: Statement::Simple(SimpleStatementLetter('B', Subscript(None))),
        derived_from: None,
    };

    #[test]
    fn struct_id_iter() {
        let mut tree = TreeBuilder::new().with_node_capacity(3).build();

        let root_id = tree.insert(Node::new(2), AsRoot).unwrap();

        let child_1_id = tree.insert(Node::new(3), UnderNode(&root_id)).unwrap();
        let child_2_id = tree.insert(Node::new(4), UnderNode(&child_1_id)).unwrap();

        let mut iter = IdsIter {
            tree: &tree,
            stack: vec![&root_id],
        };

        assert_eq!(iter.next(), Some(TreeId(root_id.clone())));
        assert_eq!(iter.next(), Some(TreeId(child_1_id)));
        assert_eq!(iter.next(), Some(TreeId(child_2_id)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn struct_traverse_upwards_branch_ids() {
        let mut tree = TreeBuilder::new().with_node_capacity(2).build();

        let root_id = tree
            .insert(Node::new(Branch::new(vec![BRANCH_NODE_1.clone()])), AsRoot)
            .unwrap();

        let child_1_id = tree
            .insert(
                Node::new(Branch::new(vec![BRANCH_NODE_2.clone()])),
                UnderNode(&root_id),
            )
            .unwrap();

        let mut iter = UpwardsBranchesIdsIter {
            iter: once(&child_1_id).chain(tree.ancestor_ids(&child_1_id).unwrap()),
        };

        assert_eq!(iter.next(), Some(TreeId(child_1_id.clone())));
        assert_eq!(iter.next(), Some(TreeId(root_id.clone())));
        assert_eq!(iter.next(), None);
    }
}
