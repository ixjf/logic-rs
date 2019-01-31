#![allow(dead_code)]

use id_tree::InsertBehavior::*;
use id_tree::*;
use std::iter::{once, Chain, Once};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Id(NodeId);

pub struct Branch<T> {
    children: Tree<T>,
    closed: bool,
}

impl<T> Branch<T>
where
    T: Clone,
{
    pub fn new(trunk: Vec<T>) -> Self {
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

    pub fn close(&mut self) {
        self.closed = true;
    }

    pub fn is_closed(&self) -> bool {
        self.closed
    }

    pub fn append(&mut self, statement: T) -> Id {
        assert!(!self.closed, "attempt to append statement to closed branch");

        let last_child_id = self.statement_id_iter().last().unwrap();

        Id(self
            .children
            .insert(Node::new(statement), UnderNode(&last_child_id.0))
            .unwrap())
    }

    pub fn statement_id_iter(&self) -> StatementIdIter<T> {
        StatementIdIter {
            tree: &self.children,
            curr_id: None,
        }
    }

    pub fn statement_from_id_mut(&mut self, id: &Id) -> &mut T {
        self.children.get_mut(&id.0).expect("invalid id").data_mut()
    }

    pub fn statement_from_id(&self, id: &Id) -> &T {
        self.children.get(&id.0).expect("invalid id").data()
    }
}

pub struct StatementIdIter<'a, T> {
    tree: &'a Tree<T>,
    curr_id: Option<NodeId>,
}

impl<'a, T> Iterator for StatementIdIter<'a, T> {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        // We can clone IDs only because Branches do not allow
        // removing any statements, hence there will never
        // be references to non-existing nodes

        match self.curr_id.clone() {
            Some(i) => match self.tree.children_ids(&i).expect("invalid id").next() {
                Some(next_id) => {
                    self.curr_id = Some(next_id.clone());
                    Some(Id(next_id.clone()))
                }
                None => None,
            },
            None => {
                let root_node_id = self.tree.root_node_id().expect("no root node");
                self.curr_id = Some(root_node_id.clone());
                Some(Id(root_node_id.clone()))
            }
        }
    }
}

pub struct BranchIdIter<'a, T> {
    tree: &'a Tree<Branch<T>>,
    stack: Vec<&'a NodeId>,
}

impl<'a, T> Iterator for BranchIdIter<'a, T> {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        // self.stack.push(root_item);

        // Preorder traversal

        if self.stack.is_empty() {
            return None;
        }

        let id = self.stack.pop().unwrap();

        for child_id in self.tree.children_ids(&id).expect("invalid id") {
            self.stack.push(&child_id);
        }

        // We can clone IDs only because Branches do not allow
        // removing any statements, hence there will never
        // be references to non-existing nodes
        Some(Id(id.clone()))
    }
}

pub struct ReverseBranchIdIter<'a, T> {
    iter: Chain<Once<&'a NodeId>, AncestorIds<'a, Branch<T>>>,
}

impl<'a, T> Iterator for ReverseBranchIdIter<'a, T> {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| Id(x.clone()))
    }
}

pub struct StatementTree<T> {
    tree: Tree<Branch<T>>,
}

impl<'a, T> StatementTree<T>
where
    T: Clone,
{
    pub fn new(main_branch: Branch<T>) -> Self {
        StatementTree {
            tree: TreeBuilder::new().with_root(Node::new(main_branch)).build(),
        }
    }

    pub fn main_trunk(&self) -> Id {
        Id(self.tree.root_node_id().unwrap().clone())
    }

    pub fn reverse_branch_id_iter(&'a self, branch_id: &'a Id) -> ReverseBranchIdIter<'a, T> {
        ReverseBranchIdIter {
            iter: once(&branch_id.0).chain(
                self.tree
                    .ancestor_ids(&branch_id.0)
                    .expect("invalid branch_id"),
            ),
        }
    }

    pub fn branch_id_iter(&'a self, branch_id: &'a Id) -> BranchIdIter<'a, T> {
        BranchIdIter {
            tree: &self.tree,
            stack: vec![&branch_id.0],
        }
    }

    pub fn branch_is_last_child(&'a self, branch_id: &'a Id) -> bool {
        self.tree
            .get(&branch_id.0)
            .expect("invalid branch_id")
            .children()
            .is_empty()
    }

    pub fn branch_from_id_mut(&mut self, branch_id: &Id) -> &mut Branch<T> {
        self.tree
            .get_mut(&branch_id.0)
            .expect("invalid branch_id")
            .data_mut()
    }

    pub fn branch_from_id(&self, branch_id: &Id) -> &Branch<T> {
        self.tree
            .get(&branch_id.0)
            .expect("invalid branch_id")
            .data()
    }

    pub fn append_branch(&mut self, branch: Branch<T>, as_child_of_branch_id: &Id) -> Id {
        assert!(
            !self.branch_from_id(&as_child_of_branch_id).is_closed(),
            "attempt to add child to closed branch"
        );

        Id(self
            .tree
            .insert(Node::new(branch), UnderNode(&as_child_of_branch_id.0))
            .expect("invalid branch_id"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn struct_statement_id_iter() {
        let mut tree = TreeBuilder::new().with_node_capacity(3).build();

        let root_id = tree.insert(Node::new(2), AsRoot).unwrap();

        let child_1_id = tree.insert(Node::new(3), UnderNode(&root_id)).unwrap();
        let child_2_id = tree.insert(Node::new(4), UnderNode(&child_1_id)).unwrap();

        let mut iter = StatementIdIter {
            tree: &tree,
            curr_id: None,
        };

        assert_eq!(iter.next(), Some(Id(root_id)));
        assert_eq!(iter.next(), Some(Id(child_1_id)));
        assert_eq!(iter.next(), Some(Id(child_2_id)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn struct_branch_id_iter() {
        let mut tree = TreeBuilder::new().with_node_capacity(2).build();

        let root_id = tree
            .insert(Node::new(Branch::new(vec!["Hello"])), AsRoot)
            .unwrap();

        let child_1_id = tree
            .insert(
                Node::new(Branch::new(vec![", world!"])),
                UnderNode(&root_id),
            )
            .unwrap();

        let mut iter = BranchIdIter {
            tree: &tree,
            stack: vec![&root_id],
        };

        assert_eq!(iter.next(), Some(Id(root_id.clone())));
        assert_eq!(iter.next(), Some(Id(child_1_id.clone())));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn struct_reverse_branch_id_iter() {
        let mut tree = TreeBuilder::new().with_node_capacity(2).build();

        let root_id = tree
            .insert(Node::new(Branch::new(vec!["Hello"])), AsRoot)
            .unwrap();

        let child_1_id = tree
            .insert(
                Node::new(Branch::new(vec![", world!"])),
                UnderNode(&root_id),
            )
            .unwrap();

        let mut iter = ReverseBranchIdIter {
            iter: once(&child_1_id).chain(tree.ancestor_ids(&child_1_id).unwrap()),
        };

        assert_eq!(iter.next(), Some(Id(child_1_id.clone())));
        assert_eq!(iter.next(), Some(Id(root_id.clone())));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn branch_new() {
        let branch = Branch::new(vec!["Hello, world!", "Goodbye, world!"]);

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
                .data(),
            &"Hello, world!"
        );

        assert_eq!(
            root_children_iter.next().expect("no first child").data(),
            &"Goodbye, world!"
        );

        assert_eq!(root_children_iter.count(), 0); // Because id_tree::Node doesn't implement PartialEq
                                                   // and so we can't compare next() with None
    }

    #[test]
    fn branch_is_closed() {
        let branch = Branch::new(vec!["Hello, world!"]);

        assert!(!branch.is_closed(), "branch was closed");
    }

    #[test]
    fn branch_close() {
        let mut branch = Branch::new(vec!["Hello, world!"]);

        branch.close();

        assert!(branch.closed, "branch was not closed");
    }

    #[test]
    fn branch_append() {
        let mut branch = Branch::new(vec!["Hello, world!"]);

        branch.append("Goodbye, world!");

        assert_eq!(
            branch
                .children
                .get(&branch.statement_id_iter().last().unwrap().0)
                .expect("no child")
                .data(),
            &"Goodbye, world!"
        );
    }

    #[test]
    fn branch_statement_from_id_mut() {
        let mut branch = Branch::new(vec!["Hello, world!"]);

        let root_id = branch.statement_id_iter().next().unwrap();

        assert_eq!(branch.statement_from_id_mut(&root_id), &"Hello, world!");
    }

    #[test]
    fn statement_tree_main_trunk() {
        let statement_tree = StatementTree::new(Branch::new(vec!["Hello!"]));

        let root_id = statement_tree.main_trunk();

        assert_eq!(
            root_id,
            Id(statement_tree.tree.root_node_id().unwrap().clone())
        );
    }

    #[test]
    fn statement_tree_reverse_branch_id_iter() {
        let mut statement_tree = StatementTree::new(Branch::new(vec!["Hello"]));

        let root_id = statement_tree.main_trunk();

        let child_branch_1_id =
            statement_tree.append_branch(Branch::new(vec![", world!"]), &root_id);

        let mut iter = statement_tree.reverse_branch_id_iter(&child_branch_1_id);

        assert_eq!(iter.next(), Some(Id(child_branch_1_id.0.clone())));
        assert_eq!(iter.next(), Some(statement_tree.main_trunk()));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn statement_tree_branch_id_iter() {
        let mut statement_tree = StatementTree::new(Branch::new(vec!["Hello"]));

        let root_id = statement_tree.main_trunk();

        let child_branch_1_id =
            statement_tree.append_branch(Branch::new(vec![", world!"]), &root_id);

        let mut iter = statement_tree.branch_id_iter(&root_id);

        assert_eq!(iter.next(), Some(statement_tree.main_trunk()));
        assert_eq!(iter.next(), Some(Id(child_branch_1_id.0.clone())));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn statement_tree_branch_from_id_mut() {
        let mut statement_tree = StatementTree::new(Branch::new(vec!["Hello"]));

        let root_id = statement_tree.main_trunk();

        let branch = statement_tree.branch_from_id_mut(&root_id);

        let first_statement_id = branch.statement_id_iter().next().unwrap();

        assert_eq!(branch.statement_from_id_mut(&first_statement_id), &"Hello");
    }

    #[test]
    fn statement_tree_append_branch() {
        let mut statement_tree = StatementTree::new(Branch::new(vec!["Hello"]));

        let root_id = statement_tree.main_trunk();

        let child_branch_1_id =
            statement_tree.append_branch(Branch::new(vec![", world!"]), &root_id);

        let mut iter = statement_tree.branch_id_iter(&root_id);

        assert_eq!(iter.next(), Some(Id(root_id.0.clone())));
        assert_eq!(iter.next(), Some(Id(child_branch_1_id.0.clone())));
        assert_eq!(iter.next(), None);
    }
}
