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

    pub fn append_statement(&mut self, statement: T) -> Id {
        assert!(!self.closed, "attempt to append statement to closed branch");

        let last_child_id = self.statement_ids().last().unwrap();

        Id(self
            .children
            .insert(Node::new(statement), UnderNode(&last_child_id.0))
            .unwrap())
    }

    pub fn statement_ids(&self) -> IdsIter<T> {
        IdsIter {
            tree: &self.children,
            stack: vec![self.children.root_node_id().unwrap()],
        }
    }

    pub fn statement_from_id(&self, id: &Id) -> &T {
        self.children.get(&id.0).expect("invalid id").data()
    }

    pub fn statements(&self) -> StatementsIter<T> {
        StatementsIter {
            tree: &self.children,
            stack: vec![self.children.root_node_id().unwrap()],
        }
    }
}

pub struct StatementsIter<'a, T> {
    tree: &'a Tree<T>,
    stack: Vec<&'a NodeId>,
}

impl<'a, T> Iterator for StatementsIter<'a, T> {
    type Item = (Id, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        // self.stack.push(root_item);

        // Implementation of preorder traversal over the tree

        if self.stack.is_empty() {
            return None;
        }

        let id = self.stack.pop().unwrap();

        for child_id in self.tree.children_ids(&id).expect("invalid id") {
            self.stack.push(&child_id);
        }

        // We can clone IDs only because branches/statement trees do not allow
        // removing any nodes, hence there will never
        // be references to non-existing nodes
        Some((Id(id.clone()), self.tree.get(id).unwrap().data()))
    }
}

pub struct IdsIter<'a, T> {
    tree: &'a Tree<T>,
    stack: Vec<&'a NodeId>,
}

impl<'a, T> Iterator for IdsIter<'a, T> {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        // self.stack.push(root_item);

        // Implementation of preorder traversal over the tree

        if self.stack.is_empty() {
            return None;
        }

        let id = self.stack.pop().unwrap();

        for child_id in self.tree.children_ids(&id).expect("invalid id") {
            self.stack.push(&child_id);
        }

        // We can clone IDs only because branches/statement trees do not allow
        // removing any nodes, hence there will never
        // be references to non-existing nodes
        Some(Id(id.clone()))
    }
}

pub struct UpwardsBranchIdsIter<'a, T> {
    iter: Chain<Once<&'a NodeId>, AncestorIds<'a, Branch<T>>>,
}

impl<'a, T> Iterator for UpwardsBranchIdsIter<'a, T> {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| Id(x.clone()))
    }
}

pub struct UpwardsBranchesIter<'a, T> {
    iter: Chain<Once<&'a NodeId>, AncestorIds<'a, Branch<T>>>,
    tree: &'a Tree<Branch<T>>,
}

impl<'a, T> Iterator for UpwardsBranchesIter<'a, T> {
    type Item = (Id, &'a Branch<T>);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|x| (Id(x.clone()), self.tree.get(&x).unwrap().data()))
    }
}

pub struct BranchImmediateChildrenIdsIter<'a> {
    iter: ChildrenIds<'a>,
}

impl<'a> Iterator for BranchImmediateChildrenIdsIter<'a> {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| Id(x.clone()))
    }
}

pub struct TruthTree<T> {
    tree: Tree<Branch<T>>,
}

impl<'a, T> TruthTree<T>
where
    T: Clone,
{
    pub fn new(main_branch: Branch<T>) -> Self {
        TruthTree {
            tree: TreeBuilder::new().with_root(Node::new(main_branch)).build(),
        }
    }

    pub fn main_trunk_id(&self) -> Id {
        Id(self.tree.root_node_id().unwrap().clone())
    }

    pub fn traverse_upwards_branch_ids(&'a self, branch_id: &'a Id) -> UpwardsBranchIdsIter<'a, T> {
        UpwardsBranchIdsIter {
            iter: once(&branch_id.0).chain(
                self.tree
                    .ancestor_ids(&branch_id.0)
                    .expect("invalid branch_id"),
            ),
        }
    }

    pub fn traverse_upwards_branches(&'a self, branch_id: &'a Id) -> UpwardsBranchesIter<'a, T> {
        UpwardsBranchesIter {
            iter: once(&branch_id.0).chain(
                self.tree
                    .ancestor_ids(&branch_id.0)
                    .expect("invalid branch_id"),
            ),
            tree: &self.tree,
        }
    }

    pub fn traverse_downwards_branch_ids(&'a self, branch_id: &'a Id) -> IdsIter<'a, Branch<T>> {
        IdsIter {
            tree: &self.tree,
            stack: vec![&branch_id.0],
        }
    }

    pub fn traverse_branch_immediate_children_ids(
        &'a self,
        branch_id: &'a Id,
    ) -> BranchImmediateChildrenIdsIter<'a> {
        BranchImmediateChildrenIdsIter {
            iter: self.tree.children_ids(&branch_id.0).unwrap(),
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

    pub fn append_branch_at(&mut self, branch: Branch<T>, as_child_of_branch_id: &Id) -> Id {
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
    fn struct_id_iter() {
        let mut tree = TreeBuilder::new().with_node_capacity(3).build();

        let root_id = tree.insert(Node::new(2), AsRoot).unwrap();

        let child_1_id = tree.insert(Node::new(3), UnderNode(&root_id)).unwrap();
        let child_2_id = tree.insert(Node::new(4), UnderNode(&child_1_id)).unwrap();

        let mut iter = IdsIter {
            tree: &tree,
            stack: vec![&root_id],
        };

        assert_eq!(iter.next(), Some(Id(root_id.clone())));
        assert_eq!(iter.next(), Some(Id(child_1_id)));
        assert_eq!(iter.next(), Some(Id(child_2_id)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn struct_traverse_upwards_branch_ids() {
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

        let mut iter = UpwardsBranchIdsIter {
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
    fn branch_append_statement() {
        let mut branch = Branch::new(vec!["Hello, world!"]);

        branch.append_statement("Goodbye, world!");

        assert_eq!(
            branch
                .children
                .get(&branch.statement_ids().last().unwrap().0)
                .expect("no child")
                .data(),
            &"Goodbye, world!"
        );
    }

    #[test]
    fn branch_statement_ids() {
        let branch = Branch::new(vec!["Hello, world!", "Goodbye, my lover!"]);

        let mut statements_iter = branch.statement_ids();

        assert_eq!(
            branch.statement_from_id(&statements_iter.next().unwrap()),
            &"Hello, world!"
        );
        assert_eq!(
            branch.statement_from_id(&statements_iter.next().unwrap()),
            &"Goodbye, my lover!"
        );
        assert_eq!(statements_iter.next(), None);
    }

    #[test]
    fn branch_statements() {
        let branch = Branch::new(vec!["Hello, world!", "Goodbye, my lover!"]);

        let mut statements_iter = branch.statements();

        let (_, first_statement) = statements_iter.next().unwrap();

        assert_eq!(first_statement, &"Hello, world!");

        let (_, second_statement) = statements_iter.next().unwrap();

        assert_eq!(second_statement, &"Goodbye, my lover!");

        assert_eq!(statements_iter.next(), None);
    }

    #[test]
    fn branch_statement_from_id() {
        let branch = Branch::new(vec!["Hello, world!"]);

        let root_id = branch.statement_ids().next().unwrap();

        assert_eq!(branch.statement_from_id(&root_id), &"Hello, world!");
    }

    #[test]
    fn statement_tree_main_trunk_id() {
        let statement_tree = TruthTree::new(Branch::new(vec!["Hello!"]));

        let root_id = statement_tree.main_trunk_id();

        assert_eq!(
            root_id,
            Id(statement_tree.tree.root_node_id().unwrap().clone())
        );
    }

    #[test]
    fn statement_tree_traverse_upwards_branch_ids() {
        let mut statement_tree = TruthTree::new(Branch::new(vec!["Hello"]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![", world!"]), &root_id);

        let mut iter = statement_tree.traverse_upwards_branch_ids(&child_branch_1_id);

        assert_eq!(iter.next(), Some(Id(child_branch_1_id.0.clone())));
        assert_eq!(iter.next(), Some(statement_tree.main_trunk_id()));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn statement_tree_traverse_upwards_branches() {
        let mut statement_tree = TruthTree::new(Branch::new(vec!["Hello"]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![", world!"]), &root_id);

        let mut iter = statement_tree.traverse_upwards_branches(&child_branch_1_id);

        let (_, first_branch) = iter.next().unwrap();

        assert_eq!(
            first_branch.statement_from_id(&first_branch.statement_ids().next().unwrap()),
            &", world!"
        );

        let (_, second_branch) = iter.next().unwrap();

        assert_eq!(
            second_branch.statement_from_id(&second_branch.statement_ids().next().unwrap()),
            &"Hello"
        );

        match iter.next() {
            Some(_) => assert!(false),
            _ => {}
        }
    }

    #[test]
    fn statement_tree_traverse_downwards_branch_ids() {
        let mut statement_tree = TruthTree::new(Branch::new(vec!["Hello"]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![", world!"]), &root_id);

        let child_branch_2_id = statement_tree
            .append_branch_at(Branch::new(vec!["Goodbye, my lover!"]), &child_branch_1_id);

        let mut iter = statement_tree.traverse_downwards_branch_ids(&root_id);

        assert_eq!(iter.next(), Some(root_id.clone()));
        assert_eq!(iter.next(), Some(child_branch_1_id));
        assert_eq!(iter.next(), Some(child_branch_2_id));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn statement_tree_traverse_branch_immediate_children_ids() {
        let mut statement_tree = TruthTree::new(Branch::new(vec!["Hello"]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![", world!"]), &root_id);

        statement_tree
            .append_branch_at(Branch::new(vec!["Goodbye, my lover!"]), &child_branch_1_id);

        let mut iter = statement_tree.traverse_branch_immediate_children_ids(&root_id);

        assert_eq!(iter.next(), Some(child_branch_1_id));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn statement_tree_branch_is_last_child() {
        let mut statement_tree = TruthTree::new(Branch::new(vec!["Hello"]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![", world!"]), &root_id);

        assert!(
            !statement_tree.branch_is_last_child(&root_id),
            "root is not last child"
        );

        assert!(
            statement_tree.branch_is_last_child(&child_branch_1_id),
            "child_branch_1 is last child"
        );
    }

    #[test]
    fn statement_tree_branch_from_id_mut() {
        let mut statement_tree = TruthTree::new(Branch::new(vec!["Hello"]));

        let root_id = statement_tree.main_trunk_id();

        let branch = statement_tree.branch_from_id_mut(&root_id);

        branch.append_statement(", world!");

        let mut statements_iter = branch.statement_ids();

        assert_eq!(
            branch.statement_from_id(&statements_iter.next().unwrap()),
            &"Hello"
        );
        assert_eq!(
            branch.statement_from_id(&statements_iter.next().unwrap()),
            &", world!"
        );
    }

    #[test]
    fn statement_tree_branch_from_id() {
        let statement_tree = TruthTree::new(Branch::new(vec!["Hello"]));

        let root_id = statement_tree.main_trunk_id();

        let branch = statement_tree.branch_from_id(&root_id);

        let first_statement_id = branch.statement_ids().next().unwrap();

        assert_eq!(branch.statement_from_id(&first_statement_id), &"Hello");
    }

    #[test]
    fn statement_tree_append_branch_at() {
        let mut statement_tree = TruthTree::new(Branch::new(vec!["Hello"]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![", world!"]), &root_id);

        let mut iter = statement_tree.traverse_downwards_branch_ids(&root_id);

        assert_eq!(iter.next(), Some(Id(root_id.0.clone())));
        assert_eq!(iter.next(), Some(Id(child_branch_1_id.0.clone())));
        assert_eq!(iter.next(), None);
    }
}
