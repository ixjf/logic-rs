use super::algorithm::Rule;
use crate::parser::Statement;
use id_tree::InsertBehavior::*;
use id_tree::*;
use snowflake::ProcessUniqueId;
use std::iter::{once, Chain, Once};

#[cfg(feature = "serde_support")]
use serde::ser::{Serialize, SerializeSeq, SerializeStruct, Serializer};

#[derive(Clone, Debug)]
pub struct BranchNodeLocation {
    pub node_id: Id,
    pub branch_id: Id,
}

#[derive(Clone, Debug)]
pub struct BranchNode {
    pub statement: Statement,
    pub derived_from: Option<(BranchNodeLocation, Rule, DerivationId)>,
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
pub struct DerivationId {
    pub(super) id: ProcessUniqueId, // Identifies a node that comes from the same derivation as another with the same ID
    // Note that this doesn't mean a node that is derived from the same statement, but rather that it is part of the same
    // vector of resulting statements from the application of ONE rule at ONE point in time
    // So e.g. deriving UQ twice leads to two statements that were derived from the same statement,
    // but they don't have the same derivation ID because they come from two different applications of a rule
    // On the other hand, deriving with the conditional rule leads to two statements, derived from the same statement,
    // and ALSO from the same application of the rule,
    pub index: u64, // Unique ID for identifying statements on the same 'line'
                    // e.g. if a statement results in two statements, each one of which starts a new branch (say, A IMPLIES B), then
                    // their index is the same. If, on the other hand, the statement results in two statements added to the same branch
                    // (say, A AND B), then each have a different index.
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
pub struct Id(NodeId); // Id is guaranteed to be unique to this process, and so can't clash with any branch ID either

pub struct Branch {
    children: Tree<BranchNode>,
    closed: bool,
}

impl Branch {
    pub fn new(trunk: Vec<BranchNode>) -> Self {
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

    pub fn append_statement(&mut self, statement: BranchNode) -> Id {
        assert!(!self.closed, "attempt to append statement to closed branch");

        let last_child_id = self.statement_ids().last().unwrap();

        Id(self
            .children
            .insert(Node::new(statement), UnderNode(&last_child_id.0))
            .unwrap())
    }

    pub fn statement_ids(&self) -> IdsIter<BranchNode> {
        IdsIter {
            tree: &self.children,
            stack: vec![self.children.root_node_id().unwrap()],
        }
    }

    pub fn statement_from_id(&self, id: &Id) -> &BranchNode {
        self.children.get(&id.0).expect("invalid id").data()
    }

    pub fn statements(&self) -> StatementsIter {
        StatementsIter {
            tree: &self.children,
            stack: vec![self.children.root_node_id().unwrap()],
        }
    }
}

#[derive(Clone)]
pub struct StatementsIter<'a> {
    tree: &'a Tree<BranchNode>,
    stack: Vec<&'a NodeId>,
}

impl<'a> Iterator for StatementsIter<'a> {
    type Item = (Id, &'a BranchNode);

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

#[derive(Clone)]
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

// FIXME Why can't I derive Clone here?
pub struct UpwardsBranchIdsIter<'a> {
    iter: Chain<Once<&'a NodeId>, AncestorIds<'a, Branch>>,
}

impl<'a> Iterator for UpwardsBranchIdsIter<'a> {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| Id(x.clone()))
    }
}

// FIXME Why can't I derive Clone here?
pub struct UpwardsBranchesIter<'a> {
    iter: Chain<Once<&'a NodeId>, AncestorIds<'a, Branch>>,
    tree: &'a Tree<Branch>,
}

impl<'a> Iterator for UpwardsBranchesIter<'a> {
    type Item = (Id, &'a Branch);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|x| (Id(x.clone()), self.tree.get(&x).unwrap().data()))
    }
}

#[derive(Clone)]
pub struct BranchImmediateChildrenIdsIter<'a> {
    iter: ChildrenIds<'a>,
}

impl<'a> Iterator for BranchImmediateChildrenIdsIter<'a> {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| Id(x.clone()))
    }
}

pub struct TruthTree {
    tree: Tree<Branch>,
}

impl<'a> TruthTree {
    pub fn new(main_branch: Branch) -> Self {
        TruthTree {
            tree: TreeBuilder::new().with_root(Node::new(main_branch)).build(),
        }
    }

    pub fn main_trunk_id(&self) -> Id {
        Id(self.tree.root_node_id().unwrap().clone())
    }

    pub fn traverse_upwards_branch_ids(&'a self, branch_id: &'a Id) -> UpwardsBranchIdsIter<'a> {
        UpwardsBranchIdsIter {
            iter: once(&branch_id.0).chain(
                self.tree
                    .ancestor_ids(&branch_id.0)
                    .expect("invalid branch_id"),
            ),
        }
    }

    pub fn traverse_upwards_branches(&'a self, branch_id: &'a Id) -> UpwardsBranchesIter<'a> {
        UpwardsBranchesIter {
            iter: once(&branch_id.0).chain(
                self.tree
                    .ancestor_ids(&branch_id.0)
                    .expect("invalid branch_id"),
            ),
            tree: &self.tree,
        }
    }

    pub fn traverse_downwards_branch_ids(&'a self, branch_id: &'a Id) -> IdsIter<Branch> {
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

    pub fn branch_from_id_mut(&mut self, branch_id: &Id) -> &mut Branch {
        self.tree
            .get_mut(&branch_id.0)
            .expect("invalid branch_id")
            .data_mut()
    }

    pub fn branch_from_id(&self, branch_id: &Id) -> &Branch {
        self.tree
            .get(&branch_id.0)
            .expect("invalid branch_id")
            .data()
    }

    pub fn append_branch_at(&mut self, branch: Branch, as_child_of_branch_id: &Id) -> Id {
        assert!(
            !self.branch_from_id(&as_child_of_branch_id).is_closed(),
            "attempt to add child to closed branch"
        );

        Id(self
            .tree
            .insert(Node::new(branch), UnderNode(&as_child_of_branch_id.0))
            .expect("invalid branch_id"))
    }

    pub fn is_open(&self) -> bool {
        self.traverse_downwards_branch_ids(&self.main_trunk_id())
            .filter(|x| self.branch_is_last_child(&x) && !self.branch_from_id(&x).is_closed())
            .count()
            > 0
    }
}

#[cfg(feature = "serde_support")]
impl Serialize for TruthTree {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        struct BranchSer<'a>(&'a Id, &'a TruthTree);

        impl<'a> Serialize for BranchSer<'a> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
            {
                let mut branch = serializer.serialize_struct("Branch", 4)?;
                branch.serialize_field("id", &self.0)?;
                branch.serialize_field("closed", &self.1.branch_from_id(&self.0).is_closed())?;
                branch.serialize_field(
                    "nodes",
                    &BranchNodesSer(&self.1.branch_from_id(&self.0).statements()),
                )?;
                branch.serialize_field("children", &BranchChildrenSer(&self.0, &self.1))?;
                branch.end()
            }
        }

        struct BranchNodesSer<'a>(&'a StatementsIter<'a>);

        impl<'a> Serialize for BranchNodesSer<'a> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
            {
                let mut seq = serializer.serialize_seq(None)?;
                let iter = self.0.clone();
                for (statement_id, branch_node) in iter {
                    seq.serialize_element(&BranchNodeSer(&statement_id, &branch_node))?;
                }
                seq.end()
            }
        }

        struct BranchNodeSer<'a>(&'a Id, &'a BranchNode);

        impl<'a> Serialize for BranchNodeSer<'a> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
            {
                let mut node = serializer.serialize_struct("BranchNode", 3)?;
                node.serialize_field("id", &self.0)?;
                node.serialize_field("statement", &self.1.statement)?;
                node.serialize_field("derived_from", &DerivedFromSer(&self.1.derived_from))?;
                node.end()
            }
        }

        struct DerivedFromSer<'a>(&'a Option<(BranchNodeLocation, Rule, DerivationId)>);

        impl<'a> Serialize for DerivedFromSer<'a> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
            {
                match self.0 {
                    Some((node_loc, rule, derivation_id)) => {
                        let mut derived_from = serializer.serialize_struct("DerivedFrom", 3)?;
                        derived_from.serialize_field("node_id", &node_loc.node_id)?;
                        derived_from.serialize_field("branch_id", &node_loc.branch_id)?;
                        derived_from.serialize_field("rule", &rule)?;
                        derived_from.serialize_field("derivation_id", &derivation_id)?;
                        derived_from.end()
                    }
                    None => serializer.serialize_none(),
                }
            }
        }

        struct BranchChildrenSer<'a>(&'a Id, &'a TruthTree);

        impl<'a> Serialize for BranchChildrenSer<'a> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
            {
                let mut seq = serializer.serialize_seq(None)?;
                for branch_id in self.1.traverse_branch_immediate_children_ids(&self.0) {
                    seq.serialize_element(&BranchSer(&branch_id, &self.1))?;
                }
                seq.end()
            }
        }

        let mut tree = serializer.serialize_struct("TruthTree", 1)?;
        tree.serialize_field("main_trunk", &BranchSer(&self.main_trunk_id(), &self))?;
        tree.end()
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
    static BRANCH_NODE_3: BranchNode = BranchNode {
        statement: Statement::Simple(SimpleStatementLetter('C', Subscript(None))),
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

        assert_eq!(iter.next(), Some(Id(root_id.clone())));
        assert_eq!(iter.next(), Some(Id(child_1_id)));
        assert_eq!(iter.next(), Some(Id(child_2_id)));
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

        let mut iter = UpwardsBranchIdsIter {
            iter: once(&child_1_id).chain(tree.ancestor_ids(&child_1_id).unwrap()),
        };

        assert_eq!(iter.next(), Some(Id(child_1_id.clone())));
        assert_eq!(iter.next(), Some(Id(root_id.clone())));
        assert_eq!(iter.next(), None);
    }

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

    #[test]
    fn statement_tree_main_trunk_id() {
        let statement_tree = TruthTree::new(Branch::new(vec![BRANCH_NODE_1.clone()]));

        let root_id = statement_tree.main_trunk_id();

        assert_eq!(
            root_id,
            Id(statement_tree.tree.root_node_id().unwrap().clone())
        );
    }

    #[test]
    fn statement_tree_traverse_upwards_branch_ids() {
        let mut statement_tree = TruthTree::new(Branch::new(vec![BRANCH_NODE_1.clone()]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![BRANCH_NODE_2.clone()]), &root_id);

        let mut iter = statement_tree.traverse_upwards_branch_ids(&child_branch_1_id);

        assert_eq!(iter.next(), Some(Id(child_branch_1_id.0.clone())));
        assert_eq!(iter.next(), Some(statement_tree.main_trunk_id()));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn statement_tree_traverse_upwards_branches() {
        let mut statement_tree = TruthTree::new(Branch::new(vec![BRANCH_NODE_1.clone()]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![BRANCH_NODE_2.clone()]), &root_id);

        let mut iter = statement_tree.traverse_upwards_branches(&child_branch_1_id);

        let (_, first_branch) = iter.next().unwrap();

        assert_eq!(
            first_branch
                .statement_from_id(&first_branch.statement_ids().next().unwrap())
                .statement,
            BRANCH_NODE_2.statement
        );

        let (_, second_branch) = iter.next().unwrap();

        assert_eq!(
            second_branch
                .statement_from_id(&second_branch.statement_ids().next().unwrap())
                .statement,
            BRANCH_NODE_1.statement
        );

        match iter.next() {
            Some(_) => assert!(false),
            _ => {}
        }
    }

    #[test]
    fn statement_tree_traverse_downwards_branch_ids() {
        let mut statement_tree = TruthTree::new(Branch::new(vec![BRANCH_NODE_1.clone()]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![BRANCH_NODE_2.clone()]), &root_id);

        let child_branch_2_id = statement_tree
            .append_branch_at(Branch::new(vec![BRANCH_NODE_3.clone()]), &child_branch_1_id);

        let mut iter = statement_tree.traverse_downwards_branch_ids(&root_id);

        assert_eq!(iter.next(), Some(root_id.clone()));
        assert_eq!(iter.next(), Some(child_branch_1_id));
        assert_eq!(iter.next(), Some(child_branch_2_id));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn statement_tree_traverse_branch_immediate_children_ids() {
        let mut statement_tree = TruthTree::new(Branch::new(vec![BRANCH_NODE_1.clone()]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![BRANCH_NODE_2.clone()]), &root_id);

        statement_tree
            .append_branch_at(Branch::new(vec![BRANCH_NODE_3.clone()]), &child_branch_1_id);

        let mut iter = statement_tree.traverse_branch_immediate_children_ids(&root_id);

        assert_eq!(iter.next(), Some(child_branch_1_id));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn statement_tree_branch_is_last_child() {
        let mut statement_tree = TruthTree::new(Branch::new(vec![BRANCH_NODE_1.clone()]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![BRANCH_NODE_2.clone()]), &root_id);

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
        let mut statement_tree = TruthTree::new(Branch::new(vec![BRANCH_NODE_1.clone()]));

        let root_id = statement_tree.main_trunk_id();

        let branch = statement_tree.branch_from_id_mut(&root_id);

        branch.append_statement(BRANCH_NODE_2.clone());

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
    }

    #[test]
    fn statement_tree_branch_from_id() {
        let statement_tree = TruthTree::new(Branch::new(vec![BRANCH_NODE_1.clone()]));

        let root_id = statement_tree.main_trunk_id();

        let branch = statement_tree.branch_from_id(&root_id);

        let first_statement_id = branch.statement_ids().next().unwrap();

        assert_eq!(
            branch.statement_from_id(&first_statement_id).statement,
            BRANCH_NODE_1.statement
        );
    }

    #[test]
    fn statement_tree_append_branch_at() {
        let mut statement_tree = TruthTree::new(Branch::new(vec![BRANCH_NODE_1.clone()]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![BRANCH_NODE_2.clone()]), &root_id);

        let mut iter = statement_tree.traverse_downwards_branch_ids(&root_id);

        assert_eq!(iter.next(), Some(Id(root_id.0.clone())));
        assert_eq!(iter.next(), Some(Id(child_branch_1_id.0.clone())));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn statement_tree_is_open() {
        let mut statement_tree = TruthTree::new(Branch::new(vec![BRANCH_NODE_1.clone()]));

        assert!(
            statement_tree.is_open(),
            "returned not open but tree is open"
        );

        statement_tree
            .branch_from_id_mut(&statement_tree.main_trunk_id())
            .close();

        assert!(
            !statement_tree.is_open(),
            "returned open but tree is not open"
        );
    }
}
