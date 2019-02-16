use super::algorithm::Rule;
use crate::parser::Statement;
use id_tree::InsertBehavior::*;
use id_tree::*;
use snowflake::ProcessUniqueId;
use std::iter::{once, Chain, Once};

#[cfg(feature = "serde_support")]
use serde::ser::{Serialize, SerializeSeq, SerializeStruct, Serializer};

/// Specifies a location to some node in the truth tree.
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

/// Identifies some derivation. A derivation is any application of some rule to some statement.
/// 
/// This ID is guaranteed to be unique for each different application of any rule. This ID
/// is useful to analyse the truth tree and know which statements are part of the same derivation,
/// since multiple statements may be derived from the same statement and rule, and yet not be
/// the result of the _same_ application of a rule (e.g. the universal quantifier rule
/// can be applied infinitely many times to the same statement).
/// 
/// **Serialization of this struct requires the feature `serde_support` to be enabled.**
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

    /// This field is a unique ID within this DerivationId. Two statements resulting from the same application of a rule
    /// added to the same branch will have different values for this field, but those two statements on a sibling branch
    /// will mirror the IDs from the previous branch. This is especially useful to construct a graphical representation
    /// of the truth tree: in order to group statements from the same derivation by level, it is necessary
    /// to actually know which statements match which. However, simply comparing the two statements is unreliable,
    /// since sometimes two different statements from the same derivation, as is the case of the result of the application of a
    /// conditional rule, _should_ be on the same line. If such a graphical representation is to not mix together
    /// on the same line different statements (other than the case mentioned and alike), even if from the same derivation,
    /// comparing against this field is required.
    pub index: u64, // Unique ID for identifying statements on the same 'line'
                    // e.g. if a statement results in two statements, each one of which starts a new branch (say, A IMPLIES B), then
                    // their index is the same. If, on the other hand, the statement results in two statements added to the same branch
                    // (say, A AND B), then each have a different index.
}

/// Identifies some branch node or some branch in a truth tree. This ID is guaranteed to be unique across any
/// truth trees created.
/// 
/// **Serialization of this struct requires the feature `serde_support` to be enabled.**
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
#[cfg_attr(feature = "serde_support", derive(Serialize))]
pub struct TreeId(NodeId); // Id is guaranteed to be unique to this process, and so can't clash with any branch ID either

/// Represents a branch in a truth tree. It is composed
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

    /// An Iterator over the IDs of this branch's nodes.
    pub fn statement_ids(&self) -> IdsIter<BranchNode> {
        IdsIter {
            tree: &self.children,
            stack: vec![self.children.root_node_id().unwrap()],
        }
    }

    /// Returns a reference to a specific node of this branch.
    pub fn statement_from_id(&self, id: &TreeId) -> &BranchNode {
        self.children.get(&id.0).expect("invalid id").data()
    }

    // FIXME: This doesn't need to be (TreeId, BranchNode). You can't and don't need to modify the node anyway!
    /// An Iterator over the nodes of this branch.
    pub fn statements(&self) -> StatementsIter {
        StatementsIter {
            tree: &self.children,
            stack: vec![self.children.root_node_id().unwrap()],
        }
    }
}

// TODO: Can't StatementsIter be made more general, then have both this and IdsIter derive
// from that same iterator?
/// An Iterator over the nodes of a branch. It starts at the root and traverses it
/// until the end of the branch (it does not iterate over children branches).
#[derive(Clone)]
pub struct StatementsIter<'a> {
    tree: &'a Tree<BranchNode>,
    stack: Vec<&'a NodeId>,
}

impl<'a> Iterator for StatementsIter<'a> {
    type Item = (TreeId, &'a BranchNode);

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
        Some((TreeId(id.clone()), self.tree.get(id).unwrap().data()))
    }
}

/// An Iterator over the IDs of either branches or statements. It implements
/// a pre-order traversal algorithm. If it is an iterator over the IDs of
/// nodes, it guarantees to traverse them from the root to the end of the branch.
#[derive(Clone)]
pub struct IdsIter<'a, T> {
    tree: &'a Tree<T>,
    stack: Vec<&'a NodeId>,
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

        for child_id in self.tree.children_ids(&id).expect("invalid id") {
            self.stack.push(&child_id);
        }

        // We can clone IDs only because branches/statement trees do not allow
        // removing any nodes, hence there will never
        // be references to non-existing nodes
        Some(TreeId(id.clone()))
    }
}

// FIXME Why can't I derive Clone here?
/// An Iterator over a branch's ancestors' IDs. It includes the origin branch as well.
pub struct UpwardsBranchIdsIter<'a> {
    iter: Chain<Once<&'a NodeId>, AncestorIds<'a, Branch>>,
}

impl<'a> Iterator for UpwardsBranchIdsIter<'a> {
    type Item = TreeId;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| TreeId(x.clone()))
    }
}

// FIXME Are both of these iterators needed?
// FIXME Why can't I derive Clone here?
/// An Iterator over a branch's ancestors. It includes the origin branch as well.
pub struct UpwardsBranchesIter<'a> {
    iter: Chain<Once<&'a NodeId>, AncestorIds<'a, Branch>>,
    tree: &'a Tree<Branch>,
}

impl<'a> Iterator for UpwardsBranchesIter<'a> {
    type Item = (TreeId, &'a Branch);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|x| (TreeId(x.clone()), self.tree.get(&x).unwrap().data()))
    }
}

/// An Iterator over the direct descendants of a branch.
#[derive(Clone)]
pub struct BranchImmediateChildrenIdsIter<'a> {
    iter: ChildrenIds<'a>,
}

impl<'a> Iterator for BranchImmediateChildrenIdsIter<'a> {
    type Item = TreeId;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| TreeId(x.clone()))
    }
}

/// Represents a truth tree generated by the truth tree algorithm.
pub struct TruthTree {
    tree: Tree<Branch>,
}

impl<'a> TruthTree {
    pub(in crate::validity) fn new(main_branch: Branch) -> Self {
        TruthTree {
            tree: TreeBuilder::new().with_root(Node::new(main_branch)).build(),
        }
    }

    /// Returns the ID of the root branch of the tree.
    pub fn main_trunk_id(&self) -> TreeId {
        TreeId(self.tree.root_node_id().unwrap().clone())
    }

    /// Returns an Iterator over some branch's ancestors' IDs. Includes the origin branch
    /// as well.
    pub fn traverse_upwards_branch_ids(
        &'a self,
        branch_id: &'a TreeId,
    ) -> UpwardsBranchIdsIter<'a> {
        UpwardsBranchIdsIter {
            iter: once(&branch_id.0).chain(
                self.tree
                    .ancestor_ids(&branch_id.0)
                    .expect("invalid branch_id"),
            ),
        }
    }

    /// Returns an Iterator over some branch's ancestors. Includes the origin branch
    /// as well.
    pub fn traverse_upwards_branches(&'a self, branch_id: &'a TreeId) -> UpwardsBranchesIter<'a> {
        UpwardsBranchesIter {
            iter: once(&branch_id.0).chain(
                self.tree
                    .ancestor_ids(&branch_id.0)
                    .expect("invalid branch_id"),
            ),
            tree: &self.tree,
        }
    }

    /// Returns an Iterator over all branches starting at `branch_id` (including it) using a pre-order
    /// traversal algorithm.
    pub fn traverse_downwards_branch_ids(&'a self, branch_id: &'a TreeId) -> IdsIter<Branch> {
        IdsIter {
            tree: &self.tree,
            stack: vec![&branch_id.0],
        }
    }

    /// Returns an Iterator over some branch's direct descendants.
    pub fn traverse_branch_immediate_children_ids(
        &'a self,
        branch_id: &'a TreeId,
    ) -> BranchImmediateChildrenIdsIter<'a> {
        BranchImmediateChildrenIdsIter {
            iter: self.tree.children_ids(&branch_id.0).unwrap(),
        }
    }

    /// Returns true if the branch has no children, false if it does.
    pub fn branch_is_last_child(&'a self, branch_id: &'a TreeId) -> bool {
        self.tree
            .get(&branch_id.0)
            .expect("invalid branch_id")
            .children()
            .is_empty()
    }

    pub(in crate::validity) fn branch_from_id_mut(&mut self, branch_id: &TreeId) -> &mut Branch {
        self.tree
            .get_mut(&branch_id.0)
            .expect("invalid branch_id")
            .data_mut()
    }

    /// Returns a reference to the `Branch` specified by `branch_id`.
    pub fn branch_from_id(&self, branch_id: &TreeId) -> &Branch {
        self.tree
            .get(&branch_id.0)
            .expect("invalid branch_id")
            .data()
    }

    pub(in crate::validity) fn append_branch_at(
        &mut self,
        branch: Branch,
        as_child_of_branch_id: &TreeId,
    ) -> TreeId {
        assert!(
            !self.branch_from_id(&as_child_of_branch_id).is_closed(),
            "attempt to add child to closed branch"
        );

        TreeId(
            self.tree
                .insert(Node::new(branch), UnderNode(&as_child_of_branch_id.0))
                .expect("invalid branch_id"),
        )
    }

    /// Returns true if there is at least one open branch in the entire tree, false if not.
    pub fn is_open(&self) -> bool {
        self.traverse_downwards_branch_ids(&self.main_trunk_id())
            .filter(|x| self.branch_is_last_child(&x) && !self.branch_from_id(&x).is_closed())
            .count()
            > 0
    }
}

#[cfg(feature = "serde_support")]
impl Serialize for TruthTree {
    /// Serializes the truth tree. Output has the following format:
    /// https://gist.github.com/ixjf/dd4ef260a05a46d2ec94873a53728599
    /// 
    /// The serialized output is guaranteed to maintain the order of the nodes
    /// relative to order of derivation.
    ///
    /// **Serialization requires the feature `serde_support` to be enabled.**
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        struct BranchSer<'a>(&'a TreeId, &'a TruthTree);

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

        struct BranchNodeSer<'a>(&'a TreeId, &'a BranchNode);

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

        struct BranchChildrenSer<'a>(&'a TreeId, &'a TruthTree);

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

        let mut iter = UpwardsBranchIdsIter {
            iter: once(&child_1_id).chain(tree.ancestor_ids(&child_1_id).unwrap()),
        };

        assert_eq!(iter.next(), Some(TreeId(child_1_id.clone())));
        assert_eq!(iter.next(), Some(TreeId(root_id.clone())));
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
            TreeId(statement_tree.tree.root_node_id().unwrap().clone())
        );
    }

    #[test]
    fn statement_tree_traverse_upwards_branch_ids() {
        let mut statement_tree = TruthTree::new(Branch::new(vec![BRANCH_NODE_1.clone()]));

        let root_id = statement_tree.main_trunk_id();

        let child_branch_1_id =
            statement_tree.append_branch_at(Branch::new(vec![BRANCH_NODE_2.clone()]), &root_id);

        let mut iter = statement_tree.traverse_upwards_branch_ids(&child_branch_1_id);

        assert_eq!(iter.next(), Some(TreeId(child_branch_1_id.0.clone())));
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

        assert_eq!(iter.next(), Some(TreeId(root_id.0.clone())));
        assert_eq!(iter.next(), Some(TreeId(child_branch_1_id.0.clone())));
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
