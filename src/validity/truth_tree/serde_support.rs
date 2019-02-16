use super::{BranchNode, BranchNodeLocation, StatementsIter, TreeId, TruthTree};
use crate::validity::algorithm::{DerivationId, Rule};
use serde::ser::{Serialize, SerializeSeq, SerializeStruct, Serializer};

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
            &BranchNodesSer(self.1.branch_from_id(&self.0).statements()),
        )?;
        branch.serialize_field("children", &BranchChildrenSer(&self.0, &self.1))?;
        branch.end()
    }
}

struct BranchNodesSer<'a>(StatementsIter<'a>);

impl<'a> Serialize for BranchNodesSer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(None)?;
        for (statement_id, branch_node) in self.0 {
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
        for branch_id in self.1.traverse_branch_direct_descendants_ids(&self.0) {
            seq.serialize_element(&BranchSer(&branch_id, &self.1))?;
        }
        seq.end()
    }
}

impl Serialize for TruthTree {
    /// Serializes the truth tree. Output in JSON has the format as shown [here](
    /// https://gist.github.com/ixjf/dd4ef260a05a46d2ec94873a53728599).
    ///
    /// The serialized output is guaranteed to maintain the order of the nodes
    /// relative to the order of derivation.
    ///
    /// **Serialization requires the feature `serde_support` to be enabled.**
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut tree = serializer.serialize_struct("TruthTree", 1)?;
        tree.serialize_field("main_trunk", &BranchSer(&self.main_trunk_id(), &self))?;
        tree.end()
    }
}
