use std::collections::{BTreeSet, HashMap};

use super::{
    cfg::NodeId,
    check_instr::{self, CheckInstr},
    stg_loc::StgLoc,
};

#[derive(Default)]
pub struct Interferences {
    pub(super) edges: HashMap<StgLoc, BTreeSet<StgLoc>>,
}

impl Interferences {
    pub fn interferes_with(&self, a: &StgLoc, b: &StgLoc) -> bool {
        self.edges
            .get(a)
            .map(|set| set.contains(b))
            .unwrap_or(false)
            || self
                .edges
                .get(b)
                .map(|set| set.contains(a))
                .unwrap_or(false)
    }

    pub fn from_live_sets(
        instrs: &[CheckInstr],
        live_outs: HashMap<NodeId, BTreeSet<StgLoc>>,
    ) -> Self {
        let mut edges: HashMap<_, BTreeSet<StgLoc>> = HashMap::new();

        let mut defs_buf = BTreeSet::new();
        let mut uses_buf = BTreeSet::new();

        for (id, instr) in instrs.iter().enumerate() {
            defs_buf.clear();
            uses_buf.clear();
            check_instr::defs_and_uses(instr, &mut defs_buf, &mut uses_buf);

            for def in defs_buf.iter() {
                for out in live_outs.get(&id).map(|set| set.iter()).unwrap_or_default() {
                    if def != out {
                        edges.entry(def.clone()).or_default().insert(out.clone());
                    }
                }
            }
        }

        Self { edges }
    }
}
