use core::fmt;
use std::collections::{BTreeSet, HashMap};

use crate::compile::CodeGen;

use super::{cfg::NodeId, check_instr::CheckInstr, defs_uses, stg_loc::StgLoc};

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
        codegen: &CodeGen,
    ) -> Self {
        let mut edges: HashMap<_, BTreeSet<StgLoc>> = HashMap::new();

        let mut defs_buf = BTreeSet::new();
        let mut uses_buf = BTreeSet::new();

        for (id, instr) in instrs.iter().enumerate() {
            defs_buf.clear();
            uses_buf.clear();
            defs_uses::defs_and_uses(instr, &mut defs_buf, &mut uses_buf, codegen);

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

impl fmt::Display for Interferences {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let all_stg_locs = self
            .edges
            .keys()
            .chain(self.edges.values().flatten())
            .collect::<BTreeSet<_>>();

        writeln!(f, "Interference graph:")?;

        for stg_loc in all_stg_locs.iter() {
            write!(f, "{stg_loc}:\t")?;
            for neighbor in all_stg_locs
                .iter()
                .filter(|neighbor| self.interferes_with(stg_loc, neighbor))
            {
                write!(f, "{neighbor}\t")?;
            }
            writeln!(f)?;
        }
        writeln!(f)?;

        Ok(())
    }
}
