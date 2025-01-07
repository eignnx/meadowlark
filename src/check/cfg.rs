use std::collections::{BTreeSet, HashMap};

use lark_vm::cpu::regs::Reg;

use super::{
    check_instr::{self, CheckInstr},
    stg_loc::StgLoc,
};

pub type NodeId = usize;

/// Control Flow Graph
pub struct Cfg {
    pub(super) stmts: Vec<CheckInstr>,
    edges: BTreeSet<(NodeId, NodeId)>,
    entry: NodeId,
    exits: BTreeSet<NodeId>,
    live_ins_on_entry: BTreeSet<StgLoc>,
    live_outs_on_exit: BTreeSet<StgLoc>,
}

impl Cfg {
    pub fn new(stmts: Vec<CheckInstr>) -> Self {
        Self {
            stmts,
            edges: BTreeSet::new(),
            entry: 0,
            exits: BTreeSet::new(),
            live_ins_on_entry: [Reg::Ra.into()].into(),
            live_outs_on_exit: [Reg::Ra.into()].into(),
        }
    }

    pub fn with_edges(mut self, edges: impl Iterator<Item = (NodeId, NodeId)>) -> Self {
        self.edges.extend(edges);
        self
    }

    pub fn with_entry(mut self, entry: NodeId) -> Self {
        self.entry = entry;
        self
    }

    pub fn with_exits(mut self, exits: impl IntoIterator<Item = NodeId>) -> Self {
        self.exits.extend(exits);
        self
    }

    pub fn set_non_void_subr(mut self) -> Self {
        self.live_outs_on_exit.insert(StgLoc::Reg(Reg::Rv));
        self
    }

    fn successors(&self, node_id: NodeId) -> impl Iterator<Item = NodeId> + '_ {
        self.edges
            .iter()
            .filter_map(move |(from, to)| if *from == node_id { Some(*to) } else { None })
    }

    pub fn compute_live_ins_live_outs(
        &self,
    ) -> (
        HashMap<NodeId, BTreeSet<StgLoc>>,
        HashMap<NodeId, BTreeSet<StgLoc>>,
    ) {
        let mut live_ins: HashMap<NodeId, BTreeSet<StgLoc>> = HashMap::new();
        let mut live_outs: HashMap<NodeId, BTreeSet<StgLoc>> = HashMap::new();

        for exit_id in self.exits.iter() {
            live_outs
                .entry(*exit_id)
                .or_default()
                .extend(self.live_outs_on_exit.iter().cloned());
        }

        live_ins
            .entry(self.entry)
            .or_default()
            .extend(self.live_ins_on_entry.iter().cloned());

        loop {
            let changed = self.update_live_sets(&mut live_ins, &mut live_outs);

            if !changed {
                break;
            }
        }

        (live_ins, live_outs)
    }

    fn update_live_sets(
        &self,
        live_ins: &mut HashMap<usize, BTreeSet<StgLoc>>,
        live_outs: &mut HashMap<usize, BTreeSet<StgLoc>>,
    ) -> bool {
        let mut changed = false;
        let mut defs_buf = BTreeSet::new();
        let mut uses_buf = BTreeSet::new();

        for (id, instr) in self.stmts.iter().enumerate().rev() {
            defs_buf.clear();
            uses_buf.clear();
            check_instr::defs_and_uses(instr, &mut defs_buf, &mut uses_buf);

            let outs: &mut BTreeSet<StgLoc> = live_outs.entry(id).or_default();

            // Outs[id] = forall successors of id called succ_id: Union(Ins[succ_id])
            for succ_id in self.successors(id) {
                let succ_ins = live_ins.entry(succ_id).or_default().clone();
                for in_ in succ_ins {
                    changed |= outs.insert(in_.clone());
                }
            }

            let ins: &mut BTreeSet<StgLoc> = live_ins.entry(id).or_default();

            // Ins[id] = Uses[id] U (Outs[id] - Defs[id])
            for use_ in uses_buf.iter().cloned() {
                changed |= ins.insert(use_);
            }

            let outs_minus_defs = outs
                .clone()
                .difference(&defs_buf)
                .cloned()
                .collect::<Vec<_>>();

            for out in outs_minus_defs {
                changed |= ins.insert(out);
            }
        }
        changed
    }
}
