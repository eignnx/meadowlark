use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap},
};

use lark_vm::cpu::regs::Reg;

use super::{check_instr::CheckInstr, defs_uses, stg_loc::StgLoc};

pub type NodeId = usize;

/// Control Flow Graph
pub struct Cfg {
    pub(super) stmts: Vec<CheckInstr>,
    edges: BTreeSet<(NodeId, NodeId)>,
    entry: NodeId,
    exits: BTreeSet<NodeId>,
    live_ins_on_entry: BTreeSet<StgLoc>,
    live_outs_on_exit: BTreeSet<StgLoc>,

    labels: HashMap<String, NodeId>,
    /// Links that will be added after the CFG is built and all labels are defined.
    label_links: Vec<(usize, String)>,
    link_from_prev: bool,
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
            labels: HashMap::new(),
            label_links: Vec::new(),
            link_from_prev: false,
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

    /// Adds a label to the CFG. If `id` is `None`, the **NEXT** instruction will be labelled with the given name.
    pub fn add_label(&mut self, label: String, id: Option<NodeId>) {
        if let Some(id) = id {
            self.labels.insert(label, id);
        } else {
            self.label_links.push((self.stmts.len(), label));
        }
    }

    pub fn add_all_deferred_labels(&mut self) {
        for (id, label) in self.label_links.drain(..) {
            if let Some(target_id) = self.labels.get(&label) {
                self.edges.insert((id, *target_id));
            } else {
                panic!("Label {label} is undefined");
            }
        }
    }

    pub fn compute_live_ins_live_outs(
        &self,
    ) -> (
        HashMap<NodeId, BTreeSet<StgLoc>>,
        HashMap<NodeId, BTreeSet<StgLoc>>,
    ) {
        if !self.label_links.is_empty() {
            panic!("Please call `Cfg::add_all_deferred_labels` before computing live ins and outs");
        }

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
            defs_uses::defs_and_uses(instr, &mut defs_buf, &mut uses_buf);

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

    pub(crate) fn push_instr<'s>(
        &mut self,
        instr: CheckInstr,
        links: impl IntoIterator<Item = Link<'s>> + 's,
    ) -> NodeId {
        let id = self.stmts.len();

        self.stmts.push(instr);

        if self.link_from_prev {
            self.edges.insert((id - 1, id));
            self.link_from_prev = false;
        }

        for link in links.into_iter() {
            match link {
                Link::ToNext => {
                    self.link_from_prev = true;
                }
                Link::JumpToNodeId(target) => {
                    self.edges.insert((id, target));
                }
                Link::JumpToLabel(label) => {
                    self.label_links.push((id, label.into()));
                }
            }
        }
        id
    }
}

#[derive(Debug, Clone)]
pub enum Link<'s> {
    /// Program control proceeds to the next instruction after executing this instruction.
    ToNext,
    /// Program control (may) jump to the given node after executing this instruction.
    JumpToNodeId(NodeId),
    JumpToLabel(Cow<'s, str>),
}
