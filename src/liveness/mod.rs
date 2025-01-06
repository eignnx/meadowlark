use core::fmt;
use std::collections::{BTreeSet, HashMap, VecDeque};

use lark_vm::cpu::{
    instr::{
        self, Instr, OpcodeAddr, OpcodeImm, OpcodeOp, OpcodeReg, OpcodeRegImm, OpcodeRegReg,
        OpcodeRegRegImm, OpcodeRegRegReg,
    },
    regs::Reg,
};

/// Represents a **Storage Location**.
///
/// These will only be used for intra-procedural analysis (within one subroutine), so hopefully
/// `$sp` and `$gp` can be assumed to be constant throughout.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum StgLoc {
    Reg(Reg),

    /// A (possibly unbound) alias to another storage location.
    Alias(String),

    /// A location on the stack. The `i32` is the offset from the stack pointer. Even though
    /// `crate::ast::Instr` allows `crate::ast::ConstValue::ConstAlias`s in this place,
    /// this module requires they be resolved to actual numbers.
    Stack(i32),

    /// A global variable. The `i32` is the offset from the global pointer. Even though
    /// `crate::ast::Instr` allows `crate::ast::ConstValue::ConstAlias`s in this place,
    /// this module requires they be resolved to actual numbers.
    Global(i32),
}

impl fmt::Display for StgLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StgLoc::Reg(reg) => write!(f, "{reg}"),
            StgLoc::Alias(name) => write!(f, "{name}"),
            StgLoc::Stack(offset) => write!(f, "[$sp{offset:+}]"),
            StgLoc::Global(offset) => write!(f, "[$gp{offset:+}]"),
        }
    }
}

impl TryFrom<StgLoc> for Reg {
    type Error = StgLoc;

    fn try_from(value: StgLoc) -> Result<Self, Self::Error> {
        match value {
            StgLoc::Reg(reg) => Ok(reg),
            other => Err(other),
        }
    }
}

impl From<Reg> for StgLoc {
    fn from(reg: Reg) -> Self {
        Self::Reg(reg)
    }
}

type AsmInstr = instr::Instr<StgLoc, i32>;

#[derive(Debug, Clone)]
enum AsmStmt {
    Instr(AsmInstr),
    Label(String),
}

impl AsmStmt {
    fn defs_and_uses(&self, defs: &mut Vec<StgLoc>, uses: &mut Vec<StgLoc>) {
        match self {
            AsmStmt::Instr(instr) => {
                defs_and_uses(instr, defs, uses);
            }
            AsmStmt::Label(_) => {}
        }
    }
}

/// - `defs` are storage locations that would be overwritten by the instruction `self`.
/// - `uses` are storage locations that would need to be read from by the instruction `self`.
fn defs_and_uses(instr: &AsmInstr, defs: &mut impl Extend<StgLoc>, uses: &mut impl Extend<StgLoc>) {
    match instr {
        Instr::O { opcode } => match opcode {
            OpcodeOp::HALT | OpcodeOp::NOP | OpcodeOp::INRE | OpcodeOp::INRD | OpcodeOp::KRET => {}
        },

        Instr::A { opcode, offset: _ } => match opcode {
            OpcodeAddr::J => {}
        },

        Instr::I { opcode, imm10: _ } => match opcode {
            OpcodeImm::EXN | OpcodeImm::KCALL => {}
        },

        Instr::R { opcode, reg } => match opcode {
            OpcodeReg::JR => uses.extend([reg.clone()]),
            OpcodeReg::MVLO | OpcodeReg::MVHI => defs.extend([reg.clone()]),
        },

        Instr::RI {
            opcode,
            reg,
            imm: _,
        } => match opcode {
            OpcodeRegImm::JAL => {
                let link_reg = reg;
                defs.extend([link_reg.clone()]);
                defs.extend(Reg::CALLER_SAVED.iter().map(|&r| r.into()));
            }
            OpcodeRegImm::LI => defs.extend([reg.clone()]),
            OpcodeRegImm::BT | OpcodeRegImm::BF => uses.extend([reg.clone()]),
        },

        Instr::RR { opcode, reg1, reg2 } => match opcode {
            OpcodeRegReg::JRAL => {
                let (link_reg, jump_addr_reg) = (reg1, reg2);

                defs.extend([link_reg.clone()]);
                defs.extend(Reg::CALLER_SAVED.iter().map(|&r| r.into()));

                uses.extend([jump_addr_reg.clone()]);
            }
            OpcodeRegReg::MV
            | OpcodeRegReg::NOT
            | OpcodeRegReg::NEG
            | OpcodeRegReg::SEB
            | OpcodeRegReg::TEZ
            | OpcodeRegReg::TNZ => {
                let (rd, rs) = (reg1, reg2);
                defs.extend([rd.clone()]);
                uses.extend([rs.clone()]);
            }
            OpcodeRegReg::MUL | OpcodeRegReg::MULU | OpcodeRegReg::DIV | OpcodeRegReg::DIVU => {
                todo!("How to handle $LO/$HI regs?")
            }
        },

        Instr::RRR {
            opcode,
            reg1: rd,
            reg2: rs,
            reg3: rt,
        } => match opcode {
            OpcodeRegRegReg::ADD
            | OpcodeRegRegReg::SUB
            | OpcodeRegRegReg::OR
            | OpcodeRegRegReg::XOR
            | OpcodeRegRegReg::AND
            | OpcodeRegRegReg::ADDU
            | OpcodeRegRegReg::SUBU
            | OpcodeRegRegReg::SHL
            | OpcodeRegRegReg::SHR
            | OpcodeRegRegReg::SHRA
            | OpcodeRegRegReg::TLT
            | OpcodeRegRegReg::TGE
            | OpcodeRegRegReg::TEQ
            | OpcodeRegRegReg::TNE
            | OpcodeRegRegReg::TLTU
            | OpcodeRegRegReg::TGEU => {
                defs.extend([rd.clone()]);
                uses.extend([rs.clone(), rt.clone()]);
            }
        },

        Instr::RRI {
            opcode,
            reg1,
            reg2,
            imm10,
        } => match opcode {
            OpcodeRegRegImm::LW | OpcodeRegRegImm::LBS | OpcodeRegRegImm::LBU => {
                let (rd, src_addr_reg) = (reg1, reg2);
                defs.extend([rd.clone()]);

                match src_addr_reg.clone().try_into() {
                    Ok(Reg::Sp) => {
                        uses.extend([src_addr_reg.clone(), StgLoc::Stack(*imm10)]);
                    }
                    Ok(Reg::Gp) => {
                        uses.extend([src_addr_reg.clone(), StgLoc::Global(*imm10)]);
                    }
                    _ => uses.extend([src_addr_reg.clone()]),
                }
            }
            OpcodeRegRegImm::SW | OpcodeRegRegImm::SB => {
                let (dest_addr_reg, rs) = (reg1, reg2);
                uses.extend([dest_addr_reg.clone(), rs.clone()]);
                match dest_addr_reg.clone().try_into() {
                    Ok(Reg::Sp) => defs.extend([StgLoc::Stack(*imm10)]),
                    Ok(Reg::Gp) => defs.extend([StgLoc::Global(*imm10)]),
                    _ => {}
                }
            }
            OpcodeRegRegImm::ADDI
            | OpcodeRegRegImm::SUBI
            | OpcodeRegRegImm::ORI
            | OpcodeRegRegImm::XORI
            | OpcodeRegRegImm::ANDI => {
                let (rd, rs) = (reg1, reg2);
                defs.extend([rd.clone()]);
                uses.extend([rs.clone()]);
            }
        },
    }
}

type NodeId = usize;

/// Control Flow Graph
struct Cfg {
    stmts: Vec<AsmInstr>,
    edges: BTreeSet<(NodeId, NodeId)>,
    entry: NodeId,
    exits: BTreeSet<NodeId>,
    live_ins_on_entry: BTreeSet<StgLoc>,
    live_outs_on_exit: BTreeSet<StgLoc>,
}

impl Cfg {
    fn new(stmts: Vec<AsmInstr>) -> Self {
        Self {
            stmts,
            edges: BTreeSet::new(),
            entry: 0,
            exits: BTreeSet::new(),
            live_ins_on_entry: [Reg::Ra.into()].into(),
            live_outs_on_exit: [Reg::Ra.into()].into(),
        }
    }

    fn with_edges(mut self, edges: impl Iterator<Item = (NodeId, NodeId)>) -> Self {
        self.edges.extend(edges);
        self
    }

    fn with_entry(mut self, entry: NodeId) -> Self {
        self.entry = entry;
        self
    }

    fn with_exits(mut self, exits: impl IntoIterator<Item = NodeId>) -> Self {
        self.exits.extend(exits);
        self
    }

    fn set_non_void_subr(mut self) -> Self {
        for exit_id in self.exits.iter() {
            self.live_outs_on_exit.insert(StgLoc::Reg(Reg::Rv));
        }
        self
    }

    fn successors(&self, node_id: NodeId) -> impl Iterator<Item = NodeId> + '_ {
        self.edges
            .iter()
            .filter_map(move |(from, to)| if *from == node_id { Some(*to) } else { None })
    }

    fn predecessors(&self, node_id: NodeId) -> impl Iterator<Item = NodeId> + '_ {
        self.edges
            .iter()
            .filter_map(move |(from, to)| if *to == node_id { Some(*from) } else { None })
    }

    fn compute_live_ins_live_outs(
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

            for (id, instr) in self.stmts.iter().enumerate() {
                let ins = live_ins
                    .get(&id)
                    .cloned()
                    .unwrap_or_default()
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                println!("ins:\t{{{ins}}}");
                println!("{id:03}:\t{}", instr);
                let outs = live_outs
                    .get(&id)
                    .cloned()
                    .unwrap_or_default()
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                println!("outs:\t{{{outs}}}",);
                println!();
            }
            eprintln!("-------------------------------------------------------------------");

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
            defs_and_uses(instr, &mut defs_buf, &mut uses_buf);

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

#[test]
fn live_ins_live_outs() {
    /*
    int foo(int n) {
        int z = 10;
        int x = 0;
        int y = 1;
        while (x < n) {
            z = x * 2 + y;
            x++;
            y = x + z;
        }
        return y;
    }
    -------------------
    subr foo_compiled(n => $a0) {
        alias   z => $t0;
        alias   x => $t1;
        alias   y => $t2;
        alias   cond => $t0;

        !li     z, 10;
        !li     x, 0;
        !li     y, 1;

        loop_top:
        !tlt    cond, x, n;
        !bf     cond, &loop_end;
        alias   tmp => $t0;
        !li     tmp, 2;
        !shl    z, x, tmp;
        !add    z, z, y;
        !addi   x, x, 1;
        !add    y, x, z;
        !j      &loop_top;;
        loop_end:

        !mv     $rv, y;
        !jr     $ra;
    }
    */

    let stmts = vec![
        AsmInstr::RI {
            opcode: OpcodeRegImm::LI,
            reg: StgLoc::Alias("z".to_string()),
            imm: 10,
        },
        AsmInstr::RI {
            opcode: OpcodeRegImm::LI,
            reg: StgLoc::Alias("x".to_string()),
            imm: 0,
        },
        AsmInstr::RI {
            opcode: OpcodeRegImm::LI,
            reg: StgLoc::Alias("y".to_string()),
            imm: 1,
        },
        // LOOP_TOP (index 3)
        AsmInstr::RRR {
            opcode: OpcodeRegRegReg::TLT,
            reg1: StgLoc::Alias("cond".to_string()),
            reg2: StgLoc::Alias("x".to_string()),
            reg3: StgLoc::Alias("n".to_string()),
        },
        // Index 4
        AsmInstr::RI {
            opcode: OpcodeRegImm::BF,
            reg: StgLoc::Alias("cond".to_string()),
            imm: 700, // Imm is irrelevant in this example
        },
        AsmInstr::RI {
            opcode: OpcodeRegImm::LI,
            reg: StgLoc::Alias("tmp".to_string()),
            imm: 2,
        },
        AsmInstr::RRR {
            opcode: OpcodeRegRegReg::SHL,
            reg1: StgLoc::Alias("z".to_string()),
            reg2: StgLoc::Alias("x".to_string()),
            reg3: StgLoc::Alias("tmp".to_string()),
        },
        AsmInstr::RRR {
            opcode: OpcodeRegRegReg::ADD,
            reg1: StgLoc::Alias("z".to_string()),
            reg2: StgLoc::Alias("z".to_string()),
            reg3: StgLoc::Alias("y".to_string()),
        },
        AsmInstr::RRI {
            opcode: OpcodeRegRegImm::ADDI,
            reg1: StgLoc::Alias("x".to_string()),
            reg2: StgLoc::Alias("x".to_string()),
            imm10: 1,
        },
        AsmInstr::RRR {
            opcode: OpcodeRegRegReg::ADD,
            reg1: StgLoc::Alias("y".to_string()),
            reg2: StgLoc::Alias("x".to_string()),
            reg3: StgLoc::Alias("z".to_string()),
        },
        AsmInstr::A {
            opcode: OpcodeAddr::J,
            offset: 3,
        },
        // LOOP_END (index 11)
        AsmInstr::RR {
            opcode: OpcodeRegReg::MV,
            reg1: StgLoc::Reg(Reg::Rv),
            reg2: StgLoc::Alias("y".to_string()),
        },
        AsmInstr::R {
            opcode: OpcodeReg::JR,
            reg: StgLoc::Reg(Reg::Ra),
        },
    ];
    let stmts_len = stmts.len();

    let cfg = Cfg::new(stmts)
        .with_edges(
            (1..stmts_len)
                .map(|idx| (idx - 1, idx))
                .chain([(4, 11), (10, 3)]),
        )
        .with_entry(0)
        .with_exits([12])
        .set_non_void_subr();

    let (live_ins, live_outs) = cfg.compute_live_ins_live_outs();

    for (id, instr) in cfg.stmts.iter().enumerate() {
        let ins = live_ins
            .get(&id)
            .cloned()
            .unwrap_or_default()
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        println!("ins:\t{{{ins}}}");
        println!("{id:03}:\t{}", instr);
        let outs = live_outs
            .get(&id)
            .cloned()
            .unwrap_or_default()
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        println!("outs:\t{{{outs}}}",);
        println!();
    }
}
