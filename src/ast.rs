use std::collections::BTreeSet;
use std::fmt;

#[derive(Debug)]
pub enum Item {
    Const {
        name: String,
        value: u16,
    },
    FnDef {
        name: String,
        args: Vec<Var>,
        body: Vec<Stmt>,
    },
}

pub type Var = String;

#[derive(Debug)]
pub enum Stmt {
    Label(String),
    Instr(Instr),
    If {
        test_reg: Reg,
        test_cond: Vec<Instr>,
        consequent: Vec<Stmt>,
        alternative: Option<Vec<Stmt>>,
    },
    While {
        test_reg: Reg,
        test_cond: Vec<Instr>,
        body: Vec<Stmt>,
    },
}

impl Stmt {
    pub fn callee_regs_to_save(&self, regs: &mut BTreeSet<Reg>) {
        match self {
            Stmt::Label(_) => {}
            Stmt::Instr(instr) => instr.callee_regs_to_save(regs),
            Stmt::If {
                test_cond,
                consequent,
                alternative,
                ..
            } => {
                // Note: we don't need to save the test_reg because it's an rvalue.
                for instr in test_cond {
                    instr.callee_regs_to_save(regs);
                }
                for stmt in consequent {
                    stmt.callee_regs_to_save(regs);
                }
                if let Some(alternative) = alternative {
                    for stmt in alternative {
                        stmt.callee_regs_to_save(regs);
                    }
                }
            }
            Stmt::While {
                test_cond, body, ..
            } => {
                // Note: we don't need to save the test_reg because it's an rvalue.
                for instr in test_cond {
                    instr.callee_regs_to_save(regs);
                }
                for stmt in body {
                    stmt.callee_regs_to_save(regs);
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Instr {
    pub op: String,
    pub args: Vec<Arg>,
}

impl Instr {
    pub fn callee_regs_to_save(&self, regs: &mut BTreeSet<Reg>) {
        // We only need to look at the first argument, because that's the
        // destination register/memory location.
        let Some(first_arg) = self.args.first() else {
            return;
        };

        if !self.modifies_first_arg() {
            // If the instruction doesn't modify the first argument, then we
            // don't need to consider it further.
            return;
        }

        match first_arg {
            Arg::Uint(_) | Arg::Int(_) | Arg::Label(_) => {}
            Arg::Reg(reg) => {
                if reg.is_callee_saved() {
                    regs.insert(*reg);
                }
            }
            Arg::Offset(_, reg) => {
                if reg.is_callee_saved() {
                    regs.insert(*reg);
                }
            }
        }
    }

    fn modifies_first_arg(&self) -> bool {
        match self.op.as_ref() {
            // These instructions' first arg are read-only.
            "jr" /* <jumpreg> */ | "bt" /* <testreg>, <addr> */ | "bf" /* <testreg>, <addr> */ => false,
            _ => true,
        }
    }
}

#[derive(Debug)]
pub enum Arg {
    Uint(u16),
    Int(i16),
    Label(String),
    Reg(Reg),
    Offset(i16, Reg),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Reg {
    Zero,
    Rv,
    Ra,
    Arg(u8),
    Saved(u8),
    Temp(u8),
    Kernel(u8),
    Gp,
    Sp,
}

impl Reg {
    pub fn is_callee_saved(&self) -> bool {
        match self {
            Reg::Ra | Reg::Saved(_) | Reg::Gp | Reg::Sp => true,
            Reg::Zero | Reg::Rv | Reg::Arg(_) | Reg::Temp(_) | Reg::Kernel(_) => false,
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg::Zero => write!(f, "$zero"),
            Reg::Rv => write!(f, "$rv"),
            Reg::Ra => write!(f, "$ra"),
            Reg::Arg(n) => write!(f, "$a{}", n),
            Reg::Saved(n) => write!(f, "$s{}", n),
            Reg::Temp(n) => write!(f, "$t{}", n),
            Reg::Kernel(n) => write!(f, "$k{}", n),
            Reg::Gp => write!(f, "$gp"),
            Reg::Sp => write!(f, "$sp"),
        }
    }
}
