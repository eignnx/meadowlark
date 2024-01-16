use std::fmt;

#[derive(Debug)]
pub enum Item {
    Const {
        name: String,
        value: u16,
    },
    FnDef {
        name: String,
        args: Vec<AliasBinding>,
        preserve_regs: Vec<Reg>,
        body: Vec<Stmt>,
    },
}

#[derive(Debug)]
pub enum AliasBinding {
    /// Example: `x` (Gets bound to one of `$a0..$a2` if argument, `$t0..$t2` if in fn body)
    ImpliedAlias(Var),
    /// Examples:
    ///    - `local => $s2`
    ///    - `arg => [$sp + 4]`
    ExplicitAlias(Var, LValue),
    // TODO: Structs
    // Struct {
    //     var_name: Var,
    //     struct_name: String,
    //     mappings: Vec<(String, LValue)>,
    // }
}

#[derive(Debug, Clone, Copy)]
pub enum LValue {
    Reg(Reg),
    Mem(Reg, i16),
}

impl fmt::Display for LValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LValue::Reg(reg) => write!(f, "{}", reg),
            LValue::Mem(reg, offset) => write!(f, "{}({})", offset, reg),
        }
    }
}

pub type Var = String;

#[derive(Debug)]
pub enum Stmt {
    Label(String),
    Instr(Instr),
    Restore,
    /// Example: `alias len => $k0;`
    DefAlias(Var, LValue),
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

#[derive(Debug)]
pub struct Instr {
    pub op: String,
    pub args: Vec<Arg>,
}

#[derive(Debug)]
pub enum Arg {
    /// Example: `0xBEEF`
    Uint(u16),
    /// Example: `+23`, `-23`
    Int(i16),
    /// Example: `@some_label`
    Label(String),
    /// Example: `$t0`, `$rv`
    Reg(Reg),
    /// Examples:
    /// - `[$a0]`
    /// - `[$a0 + 4]`
    /// - `[$a0 - 4]`
    /// - `[0xBEEF]`
    Offset(i16, Reg),
    /// Example: `[some_arg]`
    AliasIndirection(Var),
    /// Example: `some_arg`
    Alias(Var),
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
    #[allow(unused)]
    pub fn is_callee_saved(&self) -> bool {
        match self {
            Reg::Ra | Reg::Saved(_) | Reg::Gp | Reg::Sp => true,
            Reg::Zero | Reg::Rv | Reg::Arg(_) | Reg::Temp(_) | Reg::Kernel(_) => false,
        }
    }

    pub fn argument_registers() -> impl Iterator<Item = Reg> {
        (0..3).map(Reg::Arg)
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
