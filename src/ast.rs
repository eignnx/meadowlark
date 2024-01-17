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
    Directive(Directive),
}

#[derive(Debug)]
pub enum AliasBinding {
    /// Example: `x` (Gets bound to one of `$a0..$a2` if argument).
    /// Only valid for function arguments. I want users to know which registers
    /// and stack offsets are in use.
    ImplicitAlias(Var),
    /// Examples:
    ///    - `local => $s2`
    ///    - `arg => [$sp + 4]`
    ExplicitAlias(Var, LValue),

    /// Examples:
    ///     - `my_point => Point { x => $a0, y => $a1 }`
    ///     - `my_point => { x => [$sp + 4], y => [$sp + 6] }`
    Struct {
        var_name: Var,
        // /// If `Some`, refers to a previously defined struct item.
        // struct_name: Option<String>,
        field_bindings: Vec<AliasBinding>,
    },
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
    DefAlias(AliasBinding),
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
    /// Example: `'a'`, `\n`
    Char(u8),
    /// Example: `"hello\n"`
    String(String),
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
    /// Examples:
    ///     - `[some_arg]`
    ///     - `[some_arg + 4]`
    AliasIndirection(Var, Option<i16>),
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

#[derive(Debug, Clone)]
pub enum Directive {
    /// Tells the assembler to place the following code at the given address.
    /// Example: `[[addr(0x0E00)]]`
    Addr(u16),
}
