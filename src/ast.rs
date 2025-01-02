use std::fmt;

#[derive(Debug)]
pub enum Item {
    Const {
        name: String,
        value: u16,
    },
    /// `subr` is "subroutine".
    SubrDef {
        name: String,
        args: Vec<AliasBinding>,
        preserve_regs: Vec<Reg>,
        /// True if the function is an interrupt service routine ("isr").
        is_isr: bool,
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
        /// If `Some`, refers to a previously defined struct item.
        /// struct_name: Option<String>,
        field_bindings: Vec<AliasBinding>,
    },
}

#[derive(Debug, Clone)]
pub enum Base {
    Reg(Reg),
    AliasOrConst(Var),
}

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Offset::I10(i) => write!(f, "{}", i),
            Offset::Const(var) => write!(f, "+{}", var),
            Offset::NegatedConst(var) => write!(f, "-{}", var),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Offset {
    I10(i16),
    Const(Var),
    NegatedConst(Var),
}

impl fmt::Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Base::Reg(reg) => write!(f, "{}", reg),
            Base::AliasOrConst(var) => write!(f, "{}", var),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LValue {
    /// Example: `$t0`, `$rv`
    Reg(Reg),

    /// # Examples
    ///
    /// | Syntax                | Equivalent          |
    /// |:----------------------|--------------------:|
    /// | `[$a0]`               | `0($a0)`            |
    /// | `[0xBEEF]`            | `0xBEEF($zero)`     |
    /// | `[some_alias]`        | `0(some_alias)`     |
    /// | `[$a0 + 4]`           | `4($a0)`            |
    /// | `[$a0 - 4]`           | `-4($a0)`           |
    /// | `[$a0 - MY_CONSTANT]` | `-MY_CONSTANT($a0)` |
    /// | `[some_arg + 4]`      | `4(some_arg)`       |
    Indirection {
        base: Option<Base>,
        offset: Option<Offset>,
    },
}

impl fmt::Display for LValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LValue::Reg(reg) => write!(f, "{}", reg),
            LValue::Indirection { base, offset } => {
                write!(f, "[")?;
                if let Some(base) = base {
                    write!(f, "{}", base)?;
                }
                if let Some(offset) = offset {
                    write!(f, " + {}", offset)?;
                }
                write!(f, "]")
            }
        }
    }
}

pub type Var = String;

#[derive(Debug)]
pub enum Stmt {
    Label(String),
    Instr(Instr),
    Restore,
    /// Explicit preserve statement.
    Preserve(Vec<Reg>),
    /// Example: `alias len => $k0;`
    DefAlias(AliasBinding),
    If {
        test_reg: RValue,
        test_cond: Vec<Instr>,
        consequent: Vec<Stmt>,
        alternative: Option<Vec<Stmt>>,
    },
    While {
        test_arg: RValue,
        test_cond: Vec<Instr>,
        body: Vec<Stmt>,
    },
}

#[derive(Debug)]
pub struct Instr {
    pub op: String,
    pub args: Vec<RValue>,
}

#[derive(Debug, Clone)]
pub enum RValue {
    /// Example: `0xBEEF`, `123`, `0b00001111`, `0o2375`
    Uint(u16),

    /// Example: `+23`, `-23`
    Int(i16),

    /// Example: `'a'`, `'\n'`
    Char(u8),

    /// Example: `"hello\n"`
    String(String),

    /// Example: `@some_label`
    Label(String),

    /// Example: `some_arg`
    Alias(Var),

    LValue(LValue),
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

    /// Tells the assembler to emit the given data bytes.
    /// Example: `[[data(0xe4, VTTY_ADDR, 0xaf)]]`
    Data(Vec<RValue>),
}
