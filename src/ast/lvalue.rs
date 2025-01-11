use core::fmt;

use lark_vm::cpu::regs::Reg;

use super::Var;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LValue {
    /// Example: `$t0`, `$rv`
    Reg(Reg),

    /// # Examples
    ///
    /// | Meadowlark Syntax     | LARK Assembly       | AST                                                                                      |
    /// |:----------------------|--------------------:|:-----------------------------------------------------------------------------------------|
    /// | `[$a0]`               | `0($a0)`            | `Indirection { base: $a0, displacement: None }`                                          |
    /// | `[some_alias]`        | `0(some_alias)`     | `Indirection { base: some_alias, displacement: None }`                                   |
    /// | `[$a0 + 4]`           | `4($a0)`            | `Indirection { base: $a0, displacement: Some(Displacement::I10(4)) }`                    |
    /// | `[$a0 - 4]`           | `-4($a0)`           | `Indirection { base: $a0, displacement: Some(Displacement::I10(-4)) }`                   |
    /// | `[$a0 - MY_CONSTANT]` | `-MY_CONSTANT($a0)` | `Indirection { base: $a0, displacement: Some(Displacement::NegatedConst(MY_CONSTANT)) }` |
    /// | `[some_arg + 4]`      | `4(some_arg)`       | `Indirection { base: some_arg, displacement: Some(Displacement::I10(4)) }`               |
    Indirection {
        base: Base,
        displacement: Option<Displacement>,
    },
}

impl LValue {
    pub fn is_arg_reg(&self) -> bool {
        match self {
            LValue::Reg(r) => r.is_argument(),
            _ => false,
        }
    }
}

impl fmt::Display for LValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LValue::Reg(reg) => write!(f, "{}", reg),
            LValue::Indirection {
                base,
                displacement: offset,
            } => {
                write!(f, "[")?;
                write!(f, "{}", base)?;
                if let Some(offset) = offset {
                    write!(f, " + {}", offset)?;
                }
                write!(f, "]")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Base {
    Reg(Reg),
    Alias(Var),
}

impl fmt::Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Base::Reg(reg) => write!(f, "{}", reg),
            Base::Alias(var) => write!(f, "{}", var),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Displacement {
    I10(i16),
    Const(Var),
    NegatedConst(Var),
}

impl fmt::Display for Displacement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Displacement::I10(i) => write!(f, "{}", i),
            Displacement::Const(var) => write!(f, "+{}", var),
            Displacement::NegatedConst(var) => write!(f, "-{}", var),
        }
    }
}
