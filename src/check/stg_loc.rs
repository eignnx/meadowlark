use std::fmt;

use lark_vm::cpu::regs::Reg;

/// Represents a **Storage Location**.
///
/// These will only be used for intra-procedural analysis (within one subroutine), so hopefully
/// `$sp` and `$gp` can be assumed to be constant throughout.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StgLoc {
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

    /// The CPU register `$LO`. Holds the lower two bytes of a multiplication result.
    /// Holds the quotient after a division.
    Lo,
    /// The CPU register `$HI`. Holds the upper two bytes of a multiplication result.
    /// Holds the remainder after a division.
    Hi,
}

impl fmt::Display for StgLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StgLoc::Reg(reg) => write!(f, "{reg}"),
            StgLoc::Alias(name) => write!(f, "{name}"),
            StgLoc::Stack(offset) => write!(f, "[$sp{offset:+}]"),
            StgLoc::Global(offset) => write!(f, "[$gp{offset:+}]"),
            StgLoc::Lo => write!(f, "$LO"),
            StgLoc::Hi => write!(f, "$HI"),
        }
    }
}

impl From<Reg> for StgLoc {
    fn from(reg: Reg) -> Self {
        Self::Reg(reg)
    }
}

impl TryFrom<StgLoc> for Reg {
    type Error = StgLoc;

    fn try_from(value: StgLoc) -> Result<Self, Self::Error> {
        match value {
            StgLoc::Reg(reg) => Ok(reg),
            _ => Err(value),
        }
    }
}
