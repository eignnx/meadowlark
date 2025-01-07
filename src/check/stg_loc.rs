use std::{
    collections::BTreeMap,
    fmt::{self},
};

use lark_vm::cpu::regs::Reg;

use crate::ast::{Base, ConstValue, LValue, Offset, RValue};

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

pub struct RValueToStgLocTranslator<'a> {
    consts: &'a BTreeMap<String, ConstValue>,
}
pub enum RValueToStgLocError {
    ConstAliasUndefined(String),
    NonStgLocRValue(RValue),
}

impl<'a> RValueToStgLocTranslator<'a> {
    pub fn new(consts: &'a BTreeMap<String, ConstValue>) -> Self {
        Self { consts }
    }

    /// Returns `Err(const_name)` if the const alias is undefined.
    fn interpret_offset(&self, offset: &Offset) -> Result<i32, String> {
        match offset {
            Offset::I10(i) => Ok(*i as i32),
            Offset::Const(name) => self
                .consts
                .get(name)
                .ok_or_else(|| name.clone())
                .and_then(|x| x.evaluate(self.consts).ok_or_else(|| name.clone())),
            Offset::NegatedConst(name) => self
                .consts
                .get(name)
                .ok_or_else(|| name.clone())
                .and_then(|x| x.evaluate(self.consts).ok_or_else(|| name.clone()))
                .map(|x| -x),
        }
    }

    fn interpret_indirection_base_offset(
        &self,
        base: &Base,
        offset: Option<&Offset>,
    ) -> Result<StgLoc, String> {
        let offset = if let Some(offset) = offset {
            self.interpret_offset(offset)?
        } else {
            0
        };

        match base {
            Base::Reg(Reg::Sp) => Ok(StgLoc::Stack(offset)),
            Base::Reg(Reg::Gp) => Ok(StgLoc::Global(offset)),
            Base::Reg(_) | Base::Alias(_) => todo!("translate indirection base: {base:?}"),
        }
    }

    pub fn translate(&self, rvalue: &RValue) -> Result<StgLoc, RValueToStgLocError> {
        match rvalue {
            RValue::Alias(name) => Ok(StgLoc::Alias(name.clone())),
            RValue::LValue(LValue::Reg(reg)) => Ok(StgLoc::Reg(*reg)),
            RValue::LValue(LValue::Indirection { base, offset }) => {
                let base = base.clone().unwrap_or(Base::Reg(Reg::Zero));
                self.interpret_indirection_base_offset(&base, offset.as_ref())
                    .map_err(RValueToStgLocError::ConstAliasUndefined)
            }
            RValue::Uint(_)
            | RValue::Int(_)
            | RValue::Char(_)
            | RValue::String(_)
            | RValue::Label(_)
            | RValue::ConstAlias(_) => Err(RValueToStgLocError::NonStgLocRValue(rvalue.clone())),
        }
    }
}
