use std::{
    cell::Cell,
    collections::BTreeMap,
    fmt::{self},
    sync::Mutex,
};

use lark_vm::cpu::regs::Reg;

use crate::ast::{
    const_val::ConstValue,
    lvalue::{Base, Displacement, LValue},
    rvalue::RValue,
};

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

    /// In general, we can't track memory accesses like `sw [$t0], src`. Instead we'll
    /// give them a unique ID every time we encounter one. But we'll retain info on the
    /// **uses** of registers or aliases.
    UniqueMem {
        unique_id: usize,
        reg_used: Option<Reg>,
        alias_used: Option<String>,
    },
}

impl StgLoc {
    pub fn uses(&self) -> Self {
        match self {
            Self::Alias(..)
            | StgLoc::Reg(..)
            | StgLoc::Stack(..)
            | StgLoc::Global(..)
            | StgLoc::Lo
            | StgLoc::Hi => self.clone(),

            StgLoc::UniqueMem {
                reg_used: Some(reg),
                alias_used: None,
                ..
            } => (*reg).into(),

            StgLoc::UniqueMem {
                reg_used: None,
                alias_used: Some(alias),
                ..
            } => Self::Alias(alias.clone()),

            StgLoc::UniqueMem {
                unique_id,
                reg_used: None,
                alias_used: None,
            } => StgLoc::UniqueMem {
                unique_id: *unique_id,
                reg_used: None,
                alias_used: None,
            },

            StgLoc::UniqueMem {
                reg_used: Some(_),
                alias_used: Some(_),
                ..
            } => unreachable!(),
        }
    }
}

static MEM_ACCESS_ID: Mutex<Cell<usize>> = Mutex::new(Cell::new(0));

impl fmt::Display for StgLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StgLoc::Reg(reg) => write!(f, "{reg}"),
            StgLoc::Alias(name) => write!(f, "{name}"),
            StgLoc::Stack(offset) => write!(f, "[$sp{offset:+}]"),
            StgLoc::Global(offset) => write!(f, "[$gp{offset:+}]"),
            StgLoc::Lo => write!(f, "$LO"),
            StgLoc::Hi => write!(f, "$HI"),
            StgLoc::UniqueMem {
                unique_id,
                reg_used,
                alias_used,
            } => match (reg_used, alias_used) {
                (Some(reg), None) => write!(f, "M[#{unique_id}, {reg}]"),
                (None, Some(alias)) => write!(f, "M[#{unique_id}, {alias}]"),
                (None, None) => write!(f, "M[#{unique_id}]"),
                (Some(_), Some(_)) => unreachable!(),
            },
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
    fn interpret_offset(&self, offset: &Displacement) -> Result<i32, String> {
        match offset {
            Displacement::I10(i) => Ok(*i as i32),
            Displacement::Const(name) => self
                .consts
                .get(name)
                .ok_or_else(|| name.clone())
                .and_then(|x| x.evaluate(self.consts).ok_or_else(|| name.clone())),
            Displacement::NegatedConst(name) => self
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
        offset: Option<&Displacement>,
    ) -> Result<StgLoc, String> {
        let offset = if let Some(offset) = offset {
            self.interpret_offset(offset)?
        } else {
            0
        };

        match base {
            Base::Reg(Reg::Sp) => Ok(StgLoc::Stack(offset)),
            Base::Reg(Reg::Gp) => Ok(StgLoc::Global(offset)),
            Base::Reg(reg) => {
                let mut id_lock = MEM_ACCESS_ID.lock().unwrap();
                let id_ref = id_lock.get_mut();
                let unique_id = *id_ref;
                *id_ref += 1;
                Ok(StgLoc::UniqueMem {
                    unique_id,
                    reg_used: Some(*reg),
                    alias_used: None,
                })
            }
            Base::Alias(alias) => {
                let mut id_lock = MEM_ACCESS_ID.lock().unwrap();
                let id_ref = id_lock.get_mut();
                let unique_id = *id_ref;
                *id_ref += 1;
                Ok(StgLoc::UniqueMem {
                    unique_id,
                    reg_used: None,
                    alias_used: Some(alias.clone()),
                })
            }
        }
    }

    pub fn translate(&self, rvalue: &RValue) -> Result<StgLoc, RValueToStgLocError> {
        match rvalue {
            RValue::LValue(LValue::Reg(reg)) => Ok(StgLoc::Reg(*reg)),
            RValue::LValue(LValue::Alias(name)) => Ok(StgLoc::Alias(name.clone())),
            RValue::LValue(LValue::Indirection { base, displacement }) => self
                .interpret_indirection_base_offset(base, displacement.as_ref())
                .map_err(RValueToStgLocError::ConstAliasUndefined),
            RValue::Uint(_)
            | RValue::Int(_)
            | RValue::Char(_)
            | RValue::String(_)
            | RValue::Label(_)
            | RValue::ConstAlias(_) => Err(RValueToStgLocError::NonStgLocRValue(rvalue.clone())),
        }
    }
}
