use core::fmt;
use std::{collections::BTreeMap, str::FromStr};

use lark_vm::cpu::{
    instr::{self, ops::*},
    regs::Reg,
};

use crate::{
    ast::{
        const_val::{ConstEvalError, ConstValue},
        lvalue::LValue,
        rvalue::RValue,
    },
    compile::CodeGen,
};

use super::{
    cfg::Link,
    stg_loc::{RValueToStgLocError, RValueToStgLocTranslator, StgLoc},
};

pub enum ImmEvalError {
    UnboundConstAlias(String),
    MaxEvalDepthExceeded,
    LabelNotConvertableToInt,
}

impl From<ConstEvalError> for ImmEvalError {
    fn from(value: ConstEvalError) -> Self {
        match value {
            ConstEvalError::UndefinedAlias(name) => Self::UnboundConstAlias(name),
            ConstEvalError::MaxEvalDepthExceeded => Self::MaxEvalDepthExceeded,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Imm {
    Uint(u16),
    Int(i16),
    Char(u8),
    ConstAlias(String),
    Label(String),
}

impl Imm {
    pub fn try_from_rvalue(
        codegen: &mut CodeGen,
        current_opcode: &str,
        rvalue: &RValue,
    ) -> Result<Imm, InstrTranslationErr> {
        match rvalue {
            RValue::Uint(u) => Ok(Imm::Uint(*u)),
            RValue::Int(i) => Ok(Imm::Int(*i)),
            RValue::Char(c) => Ok(Imm::Char(*c)),
            RValue::ConstAlias(name) => Ok(Imm::ConstAlias(name.clone())),
            RValue::Label(lbl) => Ok(Imm::Label(lbl.clone())),
            RValue::String(s) => {
                let label = codegen.get_or_insert_string_literal(s);
                Ok(Imm::Label(label.to_owned()))
            }
            RValue::LValue(lval) => Err(InstrTranslationErr::WrongArgType {
                opcode: current_opcode.to_owned(),
                expected: "rvalue convertable to immediate value",
                got: format!("lvalue `{lval}`"),
            }),
        }
    }

    pub fn try_to_i32(&self, consts: &BTreeMap<String, ConstValue>) -> Result<i32, ImmEvalError> {
        match self {
            Imm::Uint(u) => Ok(*u as i32),
            Imm::Int(i) => Ok(*i as i32),
            Imm::Char(byte) => Ok(*byte as i32),
            Imm::ConstAlias(name) => match consts
                .get(name)
                .ok_or_else(|| ConstEvalError::UndefinedAlias(name.clone()))?
            {
                ConstValue::Uint(u) => Ok(*u as i32),
                ConstValue::Int(i) => Ok(*i as i32),
                ConstValue::Char(c) => Ok(*c as i32),
                value @ ConstValue::ConstAlias(..) => value.evaluate(consts).map_err(Into::into),
                ConstValue::BinOp(lhs, op, rhs) => {
                    let lhs = lhs.evaluate(consts)?;
                    let rhs = rhs.evaluate(consts)?;
                    Ok(op.eval(lhs, rhs))
                }
            },
            Imm::Label(_) => todo!(),
        }
    }
}

impl fmt::Display for Imm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Imm::Uint(u) => write!(f, "{u}"),
            Imm::Int(i) => write!(f, "{i}"),
            Imm::Char(c) => write!(f, "{:?}", *c as char),
            Imm::ConstAlias(name) => write!(f, "{name}"),
            Imm::Label(lbl) => write!(f, "&{lbl}"),
        }
    }
}

pub type CheckInstr = instr::Instr<StgLoc, Imm>;

pub enum InstrTranslationErr {
    WrongArgCount {
        opcode: String,
        expected: usize,
        got: usize,
    },
    WrongArgType {
        opcode: String,
        expected: &'static str,
        got: String,
    },
    UnknownOp {
        opcode: String,
    },
    UnboundConstAlias {
        opcode: String,
        name: String,
    },
    UnboundAlias {
        opcode: String,
        name: String,
    },
    MaxConstEvalDepthExceeded {
        opcode: String,
    },
}

impl fmt::Display for InstrTranslationErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstrTranslationErr::WrongArgCount {
                opcode,
                expected,
                got,
            } => {
                write!(f, "wrong number of arguments for instr `{opcode}`: expected {expected}, got {got}")
            }
            InstrTranslationErr::WrongArgType {
                opcode,
                expected,
                got,
            } => {
                write!(
                    f,
                    "wrong argument type for instr `{opcode}`: expected {expected}, got {got}"
                )
            }
            InstrTranslationErr::UnknownOp { opcode } => {
                write!(f, "unknown instruction `{opcode}`")
            }
            InstrTranslationErr::UnboundConstAlias { opcode, name } => {
                write!(f, "unbound const alias `{name}` in instr `{opcode}`")
            }
            InstrTranslationErr::UnboundAlias { opcode, name } => {
                write!(f, "unbound alias `{name}` in instr `{opcode}`")
            }
            InstrTranslationErr::MaxConstEvalDepthExceeded { opcode } => {
                write!(f, "max const eval depth exceeded in instr `{opcode}`")
            }
        }
    }
}

impl InstrTranslationErr {
    fn from_rvalue_err(op_name: &str, e: RValueToStgLocError) -> Self {
        match e {
            RValueToStgLocError::ConstAliasUndefined(name) => {
                InstrTranslationErr::UnboundConstAlias {
                    opcode: op_name.into(),
                    name,
                }
            }
            RValueToStgLocError::NonStgLocRValue(rv) => InstrTranslationErr::WrongArgType {
                opcode: op_name.into(),
                expected: "rvalue representing a register",
                got: rv.to_string(),
            },
            RValueToStgLocError::MaxEvalDepthExceeded => {
                InstrTranslationErr::MaxConstEvalDepthExceeded {
                    opcode: op_name.into(),
                }
            }
        }
    }
}

pub struct CheckInstrTranslator<'a> {
    codegen: &'a mut CodeGen,
}

impl<'a> CheckInstrTranslator<'a> {
    pub fn new(codegen: &'a mut CodeGen) -> CheckInstrTranslator<'a> {
        CheckInstrTranslator { codegen }
    }

    fn try_rvalue_as_reg(
        &self,
        current_opcode: &str,
        rv: &RValue,
    ) -> Result<Reg, InstrTranslationErr> {
        match rv {
            RValue::LValue(LValue::Reg(reg)) => Ok(*reg),
            RValue::LValue(LValue::Alias(name)) => match self.codegen.var_aliases.get(name) {
                Some(LValue::Reg(reg)) => Ok(*reg),
                Some(LValue::Alias(name)) => {
                    todo!("aliases to aliases are not yet supported. `{name}`")
                }
                Some(other) => Err(InstrTranslationErr::WrongArgType {
                    opcode: current_opcode.into(),
                    expected: "register",
                    got: other.to_string(),
                }),
                None => Err(InstrTranslationErr::UnboundAlias {
                    opcode: current_opcode.into(),
                    name: name.to_owned(),
                }),
            },
            _ => Err(InstrTranslationErr::WrongArgType {
                opcode: current_opcode.into(),
                expected: "register",
                got: rv.to_string(),
            }),
        }
    }

    fn try_rvalue_as_imm(
        &mut self,
        current_opcode: &str,
        rv: &RValue,
    ) -> Result<Imm, InstrTranslationErr> {
        Imm::try_from_rvalue(self.codegen, current_opcode, rv)
    }

    fn try_rvalue_to_stg_loc(
        &self,
        current_opcode: &str,
        rv: &RValue,
    ) -> Result<StgLoc, InstrTranslationErr> {
        RValueToStgLocTranslator::new(&self.codegen.consts)
            .translate(rv)
            .map_err(|e| InstrTranslationErr::from_rvalue_err(current_opcode, e))
    }

    pub fn translate(
        &mut self,
        op_name: &str,
        args: &[RValue],
        links: &mut impl Extend<Link<'static>>,
    ) -> Result<CheckInstr, InstrTranslationErr> {
        if let Ok(opcode) = OpcodeOp::from_str(op_name) {
            opcode.links(links, args);
            match args {
                [] => return Ok(CheckInstr::O { opcode }),
                _ => {
                    return Err(InstrTranslationErr::WrongArgCount {
                        opcode: op_name.into(),
                        expected: 0,
                        got: args.len(),
                    })
                }
            }
        }

        if let Ok(opcode) = OpcodeAddr::from_str(op_name) {
            opcode.links(links, args);
            match args {
                [offset] => {
                    let offset = self.try_rvalue_as_imm(op_name, offset)?;
                    return Ok(CheckInstr::A { opcode, offset });
                }
                _ => {
                    return Err(InstrTranslationErr::WrongArgCount {
                        opcode: op_name.into(),
                        expected: 1,
                        got: args.len(),
                    })
                }
            }
        }

        if let Ok(opcode) = OpcodeReg::from_str(op_name) {
            opcode.links(links, args);
            match args {
                [reg] => {
                    let reg = self.try_rvalue_to_stg_loc(op_name, reg)?;
                    return Ok(CheckInstr::R { opcode, reg });
                }
                _ => {
                    return Err(InstrTranslationErr::WrongArgCount {
                        opcode: op_name.into(),
                        expected: 1,
                        got: args.len(),
                    })
                }
            }
        }

        if let Ok(opcode) = OpcodeImm::from_str(op_name) {
            opcode.links(links, args);
            match args {
                [imm] => {
                    let imm = self.try_rvalue_as_imm(op_name, imm)?;
                    // TODO: Check that imm is 10 bit int.
                    return Ok(CheckInstr::I { opcode, imm10: imm });
                }
                _ => {
                    return Err(InstrTranslationErr::WrongArgCount {
                        opcode: op_name.into(),
                        expected: 1,
                        got: args.len(),
                    })
                }
            }
        }

        if let Ok(opcode) = OpcodeRegImm::from_str(op_name) {
            opcode.links(links, args);
            match args {
                [reg, imm] => {
                    let reg = self.try_rvalue_to_stg_loc(op_name, reg)?;
                    let imm = self.try_rvalue_as_imm(op_name, imm)?;
                    return Ok(CheckInstr::RI { opcode, reg, imm });
                }
                _ => {
                    return Err(InstrTranslationErr::WrongArgCount {
                        opcode: op_name.into(),
                        expected: 2,
                        got: args.len(),
                    })
                }
            }
        }

        if let Ok(opcode) = OpcodeRegReg::from_str(op_name) {
            opcode.links(links, args);
            match args {
                [reg1, reg2] => {
                    let reg1 = self.try_rvalue_to_stg_loc(op_name, reg1)?;
                    let reg2 = self.try_rvalue_to_stg_loc(op_name, reg2)?;
                    return Ok(CheckInstr::RR { opcode, reg1, reg2 });
                }
                _ => {
                    return Err(InstrTranslationErr::WrongArgCount {
                        opcode: op_name.into(),
                        expected: 2,
                        got: args.len(),
                    })
                }
            }
        }

        if let Ok(opcode) = OpcodeRegRegImm::from_str(op_name) {
            opcode.links(links, args);
            match args {
                [reg1, reg2, imm] => {
                    let reg1 = RValueToStgLocTranslator::new(&self.codegen.consts)
                        .translate(reg1)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;

                    let reg2 = RValueToStgLocTranslator::new(&self.codegen.consts)
                        .translate(reg2)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;

                    let imm = self.try_rvalue_as_imm(op_name, imm)?;
                    // TODO: Check that imm is 10 bit int.

                    return Ok(CheckInstr::RRI {
                        opcode,
                        reg1,
                        reg2,
                        imm10: imm,
                    });
                }

                // If it looks like theres only two arguments to a store instruction, there's really
                // 3, one is probably just an indirection.
                [ind @ RValue::LValue(LValue::Indirection { .. }), src_reg]
                    if matches!(opcode, OpcodeRegRegImm::SW | OpcodeRegRegImm::SB) =>
                {
                    let ind = self.try_rvalue_to_stg_loc(op_name, ind)?;
                    let src_reg = self.try_rvalue_to_stg_loc(op_name, src_reg)?;

                    return Ok(CheckInstr::RRI {
                        opcode,
                        reg1: ind,
                        reg2: src_reg,
                        imm10: Imm::Uint(0), // displacement
                    });
                }

                // If it looks like theres only two arguments to a load instruction, there's really
                // 3, one is probably just an indirection.
                [dst_reg, ind @ RValue::LValue(LValue::Indirection { .. })]
                    if matches!(
                        opcode,
                        OpcodeRegRegImm::LW | OpcodeRegRegImm::LBS | OpcodeRegRegImm::LBU
                    ) =>
                {
                    let dst_reg = self.try_rvalue_to_stg_loc(op_name, dst_reg)?;
                    let ind = self.try_rvalue_to_stg_loc(op_name, ind)?;

                    return Ok(CheckInstr::RRI {
                        opcode,
                        reg1: dst_reg,
                        reg2: ind,
                        imm10: Imm::Uint(0), // displacement
                    });
                }

                _ => {
                    return Err(InstrTranslationErr::WrongArgCount {
                        opcode: op_name.into(),
                        expected: 3,
                        got: args.len(),
                    })
                }
            }
        }

        if let Ok(opcode) = OpcodeRegRegReg::from_str(op_name) {
            opcode.links(links, args);
            match args {
                [reg1, reg2, reg3] => {
                    return Ok(CheckInstr::RRR {
                        opcode,
                        reg1: self.try_rvalue_to_stg_loc(op_name, reg1)?,
                        reg2: self.try_rvalue_to_stg_loc(op_name, reg2)?,
                        reg3: self.try_rvalue_to_stg_loc(op_name, reg3)?,
                    });
                }
                _ => {
                    return Err(InstrTranslationErr::WrongArgCount {
                        opcode: op_name.into(),
                        expected: 3,
                        got: args.len(),
                    })
                }
            }
        }

        Err(InstrTranslationErr::UnknownOp {
            opcode: op_name.to_string(),
        })
    }
}

trait Links {
    fn links(self, links: &mut impl Extend<Link<'static>>, args: &[RValue]);
}

impl Links for OpcodeOp {
    fn links(self, links: &mut impl Extend<Link<'static>>, _args: &[RValue]) {
        match self {
            OpcodeOp::HALT => {} // NO LINKS AT ALL FOR HALT!
            OpcodeOp::NOP | OpcodeOp::INRE | OpcodeOp::INRD | OpcodeOp::KRET => {
                links.extend([Link::ToNext]);
            }
        }
    }
}

impl Links for OpcodeAddr {
    fn links(self, links: &mut impl Extend<Link<'static>>, args: &[RValue]) {
        match self {
            OpcodeAddr::J => {
                let label = match args {
                    [RValue::Label(label)] => label.clone(),
                    _ => unreachable!(),
                };
                links.extend([Link::ToNext, Link::JumpToLabel(label.into())]);
            }
        }
    }
}

impl Links for OpcodeImm {
    fn links(self, links: &mut impl Extend<Link<'static>>, _args: &[RValue]) {
        match self {
            OpcodeImm::EXN => {
                // TODO: `exn DIV_BY_ZERO` should probably be assumed to never return.
                // Oh well, this is a conservative guess in any case.
                links.extend([Link::ToNext]);
            }
            OpcodeImm::KCALL => links.extend([Link::ToNext]),
        }
    }
}

impl Links for OpcodeReg {
    fn links(self, links: &mut impl Extend<Link<'static>>, _args: &[RValue]) {
        match self {
            // `jr` is usually the `return` instruction. So it never proceeds to any other
            // instruction within the same subroutine call.
            OpcodeReg::JR => {}
            OpcodeReg::MVLO | OpcodeReg::MVHI => links.extend([Link::ToNext]),
        }
    }
}

impl Links for OpcodeRegImm {
    fn links(self, links: &mut impl Extend<Link<'static>>, args: &[RValue]) {
        match self {
            OpcodeRegImm::LI => links.extend([Link::ToNext]),
            OpcodeRegImm::BT | OpcodeRegImm::BF => {
                let label = match args {
                    [_test_reg, RValue::Label(label)] => label.clone(),
                    _ => unreachable!(),
                };
                links.extend([Link::ToNext, Link::JumpToLabel(label.into())]);
            }
            // `jal` is (usually) the `call` instruction. So will very likely proceed to the next
            // instruction after returning from that subroutine call.
            OpcodeRegImm::JAL => links.extend([Link::ToNext]),
        }
    }
}

impl Links for OpcodeRegReg {
    fn links(self, links: &mut impl Extend<Link<'static>>, args: &[RValue]) {
        match self {
            OpcodeRegReg::JRAL => {
                // `jalr` is (usually) `call_indirect` instruction. So will very likely proceed to the next
                // instruction after returning from that subroutine call.
                links.extend([Link::ToNext]);
            }
            OpcodeRegReg::MV
            | OpcodeRegReg::NOT
            | OpcodeRegReg::NEG
            | OpcodeRegReg::SEB
            | OpcodeRegReg::MUL
            | OpcodeRegReg::DIV
            | OpcodeRegReg::MULU
            | OpcodeRegReg::DIVU => {
                links.extend([Link::ToNext]);
            }

            OpcodeRegReg::TEZ | OpcodeRegReg::TNZ => {
                let label = match args {
                    [RValue::Label(label)] => label.clone(),
                    _ => unreachable!(),
                };
                links.extend([Link::ToNext, Link::JumpToLabel(label.into())]);
            }
        }
    }
}

impl Links for OpcodeRegRegImm {
    fn links(self, links: &mut impl Extend<Link<'static>>, _args: &[RValue]) {
        match self {
            OpcodeRegRegImm::LW
            | OpcodeRegRegImm::LBS
            | OpcodeRegRegImm::LBU
            | OpcodeRegRegImm::SW
            | OpcodeRegRegImm::SB
            | OpcodeRegRegImm::ADDI
            | OpcodeRegRegImm::SUBI
            | OpcodeRegRegImm::ORI
            | OpcodeRegRegImm::XORI
            | OpcodeRegRegImm::ANDI => links.extend([Link::ToNext]),
        }
    }
}

impl Links for OpcodeRegRegReg {
    fn links(self, links: &mut impl Extend<Link<'static>>, _args: &[RValue]) {
        match self {
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
            | OpcodeRegRegReg::TGEU => links.extend([Link::ToNext]),
        }
    }
}
