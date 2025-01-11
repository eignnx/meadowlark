use core::fmt;
use std::{collections::BTreeMap, str::FromStr};

use lark_vm::cpu::instr::{self, ops::*};

use crate::ast::{const_val::ConstValue, LValue, RValue};

use super::{
    cfg::Link,
    stg_loc::{RValueToStgLocError, RValueToStgLocTranslator, StgLoc},
};

pub type CheckInstr = instr::Instr<StgLoc, i32>;

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
                expected: "register",
                got: rv.to_string(),
            },
        }
    }
}

pub struct CheckInstrTranslator<'a> {
    consts: &'a BTreeMap<String, ConstValue>,
}

impl CheckInstrTranslator<'_> {
    pub fn new(consts: &BTreeMap<String, ConstValue>) -> CheckInstrTranslator<'_> {
        CheckInstrTranslator { consts }
    }

    fn try_rvalue_as_i32(
        &self,
        current_opcode: &str,
        rv: &RValue,
    ) -> Result<i32, InstrTranslationErr> {
        match rv {
            RValue::ConstAlias(name) => {
                if let Some(constexpr) = self.consts.get(name) {
                    constexpr.evaluate(self.consts).ok_or_else(|| {
                        InstrTranslationErr::UnboundConstAlias {
                            opcode: current_opcode.into(),
                            name: name.clone(),
                        }
                    })
                } else {
                    Err(InstrTranslationErr::UnboundConstAlias {
                        opcode: current_opcode.into(),
                        name: name.clone(),
                    })
                }
            }
            RValue::Int(i) => Ok(*i as i32),
            RValue::Uint(u) => Ok(*u as i32),
            RValue::Label(_) => Ok(0x00CAFE00),
            _ => Err(InstrTranslationErr::WrongArgType {
                opcode: current_opcode.into(),
                expected: "constant",
                got: rv.to_string(),
            }),
        }
    }

    pub fn translate(
        &self,
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
                    let offset = self.try_rvalue_as_i32(op_name, offset)?;
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
                    let reg = RValueToStgLocTranslator::new(self.consts)
                        .translate(reg)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;
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
                    let imm = self.try_rvalue_as_i32(op_name, imm)?;
                    if !(-512..1024).contains(&imm) {
                        return Err(InstrTranslationErr::WrongArgType {
                            opcode: op_name.into(),
                            expected: "10-bit integer",
                            got: imm.to_string(),
                        });
                    }
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
                [reg, _imm] => {
                    let reg = RValueToStgLocTranslator::new(self.consts)
                        .translate(reg)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;
                    let imm = 0x00FACADE; //self.try_rvalue_as_i32(op_name, imm)?;
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
                    let reg1 = RValueToStgLocTranslator::new(self.consts)
                        .translate(reg1)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;
                    let reg2 = RValueToStgLocTranslator::new(self.consts)
                        .translate(reg2)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;
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
                    let reg1 = RValueToStgLocTranslator::new(self.consts)
                        .translate(reg1)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;

                    let reg2 = RValueToStgLocTranslator::new(self.consts)
                        .translate(reg2)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;

                    let imm = self.try_rvalue_as_i32(op_name, imm)?;
                    if !(-512..1024).contains(&imm) {
                        return Err(InstrTranslationErr::WrongArgType {
                            opcode: op_name.into(),
                            expected: "10-bit integer",
                            got: imm.to_string(),
                        });
                    }

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
                    let tr = RValueToStgLocTranslator::new(self.consts);
                    let ind = tr
                        .translate(ind)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;
                    let src_reg = tr
                        .translate(src_reg)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;

                    return Ok(CheckInstr::RRI {
                        opcode,
                        reg1: ind,
                        reg2: src_reg,
                        imm10: 0,
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
                    let tr = RValueToStgLocTranslator::new(self.consts);
                    let dst_reg = tr
                        .translate(dst_reg)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;
                    let ind = tr
                        .translate(ind)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;

                    dbg!((op_name, &dst_reg, &ind));

                    return Ok(CheckInstr::RRI {
                        opcode,
                        reg1: dst_reg,
                        reg2: ind,
                        imm10: 0,
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
                    let reg1 = RValueToStgLocTranslator::new(self.consts)
                        .translate(reg1)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;

                    let reg2 = RValueToStgLocTranslator::new(self.consts)
                        .translate(reg2)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;

                    let reg3 = RValueToStgLocTranslator::new(self.consts)
                        .translate(reg3)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;

                    return Ok(CheckInstr::RRR {
                        opcode,
                        reg1,
                        reg2,
                        reg3,
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
