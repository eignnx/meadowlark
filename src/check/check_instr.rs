use std::{collections::BTreeMap, str::FromStr};

use lark_vm::cpu::instr::{self, ops::*};

use crate::ast::{ConstValue, RValue};

use super::stg_loc::{RValueToStgLocError, RValueToStgLocTranslator, StgLoc};

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
    ) -> Result<CheckInstr, InstrTranslationErr> {
        if let Ok(opcode) = OpcodeOp::from_str(op_name) {
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
            match args {
                [reg, imm] => {
                    let reg = RValueToStgLocTranslator::new(self.consts)
                        .translate(reg)
                        .map_err(|e| InstrTranslationErr::from_rvalue_err(op_name, e))?;
                    let imm = self.try_rvalue_as_i32(op_name, imm)?;
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
