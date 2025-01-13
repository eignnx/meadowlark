use lark_vm::cpu::{instr::ops::*, regs::Reg};

use crate::compile::CodeGen;

use super::{
    check_instr::{CheckInstr, ImmEvalError},
    stg_loc::StgLoc,
};

/// - `defs` are storage locations that would be overwritten by the instruction `self`.
/// - `uses` are storage locations that would need to be read from by the instruction `self`.
pub fn defs_and_uses(
    instr: &CheckInstr,
    defs: &mut impl Extend<StgLoc>,
    uses: &mut impl Extend<StgLoc>,
    codegen: &CodeGen,
) {
    match instr {
        CheckInstr::O { opcode } => match opcode {
            OpcodeOp::HALT | OpcodeOp::NOP | OpcodeOp::INRE | OpcodeOp::INRD | OpcodeOp::KRET => {}
        },

        CheckInstr::A { opcode, offset: _ } => match opcode {
            OpcodeAddr::J => {}
        },

        CheckInstr::I { opcode, imm10: _ } => match opcode {
            OpcodeImm::EXN | OpcodeImm::KCALL => {}
        },

        CheckInstr::R { opcode, reg } => match opcode {
            OpcodeReg::JR => uses.extend([reg.uses()]),
            OpcodeReg::MVLO => {
                uses.extend([StgLoc::Lo]);
                defs.extend([reg.clone()]);
            }
            OpcodeReg::MVHI => {
                uses.extend([StgLoc::Hi]);
                defs.extend([reg.clone()]);
            }
        },

        CheckInstr::RI {
            opcode,
            reg,
            imm: _,
        } => match opcode {
            OpcodeRegImm::JAL => {
                let link_reg = reg;
                defs.extend([link_reg.clone()]);
                defs.extend(Reg::CALLER_SAVED.iter().map(|&r| r.into()));
            }
            OpcodeRegImm::LI => defs.extend([reg.clone()]),
            OpcodeRegImm::BT | OpcodeRegImm::BF => uses.extend([reg.uses()]),
        },

        CheckInstr::RR { opcode, reg1, reg2 } => match opcode {
            OpcodeRegReg::JRAL => {
                let (link_reg, jump_addr_reg) = (reg1, reg2);

                defs.extend([link_reg.clone()]);
                defs.extend(Reg::CALLER_SAVED.iter().map(|&r| r.into()));

                uses.extend([jump_addr_reg.uses()]);
            }
            OpcodeRegReg::MV
            | OpcodeRegReg::NOT
            | OpcodeRegReg::NEG
            | OpcodeRegReg::SEB
            | OpcodeRegReg::TEZ
            | OpcodeRegReg::TNZ => {
                let (rd, rs) = (reg1, reg2);
                defs.extend([rd.clone()]);
                uses.extend([rs.uses()]);
            }
            OpcodeRegReg::MUL | OpcodeRegReg::MULU | OpcodeRegReg::DIV | OpcodeRegReg::DIVU => {
                defs.extend([StgLoc::Lo, StgLoc::Hi]);
            }
        },

        CheckInstr::RRR {
            opcode,
            reg1: rd,
            reg2: rs,
            reg3: rt,
        } => match opcode {
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
            | OpcodeRegRegReg::TGEU => {
                defs.extend([rd.clone()]);
                uses.extend([rs.uses(), rt.uses()]);
            }
        },

        CheckInstr::RRI {
            opcode,
            reg1,
            reg2,
            imm10,
        } => match opcode {
            OpcodeRegRegImm::LW | OpcodeRegRegImm::LBS | OpcodeRegRegImm::LBU => {
                let (rd, src_addr_reg) = (reg1, reg2);
                defs.extend([rd.clone()]);

                let imm10 = match imm10.try_to_i32(&codegen.consts) {
                    Ok(i) => i,
                    Err(ImmEvalError::LabelNotConvertableToInt) => {
                        panic!("Unexpected label in immediate to load instruction: `{imm10}`")
                    }
                    Err(ImmEvalError::MaxEvalDepthExceeded) => {
                        panic!("Max eval depth exceeded for immediate expression `{imm10}`")
                    }
                    Err(ImmEvalError::UnboundConstAlias(name)) => {
                        panic!("Unbound const alias `{name}` in immediate expression `{imm10}`")
                    }
                };

                match src_addr_reg.clone().try_into() {
                    Ok(Reg::Sp) => {
                        uses.extend([src_addr_reg.uses(), StgLoc::Stack(imm10)]);
                    }
                    Ok(Reg::Gp) => {
                        uses.extend([src_addr_reg.uses(), StgLoc::Global(imm10)]);
                    }
                    _ => uses.extend([src_addr_reg.uses()]),
                }
            }
            OpcodeRegRegImm::SW | OpcodeRegRegImm::SB => {
                let (dest_addr_reg, rs) = (reg1, reg2);

                let imm10 = match imm10.try_to_i32(&codegen.consts) {
                    Ok(i) => i,
                    Err(ImmEvalError::LabelNotConvertableToInt) => {
                        panic!("Unexpected label in immediate to store instruction: `{imm10}`")
                    }
                    Err(ImmEvalError::MaxEvalDepthExceeded) => {
                        panic!("Max eval depth exceeded for immediate expression `{imm10}`")
                    }
                    Err(ImmEvalError::UnboundConstAlias(name)) => {
                        panic!("Unbound const alias `{name}` in immediate expression `{imm10}`")
                    }
                };

                uses.extend([dest_addr_reg.uses(), rs.clone()]);
                match dest_addr_reg.clone().try_into() {
                    Ok(Reg::Sp) => defs.extend([StgLoc::Stack(imm10)]),
                    Ok(Reg::Gp) => defs.extend([StgLoc::Global(imm10)]),
                    _ => {} // Ought to add `UniqueMem{..}` here, but that adds clutter to interference graph with no benefit.
                }
            }
            OpcodeRegRegImm::ADDI
            | OpcodeRegRegImm::SUBI
            | OpcodeRegRegImm::ORI
            | OpcodeRegRegImm::XORI
            | OpcodeRegRegImm::ANDI => {
                let (rd, rs) = (reg1, reg2);
                defs.extend([rd.clone()]);
                uses.extend([rs.uses()]);
            }
        },
    }
}
