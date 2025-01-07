use lark_vm::cpu::{
    instr::{self, ops::*},
    regs::Reg,
};

use super::stg_loc::StgLoc;

pub type CheckInstr = instr::Instr<StgLoc, i32>;

/// - `defs` are storage locations that would be overwritten by the instruction `self`.
/// - `uses` are storage locations that would need to be read from by the instruction `self`.
pub fn defs_and_uses(
    instr: &CheckInstr,
    defs: &mut impl Extend<StgLoc>,
    uses: &mut impl Extend<StgLoc>,
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
            OpcodeReg::JR => uses.extend([reg.clone()]),
            OpcodeReg::MVLO | OpcodeReg::MVHI => defs.extend([reg.clone()]),
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
            OpcodeRegImm::BT | OpcodeRegImm::BF => uses.extend([reg.clone()]),
        },

        CheckInstr::RR { opcode, reg1, reg2 } => match opcode {
            OpcodeRegReg::JRAL => {
                let (link_reg, jump_addr_reg) = (reg1, reg2);

                defs.extend([link_reg.clone()]);
                defs.extend(Reg::CALLER_SAVED.iter().map(|&r| r.into()));

                uses.extend([jump_addr_reg.clone()]);
            }
            OpcodeRegReg::MV
            | OpcodeRegReg::NOT
            | OpcodeRegReg::NEG
            | OpcodeRegReg::SEB
            | OpcodeRegReg::TEZ
            | OpcodeRegReg::TNZ => {
                let (rd, rs) = (reg1, reg2);
                defs.extend([rd.clone()]);
                uses.extend([rs.clone()]);
            }
            OpcodeRegReg::MUL | OpcodeRegReg::MULU | OpcodeRegReg::DIV | OpcodeRegReg::DIVU => {
                todo!("How to handle $LO/$HI regs?")
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
                uses.extend([rs.clone(), rt.clone()]);
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

                match src_addr_reg.clone().try_into() {
                    Ok(Reg::Sp) => {
                        uses.extend([src_addr_reg.clone(), StgLoc::Stack(*imm10)]);
                    }
                    Ok(Reg::Gp) => {
                        uses.extend([src_addr_reg.clone(), StgLoc::Global(*imm10)]);
                    }
                    _ => uses.extend([src_addr_reg.clone()]),
                }
            }
            OpcodeRegRegImm::SW | OpcodeRegRegImm::SB => {
                let (dest_addr_reg, rs) = (reg1, reg2);
                uses.extend([dest_addr_reg.clone(), rs.clone()]);
                match dest_addr_reg.clone().try_into() {
                    Ok(Reg::Sp) => defs.extend([StgLoc::Stack(*imm10)]),
                    Ok(Reg::Gp) => defs.extend([StgLoc::Global(*imm10)]),
                    _ => {}
                }
            }
            OpcodeRegRegImm::ADDI
            | OpcodeRegRegImm::SUBI
            | OpcodeRegRegImm::ORI
            | OpcodeRegRegImm::XORI
            | OpcodeRegRegImm::ANDI => {
                let (rd, rs) = (reg1, reg2);
                defs.extend([rd.clone()]);
                uses.extend([rs.clone()]);
            }
        },
    }
}
