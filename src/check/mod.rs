pub mod cfg;
pub mod check_instr;
pub mod defs_uses;
pub mod interferences;
pub mod stg_loc;

#[test]
fn live_ins_live_outs() {
    use lark_vm::cpu::{instr::ops::*, regs::Reg};

    use cfg::Cfg;
    use check_instr::CheckInstr;
    use interferences::Interferences;
    use stg_loc::StgLoc;

    /*
    int foo(int n) {
        int z = 10;
        int x = 0;
        int y = 1;
        while (x < n) {
            z = x * 2 + y;
            x++;
            y = x + z;
        }
        return y;
    }
    -------------------
    subr foo_compiled(n => $a0) {
        alias   z => $t0;
        alias   x => $t1;
        alias   y => $t2;
        alias   cond => $t0;

        !li     z, 10;
        !li     x, 0;
        !li     y, 1;

        loop_top:
        !tlt    cond, x, n;
        !bf     cond, &loop_end;
        alias   tmp => $t0;
        !li     tmp, 2;
        !shl    z, x, tmp;
        !add    z, z, y;
        !addi   x, x, 1;
        !add    y, x, z;
        !j      &loop_top;
        loop_end:

        !mv     $rv, y;
        !jr     $ra;
    }
    */

    let stmts = vec![
        CheckInstr::RI {
            opcode: OpcodeRegImm::LI,
            reg: StgLoc::Alias("z".to_string()),
            imm: 10,
        },
        CheckInstr::RI {
            opcode: OpcodeRegImm::LI,
            reg: StgLoc::Alias("x".to_string()),
            imm: 0,
        },
        CheckInstr::RI {
            opcode: OpcodeRegImm::LI,
            reg: StgLoc::Alias("y".to_string()),
            imm: 1,
        },
        // LOOP_TOP (index 3)
        CheckInstr::RRR {
            opcode: OpcodeRegRegReg::TLT,
            reg1: StgLoc::Alias("cond".to_string()),
            reg2: StgLoc::Alias("x".to_string()),
            reg3: StgLoc::Alias("n".to_string()),
        },
        // Index 4
        CheckInstr::RI {
            opcode: OpcodeRegImm::BF,
            reg: StgLoc::Alias("cond".to_string()),
            imm: 700, // Imm is irrelevant in this example
        },
        CheckInstr::RI {
            opcode: OpcodeRegImm::LI,
            reg: StgLoc::Alias("tmp".to_string()),
            imm: 2,
        },
        CheckInstr::RRR {
            opcode: OpcodeRegRegReg::SHL,
            reg1: StgLoc::Alias("z".to_string()),
            reg2: StgLoc::Alias("x".to_string()),
            reg3: StgLoc::Alias("tmp".to_string()),
        },
        CheckInstr::RRR {
            opcode: OpcodeRegRegReg::ADD,
            reg1: StgLoc::Alias("z".to_string()),
            reg2: StgLoc::Alias("z".to_string()),
            reg3: StgLoc::Alias("y".to_string()),
        },
        CheckInstr::RRI {
            opcode: OpcodeRegRegImm::ADDI,
            reg1: StgLoc::Alias("x".to_string()),
            reg2: StgLoc::Alias("x".to_string()),
            imm10: 1,
        },
        CheckInstr::RRR {
            opcode: OpcodeRegRegReg::ADD,
            reg1: StgLoc::Alias("y".to_string()),
            reg2: StgLoc::Alias("x".to_string()),
            reg3: StgLoc::Alias("z".to_string()),
        },
        CheckInstr::A {
            opcode: OpcodeAddr::J,
            offset: 3,
        },
        // LOOP_END (index 11)
        CheckInstr::RR {
            opcode: OpcodeRegReg::MV,
            reg1: StgLoc::Reg(Reg::Rv),
            reg2: StgLoc::Alias("y".to_string()),
        },
        CheckInstr::R {
            opcode: OpcodeReg::JR,
            reg: StgLoc::Reg(Reg::Ra),
        },
    ];
    let stmts_len = stmts.len();

    let cfg = Cfg::new(stmts)
        .with_edges(
            (1..stmts_len)
                .map(|idx| (idx - 1, idx))
                .chain([(4, 11), (10, 3)]),
        )
        .with_entry(0)
        .with_exits([12])
        .set_non_void_subr();

    let (live_ins, live_outs) = cfg.compute_live_ins_live_outs();

    for (id, instr) in cfg.stmts.iter().enumerate() {
        let ins = live_ins
            .get(&id)
            .cloned()
            .unwrap_or_default()
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        println!("ins:\t{{{ins}}}");
        println!("{id:03}:\t{}", instr);
        let outs = live_outs
            .get(&id)
            .cloned()
            .unwrap_or_default()
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        println!("outs:\t{{{outs}}}",);
        println!();
    }

    println!("================================");

    let interferences = Interferences::from_live_sets(&cfg.stmts, live_outs);

    for (a, bs) in interferences.edges.iter() {
        println!(
            "{a} -> [{}]",
            bs.iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        );
    }
}
