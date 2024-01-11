use std::collections::HashMap;
use std::fmt;

use crate::ast::*;

struct LabelIndexes {
    indexes: HashMap<String, usize>,
}

impl LabelIndexes {
    fn new() -> Self {
        Self {
            indexes: HashMap::new(),
        }
    }

    fn fresh(&mut self, prefix: &str) -> String {
        // Increments the label index and returns the previous value
        let index = self.indexes.entry(prefix.to_string()).or_insert(0);
        let prev = *index;
        *index += 1;
        format!("{}{}", prefix, prev)
    }
}

pub struct CodeGen {
    label_indexes: LabelIndexes,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            label_indexes: LabelIndexes::new(),
        }
    }

    pub fn compile(&mut self, out: &mut dyn fmt::Write, ast: Vec<Stmt>) -> std::fmt::Result {
        for stmt in ast {
            self.compile_stmt(out, stmt)?;
        }
        Ok(())
    }

    fn compile_stmt(&mut self, out: &mut dyn fmt::Write, stmt: Stmt) -> std::fmt::Result {
        match stmt {
            Stmt::Label(name) => writeln!(out, "{}:", name)?,
            Stmt::Instr(instr) => self.compile_instr(out, instr)?,
            Stmt::While {
                test_reg,
                test_cond,
                body,
            } => {
                let loop_top = self.label_indexes.fresh(".while_top");
                let loop_cond = self.label_indexes.fresh(".while_cond");

                writeln!(out, "\tj\t{loop_cond}")?;
                writeln!(out, "{loop_top}:")?;
                for stmt in body {
                    self.compile_stmt(out, stmt)?;
                }
                writeln!(out, "{loop_cond}:")?;
                for instr in test_cond {
                    self.compile_instr(out, instr)?;
                }
                writeln!(out, "\tbt\t{test_reg}, {loop_top}")?;
            }
        }
        Ok(())
    }

    fn compile_instr(&mut self, out: &mut dyn fmt::Write, instr: Instr) -> std::fmt::Result {
        write!(out, "\t{}\t", instr.op)?;
        // write the first arg, then comma separate the rest
        let mut args = instr.args.iter();
        if let Some(arg) = args.next() {
            self.compile_instr_arg(out, arg)?;
        }
        for arg in args {
            write!(out, ", ")?;
            self.compile_instr_arg(out, arg)?;
        }
        writeln!(out)?;
        Ok(())
    }

    fn compile_instr_arg(&mut self, out: &mut dyn fmt::Write, arg: &Arg) -> std::fmt::Result {
        match arg {
            Arg::Uint(n) => write!(out, "{}", n),
            Arg::Int(n) => write!(out, "{}", n),
            Arg::Label(name) => write!(out, "{}", name),
            Arg::Reg(reg) => write!(out, "{}", reg),
            Arg::Offset(n, reg) => write!(out, "{}({})", n, reg),
        }
    }
}
