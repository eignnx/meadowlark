use std::collections::{BTreeSet, HashMap};
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
    emit_comments: bool,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            label_indexes: LabelIndexes::new(),
            emit_comments: true,
        }
    }

    fn comment(&self, out: &mut dyn fmt::Write, comment: &str) -> std::fmt::Result {
        if self.emit_comments {
            writeln!(out, "; {}", comment)?;
        }
        Ok(())
    }

    pub fn compile(&mut self, out: &mut dyn fmt::Write, ast: Vec<Item>) -> std::fmt::Result {
        for item in &ast {
            self.compile_item(out, item)?;
        }
        Ok(())
    }

    fn compile_item(&mut self, out: &mut dyn fmt::Write, item: &Item) -> std::fmt::Result {
        match item {
            Item::Const { name, value } => {
                writeln!(out, "#const {} = {}", name, value)?;
            }

            Item::FnDef {
                name,
                args: _,
                body,
            } => {
                // Compute the set of callee-saved registers used in this function.
                let mut regs_to_save = BTreeSet::new();
                for stmt in body {
                    stmt.callee_regs_to_save(&mut regs_to_save);
                }

                self.comment(out, "<FnDef>")?;
                writeln!(out, "{}:", name)?;

                {
                    self.comment(out, "<FnDef.Prelude>")?;
                    writeln!(out, "\tsubi\t$sp, $sp, {}", regs_to_save.len() * 2)?;

                    for (i, reg) in regs_to_save.iter().enumerate() {
                        let offset = i * 2;
                        writeln!(out, "\tsw\t{offset}($sp), {reg}")?;
                    }
                    self.comment(out, "</FnDef.Prelude>")?;
                }

                self.comment(out, "<FnDef.Body>")?;
                for stmt in body {
                    self.compile_stmt(out, stmt)?;
                }
                self.comment(out, "</FnDef.Body>")?;

                let mut write_epilogue = || -> fmt::Result {
                    self.comment(out, "<FnDef.Epilogue>")?;
                    for (i, reg) in regs_to_save.iter().enumerate() {
                        let offset = i * 2;
                        writeln!(out, "\tlw\t{reg}, {offset}($sp)")?;
                    }
                    writeln!(out, "\taddi\t$sp, $sp, {}", regs_to_save.len() * 2)?;
                    self.comment(out, "</FnDef.Epilogue>")?;
                    Ok(())
                };

                write_epilogue()?;

                self.comment(out, "</FnDef>")?;
            }
        }
        writeln!(out)?;
        writeln!(out)?;
        Ok(())
    }

    fn compile_stmt(&mut self, out: &mut dyn fmt::Write, stmt: &Stmt) -> std::fmt::Result {
        match stmt {
            Stmt::Label(name) => writeln!(out, "{}:", name)?,

            Stmt::Instr(instr) => self.compile_instr(out, instr)?,

            Stmt::If {
                test_reg,
                test_cond,
                consequent,
                alternative,
            } => {
                let if_else = self.label_indexes.fresh(".if_else");
                let if_end = self.label_indexes.fresh(".if_end");

                self.comment(out, "<IfElse>")?;
                self.comment(out, "<IfElse.Cond>")?;
                for instr in test_cond {
                    self.compile_instr(out, instr)?;
                }
                self.comment(out, "</IfElse.Cond>")?;

                writeln!(out, "\tbf\t{test_reg}, {if_else}")?;

                self.comment(out, "<IfElse.Consequent>")?;
                for stmt in consequent {
                    self.compile_stmt(out, stmt)?;
                }
                self.comment(out, "</IfElse.Consequent>")?;

                writeln!(out, "\tj\t{if_end}")?;
                self.comment(out, "<IfElse.Alternative>")?;
                writeln!(out, "{if_else}:")?;

                if let Some(alternative) = alternative {
                    for stmt in alternative {
                        self.compile_stmt(out, stmt)?;
                    }
                }

                writeln!(out, "{if_end}:")?;
                self.comment(out, "</IfElse.Alternative>")?;
                self.comment(out, "</IfElse>")?;
                writeln!(out)?;
            }

            Stmt::While {
                test_reg,
                test_cond,
                body,
            } => {
                let loop_top = self.label_indexes.fresh(".while_top");
                let loop_cond = self.label_indexes.fresh(".while_cond");

                self.comment(out, "<While>")?;
                writeln!(out, "\tj\t{loop_cond}")?;
                self.comment(out, "<While.Body>")?;
                writeln!(out, "{loop_top}:")?;
                for stmt in body {
                    self.compile_stmt(out, stmt)?;
                }
                self.comment(out, "</While.Body>")?;
                self.comment(out, "<While.Cond>")?;
                writeln!(out, "{loop_cond}:")?;
                for instr in test_cond {
                    self.compile_instr(out, instr)?;
                }
                self.comment(out, "<While.Cond>")?;
                writeln!(out, "\tbt\t{test_reg}, {loop_top}")?;
                self.comment(out, "</While>")?;
                writeln!(out)?;
            }
        }
        Ok(())
    }

    fn compile_instr(&mut self, out: &mut dyn fmt::Write, instr: &Instr) -> std::fmt::Result {
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
