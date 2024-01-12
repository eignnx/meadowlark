use std::collections::{HashMap, BTreeSet};
use std::path::PathBuf;
use std::fmt;

use crate::ast::*;

pub struct CodeGen {
    label_indexes: LabelIndexes,
    emit_comments: bool,
    current_fn: Option<FnInfo>,
    filename: Option<PathBuf>,
    var_aliases: HashMap<Var, LValue>,
}

impl CodeGen {
    pub fn new(filename: impl Into<Option<PathBuf>>) -> Self {
        Self {
            label_indexes: LabelIndexes::new(),
            emit_comments: true,
            current_fn: None,
            filename: filename.into(),
            var_aliases: HashMap::new(),
        }
    }

    fn filename(&self) -> &str {
        self.filename
            .as_ref()
            .and_then(|p| p.file_name())
            .and_then(|p| p.to_str())
            .unwrap_or("<unknown>")
    }

    fn current_fn_name(&self) -> &str {
        self.current_fn
            .as_ref()
            .map(|f| f.fn_name.as_str())
            .unwrap_or("<unknown-fn>")
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
                args,
                preserve_regs,
                body,
            } => {
                self.comment(
                    out,
                    format!(
                        "<FnDef name={} args=[{}]>",
                        name,
                        args.iter()
                            .map(|a| match a {
                                FnParam::ImpliedAlias(v) => v.to_string(),
                                FnParam::Alias(v, _) => v.to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                    .as_str(),
                )?;
                writeln!(out, "{}:", name)?;

                self.var_aliases.clear();
                let mut available_argument_registers = BTreeSet::from_iter(Reg::argument_registers()); 
                for arg in args {
                    match arg {
                        FnParam::ImpliedAlias(v) => {
                            let Some(reg) = available_argument_registers.iter().next() else {
                                eprintln!("Warning [{}#{}]:", self.filename(), name);
                                eprintln!("\tNo more argument registers available for `{}`.", v);
                                continue;
                            };
                            self.var_aliases.insert(v.clone(), LValue::Reg(*reg));
                        }
                        FnParam::Alias(v, lvalue) => {
                            if let LValue::Reg(Reg::Arg(a)) = lvalue {
                                if !available_argument_registers.remove(&Reg::Arg(*a)) {
                                    let prev_use = self.var_aliases.iter().find(|(_, l)| match l {
                                        LValue::Reg(Reg::Arg(a)) => a == a,
                                        _ => false,
                                    }).map(|(v, _)| v).unwrap();
                                    eprintln!("Warning [{}#{}]:", self.filename(), name);
                                    eprintln!(
                                        "\tArgument register `{}` already assigned to argument `{}`.",
                                        Reg::Arg(*a),
                                        prev_use,
                                    );
                                    continue;
                                }
                            }
                            self.var_aliases.insert(v.clone(), lvalue.clone());
                        }
                    }
                }

                self.current_fn = Some(FnInfo {
                    fn_name: name.clone(),
                    callee_regs_to_save: preserve_regs.clone(),
                });

                if !preserve_regs.is_empty() {
                    let reg_list = 
                            preserve_regs
                                .iter()
                                .inspect(|r| if !r.is_callee_saved() {
                                    eprintln!("Warning [{}#{}]:", self.filename(), name);
                                    eprintln!("\tRegister `{r}` is not callee saved and usually does not need to be preserved.");
                                })
                                .map(|r| r.to_string())
                                .collect::<Vec<_>>()
                                .join(", ");
                    self.comment(
                        out,
                        format!(
                            "<Preserve regs=[{reg_list}]>"
                        )
                        .as_str(),
                    )?;
                    writeln!(out, "\tsubi\t$sp, $sp, {}", preserve_regs.len() * 2)?;
                    for (i, reg) in preserve_regs.iter().enumerate() {
                        let offset = i * 2;
                        writeln!(out, "\tsw\t{offset}($sp), {reg}")?;
                    }
                    self.comment(out, "</Preserve>")?;
                }

                for stmt in body {
                    self.compile_stmt(out, stmt)?;
                }
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

            Stmt::Restore => {
                let regs_to_restore = &self
                    .current_fn
                    .as_ref()
                    .expect("current function set")
                    .callee_regs_to_save;
                self.comment(out, "<Restore>")?;
                for (i, reg) in regs_to_restore.iter().enumerate() {
                    let offset = i * 2;
                    writeln!(out, "\tlw\t{reg}, {offset}($sp)")?;
                }
                writeln!(out, "\taddi\t$sp, $sp, {}", regs_to_restore.len() * 2)?;
                self.comment(out, "</Restore>")?;
            }

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
            Arg::AliasIndirection(name) => {
                let Some(resolved) = self.var_aliases.get(name) else {
                    eprintln!("Error [{}#{}]:", self.filename(), self.current_fn_name());
                    eprintln!("\tUndefined variable `{}`.", name);
                    std::process::exit(1);
                };
                match resolved {
                    LValue::Reg(reg) => write!(out, "0({reg})"),
                    LValue::Mem(reg, offset) => write!(out, "{offset}({reg})"),
                }
            }
            Arg::Alias(name) => {
                let Some(resolved) = self.var_aliases.get(name) else {
                    eprintln!("Error [{}#{}]:", self.filename(), self.current_fn_name());
                    eprintln!("\tUndefined variable `{}`.", name);
                    std::process::exit(1);
                };
                write!(out, "{}", resolved)
            }
        }
    }
}

struct FnInfo {
    fn_name: String,
    callee_regs_to_save: Vec<Reg>,
}

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
