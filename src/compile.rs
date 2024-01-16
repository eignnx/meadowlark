use std::collections::{BTreeSet, HashMap};
use std::fmt;
use std::path::PathBuf;

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
        writeln!(out, "#include \"lark.customasm\"")?;
        writeln!(out, "#bank rom")?;
        writeln!(out)?;

        for item in &ast {
            self.compile_item(out, item)?;
        }
        Ok(())
    }

    fn compile_item(&mut self, out: &mut dyn fmt::Write, item: &Item) -> std::fmt::Result {
        match item {
            Item::Const { name, value } => {
                writeln!(out, "#const {name} = {value}")?;
            }

            Item::FnDef {
                name,
                args,
                preserve_regs,
                body,
            } => self.compile_fn_def(out, name, preserve_regs, args, body)?,
        }
        writeln!(out)?;
        writeln!(out)?;
        Ok(())
    }

    fn compile_fn_def(
        &mut self,
        out: &mut dyn fmt::Write,
        fn_name: &String,
        preserve_regs: &Vec<Reg>,
        args: &Vec<AliasBinding>,
        body: &Vec<Stmt>,
    ) -> fmt::Result {
        // Save info about the current function so we can use it later.
        self.current_fn = Some(FnInfo {
            fn_name: fn_name.clone(),
            callee_regs_to_save: preserve_regs.clone(),
        });

        self.comment(
            out,
            format!(
                "<FnDef name={} args=[{}]>",
                fn_name,
                args.iter()
                    .map(|a| match a {
                        AliasBinding::ImplicitAlias(v) => v.to_string(),
                        AliasBinding::ExplicitAlias(v, _) => v.to_string(),
                        AliasBinding::Struct { var_name, .. } => var_name.to_string(), // TODO: recurse
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .as_str(),
        )?;

        // Print the label.
        writeln!(out, "{}:", fn_name)?;

        // Assign argument registers to variables according to the user's choice.
        self.var_aliases.clear();
        let mut available_argument_registers = BTreeSet::from_iter(Reg::argument_registers());

        {
            // This is used to build a qualified name for each variable in case it
            // belongs to a struct.
            let mut name_path = vec![];

            for arg in args {
                self.bind_arg_alias(
                    arg,
                    &mut available_argument_registers,
                    fn_name,
                    &mut name_path,
                );
            }
        }

        // We can skip the prelude if there are no registers to preserve.
        if !preserve_regs.is_empty() {
            self.compile_prelude(out, preserve_regs, fn_name)?;
        }

        // Compile the body.
        for stmt in body {
            self.compile_stmt(out, stmt)?;
        }

        self.comment(out, "</FnDef>")?;

        Ok(())
    }

    fn bind_arg_alias(
        &mut self,
        arg: &AliasBinding,
        available_argument_registers: &mut BTreeSet<Reg>,
        fn_name: &String,
        name_path: &mut Vec<String>,
    ) {
        match arg {
            // If the user does not assign a register to a variable, try to assign one
            // automatically.
            AliasBinding::ImplicitAlias(varname) => {
                if let Some(reg) = available_argument_registers.pop_first() {
                    name_path.push(varname.clone());
                    let qualified_name = name_path.join(".");
                    self.var_aliases.insert(qualified_name, LValue::Reg(reg));
                    name_path.pop();
                } else {
                    self.warn_no_more_arg_regs_available(fn_name, varname);
                }
            }

            // If the user explicitly assigns a register to a variable, we need to check that
            // the register is available.
            AliasBinding::ExplicitAlias(varname, lvalue) => {
                if let LValue::Reg(Reg::Arg(a)) = lvalue {
                    let was_available = available_argument_registers.remove(&Reg::Arg(*a));
                    if was_available {
                        name_path.push(varname.clone());
                        let qualified_name = name_path.join(".");
                        self.var_aliases.insert(qualified_name, *lvalue);
                        name_path.pop();
                    } else {
                        self.warn_register_already_assigned(fn_name, *a);
                    }
                }
            }

            // If a name is bound to a struct, build new identifiers for each subfield
            // using the name of the struct as a prefix.
            AliasBinding::Struct {
                var_name,
                field_bindings,
            } => {
                for field_binding in field_bindings {
                    name_path.push(var_name.clone());
                    self.bind_arg_alias(
                        field_binding,
                        available_argument_registers,
                        fn_name,
                        name_path,
                    );
                    name_path.pop();
                }
            }
        }
    }

    fn compile_prelude(
        &mut self,
        out: &mut dyn fmt::Write,
        preserve_regs: &Vec<Reg>,
        fn_name: &String,
    ) -> fmt::Result {
        // Warn if the user is preserving a register that is not
        // conventionally callee saved.
        if let Some(r) = preserve_regs.iter().find(|r| !r.is_callee_saved()) {
            eprintln!("Warning [{}#{}]:", self.filename(), fn_name);
            eprintln!(
                "\tRegister `{r}` is not callee saved and usually does not need to be preserved."
            );
        }

        self.comment(
            out,
            &format!(
                "<Preserve regs=[{reg_list}]>",
                reg_list = preserve_regs
                    .iter()
                    .map(|r| r.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        )?;

        // Bump the stack pointer to make room for the registers we're about to save.
        writeln!(out, "\tsubi\t$sp, $sp, {}", preserve_regs.len() * 2)?;

        // Save the registers.
        for (i, reg) in preserve_regs.iter().enumerate() {
            writeln!(out, "\tsw\t{offset}($sp), {reg}", offset = i as isize * 2)?;
        }

        self.comment(out, "</Preserve>")?;

        Ok(())
    }

    fn warn_no_more_arg_regs_available(&mut self, fn_name: &str, varname: &str) {
        eprintln!("Warning [{}#{}]:", self.filename(), fn_name);
        eprintln!("\tNo more argument registers available for variable `{varname}`.");
        eprintln!();
        eprintln!("\thint: Consider assigning variable `{varname}` to a stack location:");
        eprintln!("\t```");
        eprintln!("\tfn {fn_name}(.., {varname} => [$sp-INDEX], ..) {{");
        eprintln!("\t```");
    }

    fn warn_register_already_assigned(&mut self, name: &String, reg_id: u8) {
        let prev_use = self
            .var_aliases
            .iter()
            .find_map(|(vname, l)| matches!(l, LValue::Reg(Reg::Arg(_))).then(|| vname))
            .unwrap();
        eprintln!("Warning [{}#{}]:", self.filename(), name);
        eprintln!(
            "\tArgument register `{}` already assigned to argument `{}`.",
            Reg::Arg(reg_id),
            prev_use,
        );
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
                    let offset = i as isize * 2;
                    writeln!(out, "\tlw\t{reg}, {offset}($sp)")?;
                }
                writeln!(out, "\taddi\t$sp, $sp, {}", regs_to_restore.len() * 2)?;
                self.comment(out, "</Restore>")?;
            }

            Stmt::DefAlias(binding) => {
                let mut name_path = vec![];
                self.bind_fn_local_alias(out, binding, &mut name_path)?;
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

    fn bind_fn_local_alias(
        &mut self,
        out: &mut dyn fmt::Write,
        binding: &AliasBinding,
        name_path: &mut Vec<String>,
    ) -> fmt::Result {
        match binding {
            // Implicit aliases are not allowed for function local variables.
            AliasBinding::ImplicitAlias(name) => {
                eprintln!(
                    "Error [{}#{}]: (at `alias {name} => ...`)",
                    self.filename(),
                    self.current_fn_name()
                );
                eprintln!("\tImplied aliases are only valid for function arguments.");
                std::process::exit(1);
            }

            AliasBinding::ExplicitAlias(varname, lvalue) => {
                name_path.push(varname.clone());
                let qualified_name = name_path.join(".");
                self.comment(out, &format!("<DefAlias {qualified_name} => {lvalue} />"))?;
                self.var_aliases.insert(qualified_name, *lvalue);
                name_path.pop();
            }

            AliasBinding::Struct {
                var_name,
                field_bindings,
            } => {
                for field_binding in field_bindings {
                    name_path.push(var_name.clone());
                    self.bind_fn_local_alias(out, field_binding, name_path)?;
                    name_path.pop();
                }
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
