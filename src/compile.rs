use std::collections::{BTreeSet, HashMap};
use std::io;
use std::path::PathBuf;

use crate::ast::*;

pub struct CodeGen {
    label_indexes: LabelIndexes,
    emit_comments: bool,
    current_fn: Option<FnInfo>,
    filename: Option<PathBuf>,
    var_aliases: HashMap<Var, LValue>,
    consts: BTreeSet<Var>,
    string_literal_labels: HashMap<String, String>,
}

impl CodeGen {
    pub fn new(filename: impl Into<Option<PathBuf>>) -> Self {
        Self {
            label_indexes: LabelIndexes::new(),
            emit_comments: true,
            current_fn: None,
            filename: filename.into(),
            var_aliases: HashMap::new(),
            consts: BTreeSet::new(),
            string_literal_labels: HashMap::new(),
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

    fn comment(&self, out: &mut dyn io::Write, comment: &str) -> io::Result<()> {
        if self.emit_comments {
            writeln!(out, "; {}", comment)?;
        }
        Ok(())
    }

    pub fn compile(&mut self, out: &mut dyn io::Write, ast: Vec<Item>) -> io::Result<()> {
        writeln!(out, "#include \"../lark.customasm\"")?;
        writeln!(out, "#bank rom")?;
        writeln!(out)?;

        for item in &ast {
            self.compile_item(out, item)?;
        }

        // Emit string literals.
        for (s, lbl) in &self.string_literal_labels {
            writeln!(out, "{}:", lbl)?;
            writeln!(out, "\t#d \"{}\"", s)?;
            writeln!(out)?;
        }

        Ok(())
    }

    fn compile_item(&mut self, out: &mut dyn io::Write, item: &Item) -> io::Result<()> {
        match item {
            Item::Const { name, value } => {
                self.consts.insert(name.clone());
                writeln!(out, "#const {name} = {value}")?;
            }

            Item::FnDef {
                name,
                args,
                preserve_regs,
                body,
            } => {
                self.compile_fn_def(out, name, preserve_regs, args, body)?;
            }

            Item::Directive(Directive::Addr(addr)) => {
                writeln!(out, "#addr {}", addr)?;
            }

            Item::Directive(Directive::Data(data)) => {
                write!(out, "#d8 ")?;
                for (i, d) in data.iter().enumerate() {
                    if i != 0 {
                        write!(out, ", ")?;
                    }
                    match d {
                        RValue::Alias(name) => {
                            if self.consts.contains(name) {
                                write!(out, "{name}")?;
                            } else if self.var_aliases.contains_key(name) {
                                eprintln!(
                                    "Error [{}#{}]:",
                                    self.filename(),
                                    self.current_fn_name()
                                );
                                eprintln!("\tA variable reference in a `[[data(..)]]` directive cannot refer to a runtime value. It must be const. `{name}`.");
                                std::process::exit(1);
                            } else {
                                eprintln!(
                                    "Error [{}#{}]:",
                                    self.filename(),
                                    self.current_fn_name()
                                );
                                eprintln!("\tUnbound variable: `{name}`");
                                std::process::exit(1);
                            }
                        }
                        RValue::Uint(u) => write!(out, "{u}")?,
                        other => {
                            eprintln!("Error [{}#{}]:", self.filename(), self.current_fn_name());
                            eprintln!("\tA `[[data(..)]]` directive may only contain constants. Received: `{other:?}`");
                            std::process::exit(1);
                        }
                    }
                }
                writeln!(out)?;
            }
        }

        // Space out the items.
        writeln!(out)?;

        Ok(())
    }

    fn compile_fn_def(
        &mut self,
        out: &mut dyn io::Write,
        fn_name: &String,
        preserve_regs: &Vec<Reg>,
        args: &Vec<AliasBinding>,
        body: &Vec<Stmt>,
    ) -> io::Result<()> {
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
                name_path.push(varname.clone());
                let qualified_name = name_path.join(".");

                match lvalue {
                    LValue::Reg(Reg::Arg(a)) => {
                        let was_available = available_argument_registers.remove(&Reg::Arg(*a));
                        if was_available {
                            self.var_aliases.insert(qualified_name, lvalue.clone());
                        } else {
                            self.warn_register_already_assigned(fn_name, *a);
                        }
                    }
                    LValue::Indirection { .. } | LValue::Reg(..) => {
                        // TODO: check liveness of the specific memory address?
                        // For example: "Warn: [$sp-4] is already aliased by another var `blah`."
                        self.var_aliases.insert(qualified_name, lvalue.clone());
                    }
                }

                name_path.pop();
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
        out: &mut dyn io::Write,
        preserve_regs: &Vec<Reg>,
        fn_name: &String,
    ) -> io::Result<()> {
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

    fn compile_stmt(&mut self, out: &mut dyn io::Write, stmt: &Stmt) -> io::Result<()> {
        match stmt {
            Stmt::Label(name) => writeln!(out, "{}:", name)?,

            Stmt::Instr(instr) => self.compile_instr(out, instr)?,

            Stmt::Restore => {
                let regs_to_restore = &self
                    .current_fn
                    .as_ref()
                    .expect("current function set")
                    .callee_regs_to_save;
                if !regs_to_restore.is_empty() {
                    self.comment(out, "<Restore>")?;
                    for (i, reg) in regs_to_restore.iter().enumerate() {
                        let offset = i as isize * 2;
                        writeln!(out, "\tlw\t{reg}, {offset}($sp)")?;
                    }
                    writeln!(out, "\taddi\t$sp, $sp, {}", regs_to_restore.len() * 2)?;
                    self.comment(out, "</Restore>")?;
                } else {
                    eprintln!(
                        "Warning [{}#{}#restore]:",
                        self.filename(),
                        self.current_fn_name()
                    );
                    eprintln!("\tUnnecessary `restore` statement.");
                    eprintln!("hint: Since no reigisters were `preserve`ed in the function declaration, this `restore` statement does nothing.")
                }
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
                test_arg,
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
                let test_arg_resolved = match test_arg {
                    RValue::LValue(LValue::Reg(reg)) => reg,
                    RValue::Alias(name) => {
                        let resolved = self
                            .var_aliases
                            .get(name)
                            .expect("Test argument of while loop could not be resolved.");
                        match resolved {
                            LValue::Reg(reg) => reg,
                            _ => panic!("Test argument of while loop is not a register."),
                        }
                    }
                    _ => panic!("Test argument of while loop is not a register."),
                };
                writeln!(out, "\tbt\t{test_arg_resolved}, {loop_top}")?;
                self.comment(out, "</While>")?;
                writeln!(out)?;
            }
        }
        Ok(())
    }

    fn bind_fn_local_alias(
        &mut self,
        out: &mut dyn io::Write,
        binding: &AliasBinding,
        name_path: &mut Vec<String>,
    ) -> io::Result<()> {
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
                {
                    if varname == "vtty_buf_len" {
                        println!("{binding:?}");
                    }
                }
                name_path.push(varname.clone());
                let qualified_name = name_path.join(".");
                self.comment(out, &format!("<DefAlias {qualified_name} => {lvalue} />"))?;
                self.var_aliases.insert(qualified_name, lvalue.clone());
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

    fn compile_instr(&mut self, out: &mut dyn io::Write, instr: &Instr) -> io::Result<()> {
        write!(out, "\t{}\t", instr.op)?;
        // write the first arg, then comma separate the rest
        let mut args = instr.args.iter();
        if let Some(arg) = args.next() {
            self.compile_instr_rvalue(out, arg)?;
        }
        for arg in args {
            write!(out, ", ")?;
            self.compile_instr_rvalue(out, arg)?;
        }
        writeln!(out)?;
        Ok(())
    }

    fn compile_instr_rvalue(&mut self, out: &mut dyn io::Write, arg: &RValue) -> io::Result<()> {
        match arg {
            RValue::Uint(n) => write!(out, "{}", n),
            RValue::Int(n) => write!(out, "{}", n),
            RValue::Char(c) => write!(out, "{}", c),
            RValue::String(s) => {
                let fresh_lbl = self.get_or_insert_string_literal(s);
                write!(out, "{fresh_lbl}")
            }
            RValue::Label(name) => write!(out, "{}", name),

            RValue::Alias(name) => {
                if let Some(resolved) = self.var_aliases.get(name) {
                    write!(out, "{resolved}")
                } else if self.consts.contains(name) {
                    // `customasm` will handle interpolating this constant later.
                    write!(out, "{name}")
                } else {
                    eprintln!("Error [{}#{}]:", self.filename(), self.current_fn_name());
                    eprintln!("\tUndefined variable `{}`.", name);
                    std::process::exit(1);
                }
            }

            RValue::LValue(lvalue) => self.compile_instr_lvalue(out, lvalue),
        }
    }

    fn compile_instr_lvalue(&mut self, out: &mut dyn io::Write, lvalue: &LValue) -> io::Result<()> {
        match lvalue {
            LValue::Reg(reg) => write!(out, "{}", reg),
            LValue::Indirection { base, offset } => {
                // This case is a little exceptional. If it looks like the base is a constant,
                // interpret the constant as an offset from the zero register.
                if let (Some(Base::AliasOrConst(base)), None) = (base, offset) {
                    if self.consts.contains(base) {
                        write!(out, "{base}($zero)")?;
                        return Ok(());
                    }
                }

                if let Some(offset) = offset {
                    match offset {
                        Offset::I10(i) => write!(out, "{}", i)?,
                        Offset::Const(name) => {
                            if self.consts.contains(name) {
                                write!(out, "{name}")?;
                            } else {
                                eprintln!(
                                    "Error [{}#{}]:",
                                    self.filename(),
                                    self.current_fn_name()
                                );
                                eprintln!("\tA variable reference in an indirection expression must be a constant. `{name}`.");
                                std::process::exit(1);
                            }
                        }
                        Offset::NegatedConst(name) => {
                            if self.consts.contains(name) {
                                write!(out, "-{name}")?;
                            } else {
                                eprintln!(
                                    "Error [{}#{}]:",
                                    self.filename(),
                                    self.current_fn_name()
                                );
                                eprintln!("\tA variable reference in an indirection expression must be a constant. `{name}`.");
                                std::process::exit(1);
                            }
                        }
                    }
                } else {
                    write!(out, "0")?;
                }

                write!(out, "(")?;
                if let Some(base) = base {
                    match base {
                        Base::Reg(reg) => write!(out, "{reg}")?,
                        Base::AliasOrConst(name) => {
                            if self.consts.contains(name) {
                                write!(out, "{name}")?;
                            } else if let Some(resolved) = self.var_aliases.get(name) {
                                write!(out, "{resolved}")?;
                            } else {
                                eprintln!(
                                    "Error [{}#{}]:",
                                    self.filename(),
                                    self.current_fn_name()
                                );
                                eprintln!("\tUndefined variable `{name}`.");
                                std::process::exit(1);
                            }
                        }
                    }
                }
                write!(out, ")")?;

                Ok(())
            }
        }
    }

    fn get_or_insert_string_literal<'a>(&'a mut self, s: &str) -> &'a str {
        let preview = s
            .chars()
            .take(8)
            .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
            .collect::<String>();

        let label_prefix = format!("strlit__QQ{preview}QQ__");

        self.string_literal_labels
            .entry(s.to_string())
            .or_insert_with(|| self.label_indexes.fresh(&label_prefix))
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

    /// Returns a fresh label with the given prefix.
    fn fresh(&mut self, prefix: &str) -> String {
        // Increments the label index and returns the previous value
        let index = self.indexes.entry(prefix.to_string()).or_insert(0);
        let prev = *index;
        *index += 1;
        format!("{}{}", prefix, prev)
    }
}
