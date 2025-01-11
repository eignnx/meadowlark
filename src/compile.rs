use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap},
    io,
    path::PathBuf,
};

use lark_vm::cpu::{instr::ops::*, regs::Reg};

use crate::{
    ast::{
        const_val::ConstValue,
        lvalue::{Base, Displacement, LValue},
        rvalue::RValue,
        AliasBinding, Directive, Instr, Item, Stmt, Var,
    },
    check::{
        cfg::{Cfg, Link, NodeId},
        check_instr::{CheckInstr, CheckInstrTranslator},
        interferences::Interferences,
        stg_loc::StgLoc,
    },
    debug_flags,
};

pub struct CodeGen {
    label_indexes: LabelIndexes,
    emit_comments: bool,
    current_subr: Option<SubrInfo>,
    filename: Option<PathBuf>,
    var_aliases: HashMap<Var, LValue>,
    consts: BTreeMap<Var, ConstValue>,
    string_literal_labels: HashMap<String, String>,
}

impl CodeGen {
    pub fn new(filename: impl Into<Option<PathBuf>>) -> Self {
        Self {
            label_indexes: LabelIndexes::new(),
            emit_comments: true,
            current_subr: None,
            filename: filename.into(),
            var_aliases: HashMap::new(),
            consts: BTreeMap::new(),
            string_literal_labels: HashMap::new(),
        }
    }

    #[inline]
    fn filename(&self) -> &str {
        self.filename
            .as_ref()
            .and_then(|p| p.to_str())
            .unwrap_or("<unknown>")
    }

    #[inline]
    fn current_subr_name(&self) -> &str {
        self.current_subr
            .as_ref()
            .map(|f| f.subr_name.as_str())
            .unwrap_or("<unknown-subr>")
    }

    fn comment(&self, out: &mut dyn io::Write, comment: &str) -> io::Result<()> {
        if self.emit_comments {
            writeln!(out, "; {}", comment)?;
        }
        Ok(())
    }

    fn current_cfg_mut(&mut self) -> &mut Cfg {
        &mut self
            .current_subr
            .as_mut()
            .expect("current function set")
            .cfg
    }

    fn emit_instr<'s>(
        &mut self,
        out: &mut dyn io::Write,
        instr: CheckInstr,
        links: impl IntoIterator<Item = Link<'s>> + 's,
    ) -> io::Result<NodeId> {
        writeln!(out, "\t{instr}")?;
        self.emit_instr_no_write(instr, links)
    }

    fn emit_instr_no_write<'s>(
        &mut self,
        instr: CheckInstr,
        links: impl IntoIterator<Item = Link<'s>> + 's,
    ) -> io::Result<NodeId> {
        let node_id = self.current_cfg_mut().push_instr(instr, links);
        Ok(node_id)
    }

    fn emit_label(&mut self, out: &mut dyn io::Write, label: String) -> io::Result<()> {
        writeln!(out, "{label}:")?;
        self.current_cfg_mut().add_label(label, None);
        Ok(())
    }

    pub fn compile(&mut self, out: &mut dyn io::Write, ast: Vec<Item>) -> io::Result<()> {
        writeln!(out, "#include \"../lark.customasm\"")?;
        writeln!(out, "#bank rom")?;
        writeln!(out)?;

        let (consts, non_consts): (Vec<_>, _) = ast
            .into_iter()
            .partition(|item| matches!(item, Item::Const { .. }));

        let consts = consts
            .into_iter()
            .map(|item| match item {
                Item::Const { name, value } => (name, value),
                _ => unreachable!(),
            })
            .collect::<Vec<_>>();

        for (name, value) in &consts {
            self.consts.insert(name.clone(), value.clone());
        }

        for (name, value) in &consts {
            let evaluated = self.eval_const_value(value, 0);
            writeln!(out, "#const {name} = {evaluated}")?;
        }

        writeln!(out)?;

        for item in &non_consts {
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
            Item::Const { .. } => {
                unreachable!()
            }

            Item::SubrDef {
                name,
                args,
                preserve_regs,
                is_isr,
                body,
            } => {
                self.compile_subr_def(out, name, preserve_regs, args, body, *is_isr)?;
                writeln!(out)?;
                writeln!(out)?;
            }

            Item::Directive(Directive::Addr(addr)) => {
                writeln!(out, "#addr {}", addr)?;
                writeln!(out)?;
            }

            Item::Directive(Directive::Data(data)) => {
                write!(out, "#d8 ")?;
                for (i, d) in data.iter().enumerate() {
                    if i != 0 {
                        write!(out, ", ")?;
                    }
                    match d {
                        RValue::ConstAlias(name) => {
                            if self.consts.contains_key(name) {
                                write!(out, "{name}")?;
                            } else {
                                eprintln!("Error [{}#<directive {:?}>]:", self.filename(), item);
                                eprintln!("\tUnbound constant: `{name}`");
                                std::process::exit(1);
                            }
                        }
                        RValue::LValue(lval) => {
                            eprintln!("Error [{}#<directive {:?}>]:", self.filename(), item);
                            eprintln!("\tA `[[data(..)]]` directive cannot refer to a runtime value. It must be const. `{lval}`.");
                            std::process::exit(1);
                        }
                        RValue::Uint(u) => write!(out, "{u}")?,
                        other => {
                            eprintln!("Error [{}#{}]:", self.filename(), self.current_subr_name());
                            eprintln!("\tA `[[data(..)]]` directive may only contain constants. Received: `{other:?}`");
                            std::process::exit(1);
                        }
                    }
                }
                writeln!(out)?;
            }
        }

        Ok(())
    }

    const MAX_CONST_EVAL_DEPTH: usize = 64;

    fn eval_const_value(&self, val: &ConstValue, depth: usize) -> i32 {
        if depth > Self::MAX_CONST_EVAL_DEPTH {
            eprintln!("Error [{}#{}]:", self.filename(), self.current_subr_name());
            eprintln!("\tMaximum const-evaluation depth exceeded.");
            std::process::exit(1);
        }

        match val {
            ConstValue::Uint(u) => *u as i32,
            ConstValue::Int(i) => *i as i32,
            ConstValue::Char(byte) => *byte as i32,
            ConstValue::ConstAlias(name) => {
                if let Some(value) = self.consts.get(name) {
                    self.eval_const_value(value, depth + 1)
                } else {
                    eprintln!("Error [{}#{}]:", self.filename(), self.current_subr_name());
                    eprintln!("\tUndefined constant `{name}`.");
                    std::process::exit(1);
                }
            }
            ConstValue::BinOp(x, binop, y) => {
                let x = self.eval_const_value(x, depth + 1);
                let y = self.eval_const_value(y, depth + 1);
                binop.eval(x, y)
            }
        }
    }

    fn compile_subr_def(
        &mut self,
        out: &mut dyn io::Write,
        subr_name: &String,
        preserve_regs: &[Reg],
        args: &Vec<AliasBinding>,
        body: &Vec<Stmt>,
        is_isr: bool,
    ) -> io::Result<()> {
        // Save info about the current function so we can use it later.
        self.current_subr = Some(SubrInfo {
            subr_name: subr_name.clone(),
            is_isr,
            callee_regs_to_save: preserve_regs.to_vec(),
            cfg: Cfg::new(vec![]),
        });

        self.comment(
            out,
            format!(
                "<SubrDef name={} args=[{}]>",
                subr_name,
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
        writeln!(out, "{}:", subr_name)?;

        // Assign argument registers to variables according to the user's choice.
        self.var_aliases.clear();
        let mut available_argument_registers = BTreeSet::from_iter(Reg::ARGUMENT);

        {
            // This is used to build a qualified name for each variable in case it
            // belongs to a struct.
            let mut name_path = vec![];

            for arg in args {
                self.bind_arg_alias(
                    arg,
                    &mut available_argument_registers,
                    subr_name,
                    &mut name_path,
                );
            }
        }

        // We can skip the prelude if there are no registers to preserve.
        if !preserve_regs.is_empty() {
            self.compile_prelude(out, preserve_regs, subr_name, is_isr)?;
        }

        // Compile the body.
        for stmt in body {
            self.compile_stmt(out, stmt)?;
        }

        self.subr_cfg_check();

        self.comment(out, "</SubrDef>")?;

        Ok(())
    }

    fn subr_cfg_check(&mut self) {
        let cfg = self.current_cfg_mut();
        cfg.add_all_deferred_labels();
        let (_live_ins, live_outs) = cfg.compute_live_ins_live_outs();
        let intfs = Interferences::from_live_sets(cfg.stmts(), live_outs);

        // Check all aliases:
        let alias_bindings = &self.var_aliases;
        for (alias1, binding1) in alias_bindings.iter() {
            for (alias2, binding2) in alias_bindings.iter() {
                if alias1 >= alias2 || binding1 != binding2 {
                    continue;
                }

                if intfs.interferes_with(
                    &StgLoc::Alias(alias1.clone()),
                    &StgLoc::Alias(alias2.clone()),
                ) {
                    eprintln!(
                        "Warning [{}#{}]:",
                        self.filename(),
                        self.current_subr_name()
                    );
                    eprintln!("\tAliases `{alias1}` and `{alias2}` are both bound to register `{binding1}` but both are simultaneously in use.");

                    if debug_flags::PRINT_INTERFERENCE_GRAPH {
                        eprintln!("{intfs}");
                    }
                }
            }
        }

        // Check un-aliased register uses:
        for (alias, binding) in alias_bindings.iter() {
            if let &LValue::Reg(reg) = binding {
                if intfs.interferes_with(&StgLoc::Alias(alias.clone()), &reg.into()) {
                    eprintln!(
                        "Warning [{}#{}]:",
                        self.filename(),
                        self.current_subr_name()
                    );
                    eprintln!("\tAlias `{alias}` is bound to register `{reg}`, but `{reg}` is used on it's own while `{alias}` is in use.");

                    if debug_flags::PRINT_INTERFERENCE_GRAPH {
                        eprintln!("{intfs}");
                    }
                }
            }
        }
    }

    fn bind_arg_alias(
        &mut self,
        arg: &AliasBinding,
        available_argument_registers: &mut BTreeSet<Reg>,
        subr_name: &String,
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
                    self.warn_no_more_arg_regs_available(subr_name, varname);
                }
            }

            // If the user explicitly assigns a register to a variable, we need to check that
            // the register is available.
            AliasBinding::ExplicitAlias(varname, lvalue) => {
                name_path.push(varname.clone());
                let qualified_name = name_path.join(".");

                match lvalue {
                    LValue::Reg(a @ (Reg::A0 | Reg::A1 | Reg::A2)) => {
                        let was_available = available_argument_registers.remove(a);
                        if was_available {
                            self.var_aliases.insert(qualified_name, lvalue.clone());
                        } else {
                            self.warn_register_already_assigned(subr_name, *a);
                        }
                    }

                    LValue::Alias(name) => {
                        todo!("aliases to other aliases are not yet supported. `{name}`");
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
                        subr_name,
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
        preserve_regs: &[Reg],
        subr_name: &String,
        is_isr: bool,
    ) -> io::Result<()> {
        if !is_isr {
            // Warn if the user is preserving a register that is not
            // conventionally callee saved.
            let unneeded = preserve_regs
                .iter()
                .filter(|r| !r.is_callee_saved())
                .map(|r| format!("`{r}`"))
                .collect::<Vec<_>>();
            if !unneeded.is_empty() {
                let unneeded = unneeded.join(", ");
                eprintln!("Warning [{}#{}]:", self.filename(), subr_name);
                eprintln!("\tRegister(s) {unneeded} are not callee saved and usually do not need to be preserved." );
            }
        }

        let reg_list = preserve_regs
            .iter()
            .map(Reg::to_string)
            .collect::<Vec<_>>()
            .join(", ");

        self.comment(out, &format!("<Preserve regs=[{reg_list}]>"))?;

        // Bump the stack pointer to make room for the registers we're about to save.
        self.emit_instr(
            out,
            CheckInstr::RRI {
                opcode: OpcodeRegRegImm::SUBI,
                reg1: Reg::Sp.into(),
                reg2: Reg::Sp.into(),
                imm10: (preserve_regs.len() * 2) as i32,
            },
            [Link::ToNext],
        )?;

        // Save the registers.
        for (i, reg) in preserve_regs.iter().enumerate() {
            self.emit_instr(
                out,
                CheckInstr::RRI {
                    opcode: OpcodeRegRegImm::SW,
                    reg1: Reg::Sp.into(),
                    reg2: (*reg).into(),
                    imm10: (i * std::mem::size_of::<u16>()) as i32,
                },
                [Link::ToNext],
            )?;
        }

        self.comment(out, "</Preserve>")?;

        Ok(())
    }

    fn warn_no_more_arg_regs_available(&mut self, subr_name: &str, varname: &str) {
        let subr_or_isr = if self.current_subr.as_ref().unwrap().is_isr {
            "isr"
        } else {
            "subr"
        };
        eprintln!("Warning [{}#{}]:", self.filename(), subr_name);
        eprintln!("\tNo more argument registers available for variable `{varname}`.");
        eprintln!();
        eprintln!("\thint: Consider assigning variable `{varname}` to a stack location:");
        eprintln!("\t```");
        eprintln!("\t{subr_or_isr} {subr_name}(.., {varname} => [$sp-INDEX], ..) {{");
        eprintln!("\t```");
    }

    fn warn_register_already_assigned(&mut self, name: &String, reg: Reg) {
        let prev_use = self
            .var_aliases
            .iter()
            .find_map(|(vname, l)| l.is_arg_reg(&self.var_aliases).then_some(vname))
            .unwrap();
        eprintln!("Warning [{}#{}]:", self.filename(), name);
        eprintln!("\tArgument register `{reg}` already assigned to argument `{prev_use}`.");
    }

    #[allow(clippy::unusual_byte_groupings)]
    const DUMMY_LABEL_OFFSET: i32 = 0x1ABE1_DED; // "Label, Dead"

    fn compile_stmt(&mut self, out: &mut dyn io::Write, stmt: &Stmt) -> io::Result<()> {
        match stmt {
            Stmt::Label(name) => self.emit_label(out, name.to_owned())?,

            Stmt::Instr(instr) => self.compile_instr(out, instr)?,

            Stmt::Restore => {
                let regs_to_restore = &self
                    .current_subr
                    .as_ref()
                    .expect("current function set")
                    .callee_regs_to_save
                    .clone();

                if !regs_to_restore.is_empty() {
                    self.comment(out, "<Restore>")?;
                    for (i, reg) in regs_to_restore.iter().enumerate().rev() {
                        let offset = i as isize * 2;
                        // writeln!(out, "\tlw\t{reg}, {offset}($sp)")?;
                        self.emit_instr(
                            out,
                            CheckInstr::RRI {
                                opcode: OpcodeRegRegImm::LW,
                                reg1: (*reg).into(),
                                reg2: Reg::Sp.into(),
                                imm10: offset as i32,
                            },
                            [Link::ToNext],
                        )?;
                    }
                    // writeln!(out, "\taddi\t$sp, $sp, {}", regs_to_restore.len() * 2)?;
                    self.emit_instr(
                        out,
                        CheckInstr::RRI {
                            opcode: OpcodeRegRegImm::ADDI,
                            reg1: Reg::Sp.into(),
                            reg2: Reg::Sp.into(),
                            imm10: (regs_to_restore.len() * 2) as i32,
                        },
                        [Link::ToNext],
                    )?;
                    self.comment(out, "</Restore>")?;
                } else {
                    eprintln!(
                        "Warning [{}#{}#restore]:",
                        self.filename(),
                        self.current_subr_name()
                    );
                    eprintln!("\tUnnecessary `restore` statement.");
                    eprintln!("hint: Since no reigisters were `preserve`ed in the function declaration, this `restore` statement does nothing.")
                }
            }

            Stmt::Preserve(regs) => {
                // Make sure the current function remembers which registers it needs to restore.

                let regs_to_save = &mut self
                    .current_subr
                    .as_mut()
                    .expect("current function set")
                    .callee_regs_to_save;

                let mut regs_already_saved = vec![];

                if regs_to_save.is_empty() {
                    regs_to_save.extend(regs.iter().cloned());
                } else {
                    for reg in regs {
                        if !regs_to_save.contains(reg) {
                            regs_to_save.push(*reg);
                        } else {
                            regs_already_saved.push(*reg);
                        }
                    }
                }

                let reg_list = regs
                    .iter()
                    .map(Reg::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");

                self.comment(out, &format!("<Preserve regs=[{reg_list}]>"))?;

                // Bump the stack pointer to make room for the registers we're about to save.
                // writeln!(out, "\tsubi\t$sp, $sp, {}", regs.len() * 2)?;
                self.emit_instr(
                    out,
                    CheckInstr::RRI {
                        opcode: OpcodeRegRegImm::SUBI,
                        reg1: Reg::Sp.into(),
                        reg2: Reg::Sp.into(),
                        imm10: (regs.len() * 2) as i32,
                    },
                    [Link::ToNext],
                )?;

                // Save the registers.
                for (i, reg) in regs.iter().enumerate() {
                    if regs_already_saved.contains(reg) {
                        // Save the CURRENT value of the register, even if it was already saved.
                        let current_subr = self.current_subr.as_ref().unwrap();
                        let callee_regs_to_save = &current_subr.callee_regs_to_save;
                        let stack_index =
                            callee_regs_to_save.iter().position(|r| r == reg).unwrap();
                        let offset = (stack_index * std::mem::size_of::<u16>()) as isize;
                        // let comment = "Update previously saved register";
                        // writeln!(out, "\tsw\t{offset}($sp), {reg} ; {comment}")?;
                        self.emit_instr(
                            out,
                            CheckInstr::RRI {
                                opcode: OpcodeRegRegImm::SW,
                                reg1: Reg::Sp.into(),
                                reg2: (*reg).into(),
                                imm10: offset as i32,
                            },
                            [Link::ToNext],
                        )?;
                    } else {
                        let offset = (i * std::mem::size_of::<u16>()) as i32;
                        // writeln!(out, "\tsw\t{offset}($sp), {reg}")?;
                        self.emit_instr(
                            out,
                            CheckInstr::RRI {
                                opcode: OpcodeRegRegImm::SW,
                                reg1: Reg::Sp.into(),
                                reg2: (*reg).into(),
                                imm10: offset,
                            },
                            [Link::ToNext],
                        )?;
                    }
                }

                self.comment(out, "</Preserve>")?;
            }

            Stmt::DefAlias(binding) => {
                let mut name_path = vec![];
                self.bind_subr_local_alias(out, binding, &mut name_path)?;
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

                let test_reg_resolved = match test_reg {
                    RValue::LValue(LValue::Reg(reg)) => reg,
                    RValue::LValue(LValue::Alias(name)) => {
                        let resolved = self
                            .var_aliases
                            .get(name)
                            .expect("Test argument of `if` statement could not be resolved.");
                        match resolved {
                            LValue::Reg(reg) => reg,
                            _ => panic!("Test argument of `if` statement is not a register."),
                        }
                    }
                    _ => panic!("Test argument of `if` statement is not a register."),
                };

                writeln!(out, "\tbf\t{test_reg_resolved}, {if_else}")?;
                self.emit_instr_no_write(
                    CheckInstr::RI {
                        opcode: OpcodeRegImm::BF,
                        reg: (*test_reg_resolved).into(),
                        imm: Self::DUMMY_LABEL_OFFSET,
                    },
                    [Link::JumpToLabel(Cow::Borrowed(&if_else)), Link::ToNext],
                )?;

                self.comment(out, "<IfElse.Consequent>")?;
                for stmt in consequent {
                    self.compile_stmt(out, stmt)?;
                }
                self.comment(out, "</IfElse.Consequent>")?;

                writeln!(out, "\tj\t{if_end}")?;
                self.emit_instr_no_write(
                    CheckInstr::A {
                        opcode: OpcodeAddr::J,
                        offset: Self::DUMMY_LABEL_OFFSET,
                    },
                    [Link::JumpToLabel(Cow::Borrowed(&if_end)), Link::ToNext],
                )?;

                self.comment(out, "<IfElse.Alternative>")?;
                // writeln!(out, "{if_else}:")?;
                self.emit_label(out, if_else)?;

                if let Some(alternative) = alternative {
                    for stmt in alternative {
                        self.compile_stmt(out, stmt)?;
                    }
                }

                // writeln!(out, "{if_end}:")?;
                self.emit_label(out, if_end)?;
                self.comment(out, "</IfElse.Alternative>")?;
                self.comment(out, "</IfElse>")?;
                writeln!(out)?;
            }

            Stmt::While {
                test_arg,
                test_cond,
                update,
                body,
            } => {
                let loop_top = self.label_indexes.fresh(".while_top");
                let loop_cond = self.label_indexes.fresh(".while_cond");

                self.comment(out, "<While>")?;
                writeln!(out, "\tj\t{loop_cond}")?;
                self.emit_instr_no_write(
                    CheckInstr::A {
                        opcode: OpcodeAddr::J,
                        offset: Self::DUMMY_LABEL_OFFSET,
                    },
                    [Link::JumpToLabel(Cow::Borrowed(&loop_cond)), Link::ToNext],
                )?;
                self.comment(out, "<While.Body>")?;
                // writeln!(out, "{loop_top}:")?;
                self.emit_label(out, loop_top.clone())?;
                for stmt in body {
                    self.compile_stmt(out, stmt)?;
                }
                self.comment(out, "</While.Body>")?;
                if let Some(update) = update {
                    self.comment(out, "<While.Update>")?;
                    for instr in update {
                        self.compile_instr(out, instr)?;
                    }
                    self.comment(out, "</While.Update>")?;
                }
                self.comment(out, "<While.Cond>")?;
                // writeln!(out, "{loop_cond}:")?;
                self.emit_label(out, loop_cond)?;
                for instr in test_cond {
                    self.compile_instr(out, instr)?;
                }
                self.comment(out, "<While.Cond>")?;
                let test_arg_resolved = match test_arg {
                    RValue::LValue(LValue::Reg(reg)) => reg,
                    RValue::LValue(LValue::Alias(name)) => {
                        let resolved = self
                            .var_aliases
                            .get(name)
                            .expect("Test argument of `while` loop could not be resolved.");
                        match resolved {
                            LValue::Reg(reg) => reg,
                            _ => panic!("Test argument of `while` loop is not a register."),
                        }
                    }
                    _ => panic!("Test argument of `while` loop is not a register."),
                };
                writeln!(out, "\tbt\t{test_arg_resolved}, {loop_top}")?;
                self.emit_instr_no_write(
                    CheckInstr::RI {
                        opcode: OpcodeRegImm::BT,
                        reg: (*test_arg_resolved).into(),
                        imm: Self::DUMMY_LABEL_OFFSET,
                    },
                    [Link::JumpToLabel(Cow::Borrowed(&loop_top)), Link::ToNext],
                )?;
                self.comment(out, "</While>")?;
                writeln!(out)?;
            }

            Stmt::Loop { body } => {
                let loop_top = self.label_indexes.fresh(".loop_top");
                self.comment(out, "<Loop>")?;
                // writeln!(out, "{loop_top}:")?;
                self.emit_label(out, loop_top.clone())?;
                for stmt in body {
                    self.compile_stmt(out, stmt)?;
                }
                writeln!(out, "\tj\t{loop_top}")?;
                self.emit_instr_no_write(
                    CheckInstr::A {
                        opcode: OpcodeAddr::J,
                        offset: Self::DUMMY_LABEL_OFFSET,
                    },
                    [Link::JumpToLabel(loop_top.into()), Link::ToNext],
                )?;
                self.comment(out, "</Loop>")?;
                writeln!(out)?;
            }
        }
        Ok(())
    }

    fn bind_subr_local_alias(
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
                    self.current_subr_name()
                );
                eprintln!("\tImplied aliases are only valid for function arguments.");
                std::process::exit(1);
            }

            AliasBinding::ExplicitAlias(varname, lvalue) => {
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
                    self.bind_subr_local_alias(out, field_binding, name_path)?;
                    name_path.pop();
                }
            }
        }
        Ok(())
    }

    fn compile_instr(&mut self, out: &mut dyn io::Write, instr: &Instr) -> io::Result<()> {
        let mut links = vec![];
        match CheckInstrTranslator::new(&self.consts).translate(
            &instr.op,
            &instr.args[..],
            &mut links,
        ) {
            Ok(check_instr) => {
                self.emit_instr_no_write(check_instr, links)?;
            }
            Err(e) => {
                eprintln!("Error [{}#{}]:", self.filename(), self.current_subr_name());
                eprintln!("\tInvalid instruction: {e}");
                std::process::exit(1);
            }
        };

        write!(out, "\t{}\t", instr.op)?;
        for (idx, arg) in instr.args.iter().enumerate() {
            if idx > 0 {
                write!(out, ", ")?;
            }
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

            RValue::LValue(LValue::Alias(name)) => {
                if let Some(resolved) = self.var_aliases.get(name) {
                    self.compile_instr_lvalue(out, &resolved.clone())
                } else {
                    eprintln!("Error [{}#{}]:", self.filename(), self.current_subr_name());
                    eprintln!("\tUndefined alias `{}`.", name);
                    std::process::exit(1);
                }
            }

            RValue::ConstAlias(name) => {
                if self.consts.contains_key(name) {
                    write!(out, "{name}")
                } else {
                    eprintln!("Error [{}#{}]:", self.filename(), self.current_subr_name());
                    eprintln!("\tUndefined constant `{}`.", name);
                    std::process::exit(1);
                }
            }

            RValue::LValue(lvalue) => self.compile_instr_lvalue(out, lvalue),
        }
    }

    fn compile_instr_lvalue(&self, out: &mut dyn io::Write, lvalue: &LValue) -> io::Result<()> {
        match lvalue {
            LValue::Reg(reg) => write!(out, "{}", reg),
            LValue::Alias(name) => {
                if let Some(resolved) = self.var_aliases.get(name) {
                    if let LValue::Alias(name) = resolved {
                        todo!("aliases to other aliases are not yet supported. `{name}`");
                    }
                    self.compile_instr_lvalue(out, &resolved.clone())
                } else {
                    eprintln!("Error [{}#{}]:", self.filename(), self.current_subr_name());
                    eprintln!("\tUndefined alias `{}`.", name);
                    std::process::exit(1);
                }
            }
            LValue::Indirection {
                base,
                displacement: offset,
            } => {
                if let Some(offset) = offset {
                    match offset {
                        Displacement::I10(i) => write!(out, "{}", i)?,
                        Displacement::Const(name) => {
                            if self.consts.contains_key(name) {
                                write!(out, "{name}")?;
                            } else {
                                eprintln!(
                                    "Error [{}#{}]:",
                                    self.filename(),
                                    self.current_subr_name()
                                );
                                eprintln!("\tA variable reference in an indirection expression must be a constant. `{name}`.");
                                std::process::exit(1);
                            }
                        }
                        Displacement::NegatedConst(name) => {
                            if self.consts.contains_key(name) {
                                write!(out, "-{name}")?;
                            } else {
                                eprintln!(
                                    "Error [{}#{}]:",
                                    self.filename(),
                                    self.current_subr_name()
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
                match base {
                    Base::Reg(reg) => write!(out, "{reg}")?,
                    Base::Alias(name) => {
                        if let Some(resolved) = self.var_aliases.get(name) {
                            self.compile_instr_lvalue(out, &resolved.clone())?;
                        } else {
                            eprintln!("Error [{}#{}]:", self.filename(), self.current_subr_name());
                            eprintln!("\tUndefined alias `{name}`.");
                            std::process::exit(1);
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

/// Associated info for a subroutine.
struct SubrInfo {
    subr_name: String,
    is_isr: bool,
    callee_regs_to_save: Vec<Reg>,
    // The control flow graph for the subroutine's body.
    cfg: Cfg,
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
