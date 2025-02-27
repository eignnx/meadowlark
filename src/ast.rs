use const_val::ConstValue;
use lark_vm::cpu::regs::Reg;
use lvalue::LValue;
use rvalue::RValue;

pub mod const_val;
pub mod lvalue;
pub mod rvalue;

#[derive(Debug)]
pub enum Item {
    Const {
        name: String,
        value: ConstValue,
    },
    /// `subr` is "subroutine".
    SubrDef {
        name: String,
        args: Vec<AliasBinding>,
        preserve_regs: Vec<Reg>,
        /// True if the function is an interrupt service routine ("isr").
        is_isr: bool,
        body: Vec<Stmt>,
    },
    Directive(Directive),
}

#[derive(Debug)]
pub enum AliasBinding {
    /// Example: `x` (Gets bound to one of `$a0..$a2` if argument).
    /// Only valid for function arguments. I want users to know which registers
    /// and stack offsets are in use.
    ImplicitAlias(Var),

    /// Examples:
    ///    - `local => $s2`
    ///    - `arg => [$sp + 4]`
    ExplicitAlias(Var, LValue),

    /// Examples:
    ///     - `my_point => Point { x => $a0, y => $a1 }`
    ///     - `my_point => { x => [$sp + 4], y => [$sp + 6] }`
    Struct {
        var_name: Var,
        /// If `Some`, refers to a previously defined struct item.
        /// struct_name: Option<String>,
        field_bindings: Vec<AliasBinding>,
    },
}
pub type Var = String;

#[derive(Debug)]
pub enum Stmt {
    Label(String),
    Instr(Instr),
    Restore,
    /// Explicit preserve statement.
    Preserve(Vec<Reg>),
    /// Example: `alias len => $k0;`
    DefAlias(AliasBinding),
    If {
        test_reg: RValue,
        test_cond: Vec<Instr>,
        consequent: Vec<Stmt>,
        alternative: Option<Vec<Stmt>>,
    },
    While {
        test_arg: RValue,
        test_cond: Vec<Instr>,
        update: Option<Vec<Instr>>,
        body: Vec<Stmt>,
    },
    Loop {
        body: Vec<Stmt>,
    },
}

#[derive(Debug)]
pub struct Instr {
    pub op: String,
    pub args: Vec<RValue>,
}

#[derive(Debug, Clone)]
pub enum Directive {
    /// Tells the assembler to place the following code at the given address.
    /// Example: `[[addr(0x0E00)]]`
    Addr(u16),

    /// Tells the assembler to emit the given data bytes.
    /// Example: `[[data(0xe4, VTTY_ADDR, 0xaf)]]`
    Data(Vec<RValue>),
}
