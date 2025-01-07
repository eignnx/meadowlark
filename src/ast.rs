use std::{collections::BTreeMap, fmt};

use lark_vm::cpu::regs::Reg;

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

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

impl BinOp {
    pub(crate) fn eval(&self, x: i32, y: i32) -> i32 {
        match self {
            BinOp::Add => x + y,
            BinOp::Sub => x - y,
            BinOp::Mul => x * y,
            BinOp::Div => x / y,
            BinOp::Mod => x % y,
            BinOp::And => x & y,
            BinOp::Or => x | y,
            BinOp::Xor => x ^ y,
            BinOp::Shl => x << y,
            BinOp::Shr => x >> y,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ConstValue {
    Uint(u16),
    Int(i16),
    Char(u8),
    ConstAlias(Var),
    BinOp(Box<ConstValue>, BinOp, Box<ConstValue>),
}

impl ConstValue {
    pub fn evaluate(&self, aliases: &BTreeMap<Var, ConstValue>) -> Option<i32> {
        match self {
            ConstValue::Uint(u) => Some(*u as i32),
            ConstValue::Int(i) => Some(*i as i32),
            ConstValue::Char(c) => Some(*c as i32),
            ConstValue::ConstAlias(alias) => {
                let value = aliases.get(alias)?;
                value.evaluate(aliases)
            }
            ConstValue::BinOp(lhs, op, rhs) => {
                let lhs = lhs.evaluate(aliases)?;
                let rhs = rhs.evaluate(aliases)?;
                Some(op.eval(lhs, rhs))
            }
        }
    }
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

#[derive(Debug, Clone)]
pub enum Base {
    Reg(Reg),
    Alias(Var),
}

impl fmt::Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Base::Reg(reg) => write!(f, "{}", reg),
            Base::Alias(var) => write!(f, "{}", var),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Offset {
    I10(i16),
    Const(Var),
    NegatedConst(Var),
}

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Offset::I10(i) => write!(f, "{}", i),
            Offset::Const(var) => write!(f, "+{}", var),
            Offset::NegatedConst(var) => write!(f, "-{}", var),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LValue {
    /// Example: `$t0`, `$rv`
    Reg(Reg),

    /// # Examples
    ///
    /// | Syntax                | Equivalent          |
    /// |:----------------------|--------------------:|
    /// | `[$a0]`               | `0($a0)`            |
    /// | `[some_alias]`        | `0(some_alias)`     |
    /// | `[$a0 + 4]`           | `4($a0)`            |
    /// | `[$a0 - 4]`           | `-4($a0)`           |
    /// | `[$a0 - MY_CONSTANT]` | `-MY_CONSTANT($a0)` |
    /// | `[some_arg + 4]`      | `4(some_arg)`       |
    Indirection {
        base: Option<Base>,
        offset: Option<Offset>,
    },
}

impl LValue {
    pub fn is_arg_reg(&self) -> bool {
        match self {
            LValue::Reg(r) => r.is_argument(),
            _ => false,
        }
    }
}

impl fmt::Display for LValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LValue::Reg(reg) => write!(f, "{}", reg),
            LValue::Indirection { base, offset } => {
                write!(f, "[")?;
                if let Some(base) = base {
                    write!(f, "{}", base)?;
                }
                if let Some(offset) = offset {
                    write!(f, " + {}", offset)?;
                }
                write!(f, "]")
            }
        }
    }
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
pub enum RValue {
    /// Example: `0xBEEF`, `123`, `0b00001111`, `0o2375`
    Uint(u16),

    /// Example: `+23`, `-23`
    Int(i16),

    /// Example: `'a'`, `'\n'`
    Char(u8),

    /// Example: `"hello\n"`
    String(String),

    /// Example: `@some_label`
    Label(String),

    /// Example: `MY_CONST`
    ConstAlias(Var),

    /// Example: `some_arg`
    Alias(Var),

    LValue(LValue),
}

impl fmt::Display for RValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RValue::Uint(u) => write!(f, "{u}"),
            RValue::Int(i) => write!(f, "{i:+}"),
            RValue::Char(byte) => write!(f, "{:?}", *byte as char),
            RValue::String(str) => write!(f, "{:?}", str),
            RValue::Label(lbl) => write!(f, "&{}", lbl),
            RValue::ConstAlias(name) => write!(f, "{name}"),
            RValue::Alias(name) => write!(f, "{name}"),
            RValue::LValue(lvalue) => write!(f, "{lvalue}"),
        }
    }
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
