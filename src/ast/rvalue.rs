use core::fmt;

use super::{lvalue::LValue, Var};

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
            RValue::LValue(lvalue) => write!(f, "{lvalue}"),
        }
    }
}
