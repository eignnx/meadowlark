use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub enum ConstValue {
    Uint(u16),
    Int(i16),
    Char(u8),
    ConstAlias(String),
    BinOp(Box<ConstValue>, BinOp, Box<ConstValue>),
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

impl ConstValue {
    /// Returns `None` if the const alias is undefined.
    pub fn evaluate(&self, aliases: &BTreeMap<String, ConstValue>) -> Option<i32> {
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
