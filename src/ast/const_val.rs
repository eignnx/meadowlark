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

pub enum ConstEvalError {
    UndefinedAlias(String),
    MaxEvalDepthExceeded,
}

impl ConstValue {
    pub(crate) const MAX_EVALUATION_DEPTH: usize = 64;

    /// Returns `None` if the const alias is undefined.
    pub fn evaluate(&self, aliases: &BTreeMap<String, ConstValue>) -> Result<i32, ConstEvalError> {
        self.evaluate_rec(aliases, 0)
    }

    fn evaluate_rec(
        &self,
        aliases: &BTreeMap<String, ConstValue>,
        depth: usize,
    ) -> Result<i32, ConstEvalError> {
        if depth > ConstValue::MAX_EVALUATION_DEPTH {
            return Err(ConstEvalError::MaxEvalDepthExceeded);
        }

        match self {
            ConstValue::Uint(u) => Ok(*u as i32),
            ConstValue::Int(i) => Ok(*i as i32),
            ConstValue::Char(c) => Ok(*c as i32),
            ConstValue::ConstAlias(alias) => {
                let value = aliases
                    .get(alias)
                    .ok_or_else(|| ConstEvalError::UndefinedAlias(alias.to_owned()))?;
                value.evaluate_rec(aliases, depth + 1)
            }
            ConstValue::BinOp(lhs, op, rhs) => {
                let lhs = lhs.evaluate_rec(aliases, depth + 1)?;
                let rhs = rhs.evaluate_rec(aliases, depth + 1)?;
                Ok(op.eval(lhs, rhs))
            }
        }
    }
}
