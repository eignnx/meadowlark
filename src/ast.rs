use std::fmt;

#[derive(Debug)]
pub enum Stmt {
    Label(String),
    Instr(Instr),
    While {
        test_reg: Reg,
        test_cond: Vec<Instr>,
        body: Vec<Stmt>,
    },
}

#[derive(Debug)]
pub struct Instr {
    pub op: String,
    pub args: Vec<Arg>,
}

#[derive(Debug)]
pub enum Arg {
    Uint(u16),
    Int(i16),
    Label(String),
    Reg(Reg),
    Offset(i16, Reg),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg {
    Zero,
    Rv,
    Ra,
    Arg(u8),
    Saved(u8),
    Temp(u8),
    Kernel(u8),
    Gp,
    Sp,
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reg::Zero => write!(f, "$zero"),
            Reg::Rv => write!(f, "$rv"),
            Reg::Ra => write!(f, "$ra"),
            Reg::Arg(n) => write!(f, "$a{}", n),
            Reg::Saved(n) => write!(f, "$s{}", n),
            Reg::Temp(n) => write!(f, "$t{}", n),
            Reg::Kernel(n) => write!(f, "$k{}", n),
            Reg::Gp => write!(f, "$gp"),
            Reg::Sp => write!(f, "$sp"),
        }
    }
}
