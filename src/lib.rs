//! Meadowlark is a language for writing Lark VM programs.

use std::{
    fmt, fs, io,
    path::{Path, PathBuf},
};

use lalrpop_util::{lalrpop_mod, ParseError};

use crate::parse_lark::Token;

pub mod ast;
pub mod cli;
pub mod compile;
lalrpop_mod!(pub parse_lark);

pub enum CompilationErr {
    ParseError(String),
    IoError(io::Error),
    AssemblerError(String),
}

impl std::fmt::Display for CompilationErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilationErr::ParseError(s) => write!(f, "Parse error: {}", s),
            CompilationErr::IoError(e) => write!(f, "IO error: {}", e),
            CompilationErr::AssemblerError(s) => write!(f, "Assembler error: {}", s),
        }
    }
}

impl From<io::Error> for CompilationErr {
    fn from(e: io::Error) -> Self {
        CompilationErr::IoError(e)
    }
}

pub fn compile(path: &Path, debug: bool) -> Result<PathBuf, CompilationErr> {
    // Read in the source file.
    let src = std::fs::read_to_string(path)?;

    // Parse.
    let parser = parse_lark::ProgramParser::new();
    let ast = parser.parse(&src).map_err(|e| {
        CompilationErr::ParseError(
            display_error(e, path, &src).expect("fmt::Write should not fail"),
        )
    })?;

    let basename = path.file_stem().ok_or_else(|| {
        CompilationErr::IoError(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Invalid filename",
        ))
    })?;

    let asm_path = PathBuf::new()
        .join("target")
        .join(basename)
        .with_extension("lark.asm");

    {
        let mut out = io::BufWriter::new(fs::File::create(&asm_path)?);
        let mut codegen = compile::CodeGen::new(Some(path.to_owned()));
        codegen.compile(&mut out, ast)?;
    }

    let bin_path = PathBuf::new()
        .join("target")
        .join(basename)
        .with_extension("lark.bin");

    let output = std::process::Command::new("customasm")
        .arg(&asm_path)
        .arg("-o")
        .arg(&bin_path)
        .output()?;

    if !output.status.success() {
        return Err(CompilationErr::AssemblerError(
            String::from_utf8_lossy(&output.stderr).to_string(),
        ));
    }

    Ok(bin_path)
}

#[derive(Debug)]
pub struct UserError {
    pub msg: String,
    pub span: (usize, usize),
}

impl std::fmt::Display for UserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[<unknown>:{}-{}]: {}",
            self.span.0, self.span.1, self.msg
        )
    }
}

pub fn display_error(
    e: ParseError<usize, Token, UserError>,
    path: &Path,
    src: &str,
) -> Result<String, fmt::Error> {
    use fmt::Write;
    let mut err = String::new();
    match e {
        ParseError::UnrecognizedToken {
            token: (l, t, _r),
            expected,
        } => {
            let (line, col) = line_col(path, src, l);
            let expected = expected
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(" or ");
            writeln!(err, "Unexpected token [{}:{line}:{col}]:", path.display())?;
            writeln!(err, "\tExpected [{expected}] but found {:?}.", t.1)?;
        }

        ParseError::InvalidToken { location } => {
            let (line, col) = line_col(path, src, location);
            let snippet = &src[location..].chars().take(10).collect::<String>();
            writeln!(err, "Invalid token [{}:{line}:{col}]", path.display())?;
            writeln!(err, "\tToken begins `{snippet}…`.")?;
        }

        ParseError::UnrecognizedEof { location, expected } => {
            let (line, col) = line_col(path, src, location);
            let expected = expected
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(" or ");
            writeln!(
                err,
                "Unexpected end of file [{}:{line}:{col}]:",
                path.display()
            )?;
            writeln!(err, "\tExpected [{expected}].", expected = expected)?;
        }

        ParseError::User {
            error:
                UserError {
                    msg,
                    span: (begin_idx, _),
                },
        } => {
            let (line, col) = line_col(path, src, begin_idx);
            writeln!(err, "Error [{}:{line}:{col}]:", path.display())?;
            writeln!(err, "\t{}", msg)?;
        }

        other_error => writeln!(
            err,
            "Parse error:\n~~~~~~~~~~~~~~~{}\n~~~~~~~~~~~~~~\n{:#?}",
            other_error, other_error
        )?,
    }
    Ok(err)
}

fn line_col(filename: &Path, src: &str, pos: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    for (i, c) in src.char_indices() {
        if i == pos {
            return (line, col);
        }
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    panic!("pos {} out of range for {}", pos, filename.display());
}
