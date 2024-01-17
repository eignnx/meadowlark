//! Meadowlark is a language for writing Lark VM programs.

use std::{
    fs, io,
    path::{Path, PathBuf},
};

use clap::Parser;
use lalrpop_util::{lalrpop_mod, ParseError};

use crate::parse_lark::Token;

mod ast;
mod cli;
mod compile;
lalrpop_mod!(pub parse_lark);

fn main() {
    let opts = cli::CliOpts::parse();

    match opts.cmd {
        cli::Cmd::Run { src_path, debug } => run(&src_path, debug),
    }
}

fn run(path: &Path, debug: bool) {
    // Read in the source file.
    let src = std::fs::read_to_string(path).unwrap();

    // Parse.
    let parser = parse_lark::ProgramParser::new();
    let ast = parser.parse(&src).unwrap_or_else(|e| {
        display_error(e, path, &src);
        std::process::exit(1);
    });

    let basename = path.file_stem().unwrap();

    let asm_path = PathBuf::new()
        .join("target")
        .join(basename)
        .with_extension("lark.asm");

    {
        let mut out = io::BufWriter::new(fs::File::create(&asm_path).unwrap());
        let mut codegen = compile::CodeGen::new(Some(path.to_owned()));
        codegen.compile(&mut out, ast).unwrap();
    }

    let bin_path = PathBuf::new()
        .join("target")
        .join(basename)
        .with_extension("lark.bin");

    std::process::Command::new("customasm")
        .arg(&asm_path)
        .arg("-o")
        .arg(&bin_path)
        .status()
        .unwrap();

    let mut vm_cmd = std::process::Command::new("cargo");
    vm_cmd
        .arg("run")
        .args(["--package", "lark-vm"])
        .arg(&bin_path);

    if debug {
        vm_cmd.arg("--debug");
    }
    vm_cmd.status().unwrap();
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

pub fn display_error(e: ParseError<usize, Token, UserError>, path: &Path, src: &str) {
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
            eprintln!("Unexpected token [{}:{line}:{col}]:", path.display());
            eprintln!("\tExpected [{expected}] but found {:?}.", t.1);
        }

        ParseError::InvalidToken { location } => {
            let (line, col) = line_col(path, src, location);
            let snippet = &src[location..].chars().take(10).collect::<String>();
            eprintln!("Invalid token [{}:{line}:{col}]", path.display());
            eprintln!("\tToken begins `{snippet}…`.");
        }

        ParseError::UnrecognizedEof { location, expected } => {
            let (line, col) = line_col(path, src, location);
            let expected = expected
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(" or ");
            eprintln!("Unexpected end of file [{}:{line}:{col}]:", path.display());
            eprintln!("\tExpected [{expected}].", expected = expected);
        }

        ParseError::User {
            error:
                UserError {
                    msg,
                    span: (begin_idx, _),
                },
        } => {
            let (line, col) = line_col(path, src, begin_idx);
            eprintln!("Error [{}:{line}:{col}]:", path.display());
            eprintln!("\t{}", msg);
        }

        other_error => eprintln!(
            "Parse error:\n~~~~~~~~~~~~~~~{}\n~~~~~~~~~~~~~~\n{:#?}",
            other_error, other_error
        ),
    }
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
