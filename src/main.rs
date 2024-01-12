//! Meadowlark is a language for writing Lark VM programs.

use crate::parse_lark::Token;
use lalrpop_util::ParseError;
use std::path::{Path, PathBuf};

mod ast;
mod compile;
mod parse_lark;

fn main() {
    let path = PathBuf::from(std::env::args().nth(1).unwrap());
    let src = std::fs::read_to_string(&path).unwrap();
    let parser = parse_lark::ProgramParser::new();
    let ast = match parser.parse(&src) {
        Ok(ast) => {
            // for stmt in ast {
            //     println!("{:?}", stmt);
            // }
            ast
        }
        Err(e) => {
            display_error(e, &path, &src);
            std::process::exit(1);
        }
    };

    let mut out = String::new();
    let mut codegen = compile::CodeGen::new(path);
    codegen.compile(&mut out, ast).unwrap();
    println!("`````````````````");
    println!("{out}");
    println!("`````````````````");
}

fn display_error<E>(e: ParseError<usize, Token, E>, path: &Path, src: &str)
where
    E: std::fmt::Display + std::fmt::Debug,
{
    match e {
        ParseError::UnrecognizedToken {
            token: (l, t, _r),
            expected,
        } => {
            let (line, col) = line_col(&path, &src, l);
            let expected = expected
                .iter()
                .map(|s| format!("{}", s))
                .collect::<Vec<_>>()
                .join(" or ");
            println!("Unexpected token [{}:{line}:{col}]:", path.display());
            println!("\tExpected [{expected}] but found {:?}.", t.1);
        }

        ParseError::InvalidToken { location } => {
            let (line, col) = line_col(&path, &src, location);
            let snippet = &src[location..].chars().take(10).collect::<String>();
            println!("Invalid token [{}:{line}:{col}]", path.display());
            println!("\tToken begins `{snippet}…`.");
        }

        ParseError::UnrecognizedEof { location, expected } => {
            let (line, col) = line_col(&path, &src, location);
            let expected = expected
                .iter()
                .map(|s| format!("{}", s))
                .collect::<Vec<_>>()
                .join(" or ");
            println!("Unexpected end of file [{}:{line}:{col}]:", path.display());
            println!("\tExpected [{expected}].", expected = expected);
        }

        other_error => println!(
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
