use clap::Parser;

use meadowlark::{self, cli};

fn main() {
    let opts = cli::CliOpts::parse();

    match opts.cmd {
        cli::Cmd::Run { src_path, debug } => match meadowlark::compile(&src_path, debug) {
            Ok(path) => println!("{}", path.display()),
            Err(e) => println!("{e}"),
        },
    }
}
