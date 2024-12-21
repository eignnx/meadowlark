use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser, Debug)]
pub struct CliOpts {
    #[clap(subcommand)]
    pub cmd: Cmd,
}

#[derive(Subcommand, Debug)]
pub enum Cmd {
    #[clap(name = "run")]
    Run {
        src_path: PathBuf,
        #[clap(short, long)]
        debug: bool,
    },
    // TODO:
    // #[clap(name = "build")]
    // Build(BuildOpts),
}
