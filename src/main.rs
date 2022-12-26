mod sic_xe;

#[macro_use]
extern crate pest_derive;
use crate::sic_xe::SicXeProgram;
use clap::Parser as ClapParser;
use pest::Parser;
use std::{fs, io::stdout, path::PathBuf};

#[derive(Parser)]
#[grammar = "sic_xe.pest"]
struct SicXeParser;

#[derive(clap::Parser)]
struct Cli {
    /// Path to source code
    file: PathBuf,
    /// Print object program
    #[arg(long)]
    object: bool,
}

fn main() {
    let cli = Cli::parse();
    let s = fs::read_to_string(cli.file).expect("Failed to reading input file");
    let pair = SicXeParser::parse(Rule::Program, &s)
        .unwrap()
        .next()
        .unwrap();
    let program = SicXeProgram::try_from(pair).unwrap();

    if cli.object {
        println!("{}", program.object_code());
    } else {
        program.pretty_print(stdout()).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        SicXeParser::parse(Rule::String, "C'EOF'").unwrap();
    }

    #[test]
    fn test_expr() {
        SicXeParser::parse(Rule::Expression, "COPY\tSTART\t0").unwrap();
    }

    #[test]
    fn test_program() {
        SicXeParser::parse(Rule::Program, "COPY\tSTART\t0\n").unwrap();
        SicXeParser::parse(Rule::Program, include_str!("../test_input.asm")).unwrap();
    }
}
