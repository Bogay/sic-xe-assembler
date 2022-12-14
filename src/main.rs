mod command;
mod sic_xe;

#[macro_use]
extern crate pest_derive;
use std::io::stdout;

use crate::sic_xe::SicXeProgram;
use pest::Parser;

#[derive(Parser)]
#[grammar = "sic_xe.pest"]
struct SicXeParser;

fn main() {
    let s = include_str!("../test_input.asm");
    let pair = SicXeParser::parse(Rule::Program, s)
        .unwrap()
        .next()
        .unwrap();
    let program: SicXeProgram = SicXeProgram::try_from(pair).unwrap();

    program.prettry_print(stdout()).unwrap();
    println!("\n==========\n");
    println!("{}", program.opcode());
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
