use crate::lexer::Lexer;
use crate::parser::RiscVParser;
use crate::program_state::{IRegister, RiscVProgram};
use clap::{App, Arg};
use std::process;

mod instruction;
mod isa;
mod lexer;
mod parser;
mod program_state;

fn main() {
    let matches = App::new("emulator")
        .version("0.1")
        .author("Jonathan Shi <jhshi@berkeley.edu>")
        .arg(
            Arg::with_name("INPUT")
                .help("The input assembly file to run.")
                .required(true)
                .index(1),
        )
        .get_matches();
    let file_name = matches.value_of("INPUT").unwrap();
    let (toks, lex_errs) = Lexer::from_file(file_name).lex();
    let (insts, parse_errs) = RiscVParser::from_tokens(toks).parse();
    let mut all_errs = lex_errs;
    all_errs.extend(parse_errs);
    if !all_errs.is_empty() {
        all_errs.iter().for_each(|e| println!("{}", e));
        process::exit(1);
    }
    let mut program = RiscVProgram::new(insts);

    for inst in &program.insts {
        program.state.apply_inst(inst);
    }
    println!(
        "{}",
        format!(
            "Program exited with code {}",
            program.state.regfile.read(IRegister::A0)
        )
    );
}
