// #![allow(dead_code)]
#[macro_use]
extern crate lazy_static;
use crate::program_state::RiscVProgram;
use clap::{App, Arg};
use std::process;

mod instruction;
mod isa;
mod lexer;
mod parser;
mod program_state;
mod pseudo_inst;

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
    let parse_result = RiscVProgram::from_file(file_name);
    let mut program = match parse_result {
        Ok(p) => p,
        Err(errs) => {
            errs.iter().for_each(|e| println!("{}", e));
            process::exit(1);
        }
    };
    let prog_exit_code = program.run();
    println!("{}", format!("Program exited with code {}", prog_exit_code));
}
