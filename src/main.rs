// #![allow(dead_code)]
use clap::{App, Arg};
use duna::program_state::RiscVProgram;
use std::process;

fn main() {
    let matches = App::new("emulator")
        .version("0.1")
        .author("Jonathan Shi <jhshi@berkeley.edu>")
        .arg(
            // TODO allow using stdin
            Arg::with_name("INPUT")
                .help("The input assembly file to run.")
                .required(true)
                .min_values(1)
                .index(1),
        )
        .get_matches();
    let file_name = matches.value_of("INPUT").unwrap();
    let parse_result = RiscVProgram::from_file(file_name);
    let mut program = match parse_result {
        Ok(p) => p,
        Err(errs) => {
            errs.report();
            process::exit(1);
        }
    };
    let prog_exit_code = program.run();
    println!("{}", format!("Program exited with code {}", prog_exit_code));
}
