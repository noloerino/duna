// #![allow(dead_code)]
use clap::{App, Arg};
use duna::arch::Architecture;
use duna::architectures::riscv::{RV32, RV64};
use duna::assembler::Linker;
use duna::config::AsmConfig;
use std::process;

fn main() {
    let matches = App::new("duna")
        .version("0.1")
        .author("Jonathan Shi <jhshi@berkeley.edu>")
        .arg(
            Arg::with_name("isa")
                .long("isa")
                .help("The instruction set architecture of the input assembly files.")
                .required(false)
                .takes_value(true)
                .default_value("rv32")
                .possible_values(&["rv32", "rv64"]),
        )
        .arg(
            // TODO allow using stdin
            Arg::with_name("INPUT")
                .help("The input assembly file to run.")
                .required(true)
                .min_values(1)
                .index(1),
        )
        .get_matches();
    let isa = matches.value_of("isa").unwrap();
    let mut file_names = matches.values_of("INPUT").unwrap();
    // min_values was set to 1, so this is guaranteed
    let main_path = file_names.next().unwrap();
    let mut linker = Linker::with_main(main_path);
    for file in file_names {
        linker = linker.with_file(file);
    }
    let config: AsmConfig = Default::default();
    match isa {
        "rv32" => link_and_run::<RV32>(config, linker),
        "rv64" => link_and_run::<RV64>(config, linker),
        _ => panic!("invalid ISA: {}", isa),
    }
}

fn link_and_run<A: Architecture>(config: AsmConfig, linker: Linker) {
    let link_result = linker.link::<A>(config);
    let mut program = match link_result {
        Ok(p) => p,
        Err(errs) => {
            errs.report();
            process::exit(1);
        }
    };
    let prog_exit_code = program.run();
    println!("{}", format!("Program exited with code {}", prog_exit_code));
}
