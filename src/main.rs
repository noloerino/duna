// #![allow(dead_code)]
use clap::{App, Arg};
use duna::arch::Width32b;
use duna::architectures::riscv::arch::RiscV;
use duna::assembler::Linker;
use duna::program_state::Program;
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
    let mut file_names = matches.values_of("INPUT").unwrap();
    // min_values was set to 1, so this is guaranteed
    let main_path = file_names.next().unwrap();
    let mut linker = Linker::with_main(main_path);
    for file in file_names {
        linker = linker.with_file(file);
    }
    let link_result = linker.link::<RiscV<Width32b>>();
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
