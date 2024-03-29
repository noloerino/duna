use clap::{App, Arg};
use duna_core::arch::Architecture;
use duna_core::architectures::riscv::{Rv32, Rv64};
use duna_core::assembler::Linker;
use duna_core::config::AsmConfig;
use duna_core::program_state::{Program, ProgramExecutor};
use std::io;
use std::io::Write;
use std::process;

fn main() {
    let matches = App::new("duna")
        .version("0.0.1")
        .author("Jonathan Shi <jonathan.h.shi@gmail.com>")
        .arg(
            Arg::with_name("isa")
                .long("isa")
                .help("The instruction set architecture of the input assembly files.")
                .default_value("rv32")
                .possible_values(&["rv32", "rv64"]),
        )
        .arg(
            Arg::with_name("debugger")
                .long("debugger")
                .help("Launches the interactive command line debugger."),
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
    // TODO expose this
    let config: AsmConfig = Default::default();
    if matches.is_present("debugger") {
        match isa {
            "rv32" => link_and_repl::<Rv32>(config, linker),
            "rv64" => link_and_repl::<Rv64>(config, linker),
            _ => panic!("invalid ISA: {}", isa),
        }
    } else {
        match isa {
            "rv32" => link_and_run::<Rv32>(config, linker),
            "rv64" => link_and_run::<Rv64>(config, linker),
            _ => panic!("invalid ISA: {}", isa),
        }
    }
}

fn link_or_exit<A: Architecture>(config: AsmConfig, linker: Linker) -> Program<A> {
    let link_result = linker.link::<A>(config);
    match link_result {
        Ok(p) => p,
        Err(errs) => {
            errs.report();
            process::exit(1);
        }
    }
}

// Runs the program to completion.
fn link_and_run<A: Architecture>(config: AsmConfig, linker: Linker) {
    let mut program = link_or_exit::<A>(config, linker);
    let prog_exit_code = program.run();
    println!("Program exited with code {}", prog_exit_code);
}

// Runs a gdb-like repl.
fn link_and_repl<A: Architecture>(config: AsmConfig, linker: Linker) {
    let mut executor = ProgramExecutor::<A>::new(link_or_exit(config, linker));
    let mut exited = false;
    println!("Running debugger.");
    while !exited {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut raw_input = String::new();
        io::stdin().read_line(&mut raw_input).unwrap();
        let input = raw_input.trim();
        // for now, just match directly
        match input {
            "s" | "step" => {
                let curr_inst = executor.curr_inst();
                println!(
                    "{} @ {}",
                    if let Some(inst) = curr_inst {
                        format!("{}", inst)
                    } else {
                        "<no instruction>".to_string()
                    },
                    executor.program.state.get_pc()
                );
                executor.step();
                // println!("new PC: {}", executor.program.state.get_pc())
            }
            "c" | "continue" => {
                executor.step_to_completion(1000);
            }
            "info registers" | "i r" | "i registers" | "info r" => {
                println!("{}", executor.program.state.regfile())
            }
            // Because we called trim(), no need for new newline
            "" => (),
            "q" | "quit" => {
                exited = true;
            }
            _ => println!("Unrecognized commands."),
        }
    }
}
