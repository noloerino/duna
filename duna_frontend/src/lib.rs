mod utils;

use duna::architectures::riscv::RV32;
use duna::assembler::{Linker, ParseErrorReport, ParseErrorReporter};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct SimResult {
    stdout: Vec<u8>,
    compile_errs: ParseErrorReport,
    pub exit_code: u8,
}

#[wasm_bindgen]
impl SimResult {
    pub fn get_stdout(&self) -> String {
        std::str::from_utf8(&self.stdout).unwrap().to_string()
    }

    pub fn get_compile_errs(&self) -> String {
        format!("{:?}", self.compile_errs)
    }
}

#[wasm_bindgen]
/// Runs the program from start to finish.
/// Hacky.
pub fn simulate(program_text: &str) -> SimResult {
    match Linker::with_main_str(program_text).link::<RV32>(Default::default()) {
        Ok(mut program) => {
            let result = program.run();
            SimResult {
                stdout: program.state.get_stdout().to_vec(),
                compile_errs: ParseErrorReporter::new()
                    .into_report_with_file_map(Default::default()),
                exit_code: result,
            }
        }
        Err(errs) => SimResult {
            stdout: vec![],
            compile_errs: errs,
            exit_code: 255,
        },
    }
}

#[wasm_bindgen]
pub fn init() {
    utils::set_panic_hook();
}
