mod utils;

use duna::architectures::riscv::RV32;
use duna::assembler::Linker;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct SimResult {
    stdout: Vec<u8>,
    pub exit_code: u8,
}

#[wasm_bindgen]
impl SimResult {
    pub fn get_stdout(&self) -> String {
        std::str::from_utf8(&self.stdout).unwrap().to_string()
    }
}

#[wasm_bindgen]
/// Runs the program from start to finish.
/// Hacky.
pub fn simulate(program: &str) -> SimResult {
    let mut program = Linker::with_main_str(program)
        .link::<RV32>(Default::default())
        .unwrap();
    let result = program.run();
    SimResult {
        stdout: program.state.get_stdout().to_vec(),
        exit_code: result,
    }
}

#[wasm_bindgen]
pub fn init() {
    utils::set_panic_hook();
}
