mod utils;

use duna::arch::Width32b;
use duna::architectures::riscv::{RiscV, RiscVRegister, RV32};
use duna::assembler::Linker;
use duna::program_state::ProgramState;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct SimResult {
    stdout: Vec<u8>,
    pub exit_code: u8,
    registers: RegFileView,
}

#[wasm_bindgen]
impl SimResult {
    pub fn get_stdout(&self) -> String {
        std::str::from_utf8(&self.stdout).unwrap().to_string()
    }

    pub fn get_register_view(&self) -> *const RegFileElem {
        self.registers.mapping.as_ptr()
    }

    pub fn get_register_view_size(&self) -> u8 {
        self.registers.mapping.len() as u8
    }
}

/// Runs the program from start to finish.
/// Hacky.
#[wasm_bindgen]
pub fn simulate(program: &str) -> SimResult {
    let mut program = Linker::with_main_str(program)
        .link::<RV32>(Default::default())
        .unwrap();
    let result = program.run();
    SimResult {
        stdout: program.state.get_stdout().to_vec(),
        exit_code: result,
        registers: RegFileView::from_rv32(&program.state),
    }
}

#[wasm_bindgen]
struct RegFileView {
    // Maps a regfile name to the value being displayed.
    mapping: Vec<RegFileElem>,
}

#[wasm_bindgen]
pub struct RegFileElem(String, String);

impl RegFileView {
    fn from_rv32(state: &ProgramState<RiscV<Width32b>, Width32b>) -> Self {
        let registers = RiscVRegister::REG_ARRAY;
        let mut mapping = Vec::new();
        for (i, r) in registers.iter().enumerate() {
            mapping.push(RegFileElem(
                format!("x{} ({})", i, r),
                format!("{}", state.regfile_read(*r)),
            ));
        }
        RegFileView { mapping }
    }
}

#[wasm_bindgen]
pub fn init() {
    utils::set_panic_hook();
}
