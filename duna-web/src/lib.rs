mod utils;

use duna_core::{
    architectures::riscv::RV32,
    assembler::{ErrorReport, Linker},
    config::AsmConfig,
    data_structures::ByteAddr32,
    program_state::ProgramExecutor,
};
use wasm_bindgen::prelude::*;

/// A snapshot of a paused simulation.
#[wasm_bindgen]
pub struct SimSnapshot {
    curr_pc: ByteAddr32,
    curr_inst: String,
    reg_dump: String,
}

#[wasm_bindgen]
impl SimSnapshot {
    pub fn curr_pc(&self) -> String {
        format!("{}", self.curr_pc)
    }

    pub fn curr_inst(&self) -> String {
        self.curr_inst.clone()
    }

    pub fn reg_dump(&self) -> String {
        self.reg_dump.clone()
    }
}

// TODO wasm bindgen doesn't like type parameters, so eventually
// with more architectures, so eventually the solution will be to wrap instances
// of the generic struct in an enum
// and likely to define a parameterized trait with concrete behaviors
#[wasm_bindgen]
pub struct SimState {
    assemble_result: Option<Result<ProgramExecutor<RV32>, ErrorReport>>,
    exit_code: Option<u8>,
}

impl Default for SimState {
    fn default() -> Self {
        Self::new()
    }
}

#[wasm_bindgen]
impl SimState {
    pub fn new() -> Self {
        SimState {
            assemble_result: None,
            exit_code: None,
        }
    }

    pub fn assemble(&mut self, program_text: &str) {
        self.assemble_result = Some(
            Linker::with_main_str(program_text)
                .link::<RV32>(AsmConfig::default())
                .map(ProgramExecutor::new),
        );
    }

    fn executor_mut(&mut self) -> Option<&mut ProgramExecutor<RV32>> {
        self.assemble_result
            .as_mut()?
            .as_mut() // convert Result<Program> to Result<&mut Program>
            .ok() // convert Result<&mut Program> to Option<&mut Program>
    }

    fn executor_ref(&self) -> Option<&ProgramExecutor<RV32>> {
        self.assemble_result
            .as_ref()?
            .as_ref() // convert Result<Program> to Result<&Program>
            .ok() // convert Result<&Program> to Option<&Program>
    }

    /// Runs the program from start to finish if the program was already assembled.
    pub fn run(&mut self) {
        if let Some(executor) = self.executor_mut() {
            let result = executor.run();
            self.exit_code = Some(result)
        }
    }

    pub fn snapshot(&self) -> Option<SimSnapshot> {
        let executor = self.executor_ref()?;
        Some(SimSnapshot {
            curr_pc: executor.state().get_pc(),
            curr_inst: if let Some(inst) = executor.curr_inst() {
                format!("{} ({:#X})", inst, inst)
            } else {
                "<no instruction>".to_string()
            },
            reg_dump: format!("{}", executor.state().regfile()),
        })
    }

    /// Steps through one instruction in the program, returning a snapshot of the state after the
    /// step. If the program terminated, then the sim_result field is updated accordingly.
    pub fn step(&mut self) {
        if let Some(executor) = self.executor_mut() {
            self.exit_code = executor.step();
        };
    }

    pub fn result(&self) -> Option<u8> {
        self.exit_code
    }

    pub fn stdout(&self) -> Option<String> {
        self.executor_ref().map(|executor| {
            std::str::from_utf8(&executor.state().get_stdout().to_vec())
                .unwrap()
                .to_string()
        })
    }

    pub fn get_errors(&self) -> Option<String> {
        if let Err(errs) = self.assemble_result.as_ref()? {
            Some(format!("{:?}", errs))
        } else {
            None
        }
    }

    pub fn reset(&mut self) {
        if let Some(executor) = self.executor_mut() {
            executor.reset()
        }
        self.exit_code = None;
    }
}

#[wasm_bindgen]
pub fn init() {
    utils::set_panic_hook();
}
