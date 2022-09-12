mod utils;

use duna_core::{
    architectures::mips::Mips32,
    architectures::riscv::Rv32,
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

#[wasm_bindgen]
pub enum ActiveArch {
    Rv32,
    Mips32,
}

#[wasm_bindgen]
impl ActiveArch {
    fn from_str(s: &str) -> Option<ActiveArch> {
        match s {
            "rv32i" => Some(ActiveArch::Rv32),
            "mips32" => Some(ActiveArch::Mips32),
            _ => None,
        }
    }
}

/// Because wasm doesn't support complex enums, we mimic its functionality
/// by having a bunch of Option fields. `active` is the discriminant.
#[wasm_bindgen]
pub struct Executor {
    active: ActiveArch,
    rv32i: Option<ProgramExecutor<Rv32>>,
    mips32: Option<ProgramExecutor<Mips32>>,
}

impl Executor {
    fn get_state_snapshot(&self) -> SimSnapshot {
        match self.active {
            ActiveArch::Rv32 => {
                let executor = self.rv32i.as_ref().unwrap();
                SimSnapshot {
                    curr_pc: executor.state().get_pc(),
                    curr_inst: if let Some(inst) = executor.curr_inst() {
                        format!("{} ({:#X})", inst, inst)
                    } else {
                        "<no instruction>".to_string()
                    },
                    reg_dump: format!("{}", executor.state().regfile()),
                }
            }
            ActiveArch::Mips32 => {
                let executor = self.mips32.as_ref().unwrap();
                SimSnapshot {
                    curr_pc: executor.state().get_pc(),
                    curr_inst: if let Some(inst) = executor.curr_inst() {
                        format!("{} ({:#X})", inst, inst)
                    } else {
                        "<no instruction>".to_string()
                    },
                    reg_dump: format!("{}", executor.state().regfile()),
                }
            }
        }
    }

    fn get_stdout(&self) -> &[u8] {
        match self.active {
            ActiveArch::Rv32 => self.rv32i.as_ref().unwrap().state().get_stdout(),
            ActiveArch::Mips32 => self.mips32.as_ref().unwrap().state().get_stdout(),
        }
    }

    fn link(active_str: &str, program_text: &str) -> Result<Executor, ErrorReport> {
        let active = ActiveArch::from_str(active_str).unwrap();
        Ok(match active {
            ActiveArch::Rv32 => Executor {
                active,
                rv32i: Some(
                    Linker::with_main_str(program_text)
                        .link::<Rv32>(AsmConfig::default())
                        .map(ProgramExecutor::new)?,
                ),
                mips32: None,
            },
            ActiveArch::Mips32 => Executor {
                active,
                rv32i: None,
                mips32: Some(
                    Linker::with_main_str(program_text)
                        .link::<Mips32>(AsmConfig::default())
                        .map(ProgramExecutor::new)?,
                ),
            },
        })
    }

    fn revert(&mut self) -> Option<()> {
        match self.active {
            ActiveArch::Rv32 => self.rv32i.as_mut().unwrap().revert(),
            ActiveArch::Mips32 => self.mips32.as_mut().unwrap().revert(),
        }
    }

    fn reset(&mut self) {
        match self.active {
            ActiveArch::Rv32 => self.rv32i.as_mut().unwrap().reset(),
            ActiveArch::Mips32 => self.mips32.as_mut().unwrap().reset(),
        }
    }

    fn run(&mut self) -> u8 {
        match self.active {
            ActiveArch::Rv32 => self.rv32i.as_mut().unwrap().run(),
            ActiveArch::Mips32 => self.mips32.as_mut().unwrap().run(),
        }
    }

    fn step(&mut self) -> Option<u8> {
        match self.active {
            ActiveArch::Rv32 => self.rv32i.as_mut().unwrap().step(),
            ActiveArch::Mips32 => self.mips32.as_mut().unwrap().step(),
        }
    }
}

// TODO wasm bindgen doesn't like type parameters, so eventually
// with more architectures, so eventually the solution will be to wrap instances
// of the generic struct in an enum
// and likely to define a parameterized trait with concrete behaviors
#[wasm_bindgen]
pub struct SimState {
    // TODO dedup this field from Executor
    active_arch_str: String,
    assemble_result: Option<Result<Executor, ErrorReport>>,
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
            // TODO connect this to frontend
            active_arch_str: "rv32i".to_string(),
            assemble_result: None,
            exit_code: None,
        }
    }

    pub fn reset_to_arch(&mut self, arch_id: &str) {
        self.active_arch_str = arch_id.to_string();
        self.assemble_result = None;
    }

    pub fn assemble(&mut self, program_text: &str) {
        self.assemble_result = Some(Executor::link(&self.active_arch_str, program_text));
    }

    fn executor_mut(&mut self) -> Option<&mut Executor> {
        self.assemble_result
            .as_mut()?
            .as_mut() // convert Result<Program> to Result<&mut Program>
            .ok() // convert Result<&mut Program> to Option<&mut Program>
    }

    fn executor_ref(&self) -> Option<&Executor> {
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
        Some(self.executor_ref()?.get_state_snapshot())
    }

    pub fn revert(&mut self) {
        if let Some(executor) = self.executor_mut() {
            _ = executor.revert();
        }
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
            std::str::from_utf8(executor.get_stdout())
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
