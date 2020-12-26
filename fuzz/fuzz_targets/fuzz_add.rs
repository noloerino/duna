#![no_main]
use duna_core::instruction::RType;
use duna_core::isa::Add;
use duna_core::program_state::{DataWord, IRegister, ProgramState};
use libfuzzer_sys::arbitrary;
use libfuzzer_sys::fuzz_target;

#[derive(Clone, Debug, arbitrary::Arbitrary)]
struct FuzzData {
    rs1_val: i32,
    rs2_val: i32,
    rs1: u8,
    rs2: u8,
    rd: u8,
}

fuzz_target!(|input: FuzzData| {
    if input.rs1 != input.rs2 && input.rs1 < 32 && input.rs2 < 32 && input.rd < 32 {
        let mut state = ProgramState::new();
        let rs1 = IRegister::from(input.rs1);
        let rs2 = IRegister::from(input.rs2);
        let rd = IRegister::from(input.rd);
        let rs1_val = if rs1 == IRegister::ZERO {
            0
        } else {
            input.rs1_val
        };
        let rs2_val = if rs2 == IRegister::ZERO {
            0
        } else {
            input.rs2_val
        };
        state.regfile.set(rs1, DataWord::from(rs1_val));
        state.regfile.set(rs2, DataWord::from(rs2_val));
        let inst = Add::new(rd, rs1, rs2);
        state.apply_inst(&inst);
        assert_eq!(
            i32::from(state.regfile.read(rd)),
            if rd == IRegister::ZERO {
                0
            } else {
                rs1_val.wrapping_add(rs2_val)
            }
        );
    }
});
