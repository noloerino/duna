#![no_main]
use libfuzzer_sys::arbitrary;
use libfuzzer_sys::fuzz_target;
use mars::instruction::ITypeLoad;
use mars::isa::Lw;
use mars::program_state::{ByteAddress, DataWord, IRegister, ProgramState};

#[derive(Clone, Debug, arbitrary::Arbitrary)]
struct FuzzData {
    data: u32,
    addr: i32,
    offs: i8, // 12 bit immediate
    rs1: u8,
    rd: u8,
}

fuzz_target!(|input: FuzzData| {
    if input.rs1 > 0 && input.rd > 0 && input.rs1 < 32 && input.rd < 32 {
        let mut state = ProgramState::new();
        let rs1 = IRegister::from(input.rs1);
        let rd = IRegister::from(input.rd);
        state.regfile.set(rs1, DataWord::from(input.addr));
        state.memory.set_word(
            ByteAddress::from(input.addr.wrapping_add(input.offs as i32)).to_word_address(),
            DataWord::from(input.data),
        );
        let inst = Lw::new(rd, rs1, DataWord::from(input.offs as i32));
        state.apply_inst(&inst);
        assert_eq!(u32::from(state.regfile.read(rd)), input.data);
    }
});
