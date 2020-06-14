//! Some basic tests for the 64-bit variant of RISCV.
use duna::architectures::riscv::{RiscVRegister, RV64};
use duna::assembler::Linker;
use duna::program_state::Program;

/// Tests basic jump instructions.
/// This is identical to the 32-bit local_labels test, with the only difference being a different
/// target a0 value since the "lui/addi" version of li would inappropriately sign extend.
#[test]
fn test_jumps() {
    let code = "
        li a0, 0x0BCD_0122
        li t1, 1
        li t2, -1
        j actual_start
        # This line should be skipped
        bad: li a0, 0x0FFF_FFFF
        li a0, 0x0FFF_FFFE
        actual_start: 
        bge t1, zero, l1
        addi a0, zero, -1
        l1: beq zero, zero, l2
        addi a0, zero, -2
        l2:
        blt t2, t1, end
        addi a0, zero, -3
        end:
            addi a0, a0, 1
        ";
    let mut program: Program<RV64> = Linker::with_main_str(code)
        .link::<RV64>(Default::default())
        .unwrap();
    program.run();
    assert_eq!(
        u64::from(program.state.regfile_read(RiscVRegister::A0)),
        0x0BCD_0123u64
    );
}
