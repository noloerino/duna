//! Some basic tests for the 64-bit variant of RISCV.
use duna_core::{
    architectures::riscv::{RiscVRegister, Rv64},
    assembler::Linker,
    program_state::Program,
};

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
    let mut program: Program<Rv64> = Linker::with_main_str(code)
        .link::<Rv64>(Default::default())
        .unwrap();
    program.run();
    assert_eq!(
        u64::from(program.state.regfile_read(RiscVRegister::A0)),
        0x0BCD_0123u64
    );
}

/// Tests the sext.w pseudo-instruction.
#[test]
fn test_sext_w() {
    let code = "
        li t1, 0xDEAD_BEEF
        li t2, 0x0123_4567
        sext.w a0, t1
        sext.w a1, t2
        ";
    let mut program: Program<Rv64> = Linker::with_main_str(code)
        .link::<Rv64>(Default::default())
        .unwrap();
    program.run();
    assert_eq!(
        u64::from(program.state.regfile_read(RiscVRegister::A0)),
        0xFFFF_FFFF_DEAD_BEEF
    );
    assert_eq!(
        u64::from(program.state.regfile_read(RiscVRegister::A1)),
        0x0123_4567
    );
}

/// Sanity checks instructions from the M extension; does not check overflow or sign extension
/// edge cases
#[test]
fn test_m_sanity() {
    let code = "
        li t0, 9
        li t1, 3
        mul a0, t0, t1   # a0 = 27
        mulw a1, t0, t1  # a1 = 27
        div a2, t0, t1   # a2 = 3
        divu a3, t0, t1  # a3 = 3
        divw a4, t0, t1  # a4 = 3
        divuw a5, t0, t1 # a5 = 3
        rem a6, t0, t1   # a6 = 0
        remu a7, t0, t1  # a7 = 0
        remw s0, t0, t1  # s0 = 0
        remuw s1, t0, t1 # s1 = 0
        # div by 0 => all 1s in rd
        div s2, t0, x0   # s2 = -1
        # rem by 0 => source in output
        rem s3, t0, x0   # s3 = 9
        ";
    let mut program: Program<Rv64> = Linker::with_main_str(code)
        .link::<Rv64>(Default::default())
        .unwrap();
    program.run();
    let state = program.state;
    assert_eq!(u64::from(state.regfile_read(RiscVRegister::A0)), 27);
    assert_eq!(u64::from(state.regfile_read(RiscVRegister::A1)), 27);
    assert_eq!(u64::from(state.regfile_read(RiscVRegister::A2)), 3);
    assert_eq!(u64::from(state.regfile_read(RiscVRegister::A3)), 3);
    assert_eq!(u64::from(state.regfile_read(RiscVRegister::A4)), 3);
    assert_eq!(u64::from(state.regfile_read(RiscVRegister::A5)), 3);
    assert_eq!(u64::from(state.regfile_read(RiscVRegister::A6)), 0);
    assert_eq!(u64::from(state.regfile_read(RiscVRegister::A7)), 0);
    assert_eq!(u64::from(state.regfile_read(RiscVRegister::S0)), 0);
    assert_eq!(u64::from(state.regfile_read(RiscVRegister::S1)), 0);
    assert_eq!(i64::from(state.regfile_read(RiscVRegister::S2)), -1);
    assert_eq!(i64::from(state.regfile_read(RiscVRegister::S3)), 9);
}

/// Sanity checks CSR instructions.
#[test]
fn test_csr_sanity() {
    let code = "
        li t0, 0x234
        csrrw a0, t0, 0xFF
        csrrs a0, x0, 0xFF
        li t0, 0xFF0
        csrrs x0, t0, 0xFF
        csrrs a1, x0, 0xFF
    ";
    let mut program: Program<Rv64> = Linker::with_main_str(code)
        .link::<Rv64>(Default::default())
        .unwrap();
    program.run();
    let state = program.state;
    assert_eq!(state.regfile_read(RiscVRegister::A0), 0x234u64.into());
    assert_eq!(state.regfile_read(RiscVRegister::A1), 0xFF4u64.into());
}
