use mars::program_state::RiscVProgram;
use std::path::Path;

fn program_from_file(filename: &str) -> RiscVProgram {
    let program = RiscVProgram::from_file(
        Path::new("tests/asm_files")
            .join(filename)
            .to_str()
            .unwrap(),
    )
    .unwrap();
    // stdout is suppressed unless a test fails
    program.dump_insts();
    program
}

/// Runs a test that checks the value left in register a0 after running the program.
fn check_a0_at_end(filename: &str, exp_a0: u32) {
    let mut program = program_from_file(filename);
    let result = program.run();
    assert_eq!(result as u32, exp_a0);
}

#[test]
/// Tests some basic I-type instructions.
fn test_simple() {
    check_a0_at_end("simple.s", 4);
}

#[test]
/// Tests basic aligned loads and stores.
fn test_basic_mem() {
    check_a0_at_end("basic_mem.s", 0xABCD_0123u32);
}

#[test]
/// Tests li, mv, and nop pseudo-instructions.
fn test_pseudo() {
    check_a0_at_end("pseudo.s", 0xFFEE_DDCCu32);
}

#[test]
/// Tests j, ret, and the non-pseudo version of jalr.
/// Uses only relative offsets.
fn test_pseudo_jumps() {
    check_a0_at_end("pseudo_jumps.s", 0xDEAD);
}

#[test]
/// Tests some branches to offsets, again using only relative offsets.
fn test_offset_branches() {
    check_a0_at_end("offset_branches.s", 0xABCD_0123u32)
}

// #[test]
// /// Tests the write syscall with some nice happy ASCII characters.
// fn test_write_stdout() {
//     // this will print "deadbeef\n" to stdout
//     let mut program = program_from_file("write_stdout.s");
//     // should have written 9 bytes
//     let result = program.run();
//     assert_eq!(result, 9);
//     assert_eq!(
//         String::from_utf8(program.state.stdout),
//         Ok("deadbeef\n".to_string())
//     );
// }
