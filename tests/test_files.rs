use duna::assembler::Linker;
use duna::program_state::{RiscVProgram, Width32b};
use std::path::Path;

fn program_from_file(filename: &str) -> RiscVProgram<Width32b> {
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
    check_a0_at_end("offset_branches.s", 0xABCD_0123u32);
}

#[test]
/// Tests the write syscall with some nice happy ASCII characters.
fn test_write_stdout() {
    // this will print "deadbeef\n" to stdout
    let mut program = program_from_file("write_stdout.s");
    // should have written 9 bytes
    let result = program.run();
    assert_eq!(result, 9);
    assert_eq!(
        String::from_utf8(program.state.get_stdout().to_vec()),
        Ok("deadbeef\n".to_string())
    );
}

#[test]
/// Tests jumping and branching to locally defined labels.
fn test_local_labels() {
    check_a0_at_end("local_labels.s", 0xABCD_0123u32);
}

#[test]
/// Tests linking two files that have no label dependencies.
fn test_basic_link() {
    let mut program = Linker::with_main("tests/asm_files/local_labels.s")
        .with_file("tests/asm_files/local_labels.s")
        .link()
        .unwrap();
    program.dump_insts();
    assert_eq!(program.run() as u32, 0xABCD_0123u32);
}

#[test]
/// Tests linking files that require global symbols.
fn test_global_link() {
    let mut program = Linker::with_main("tests/asm_files/global_link_0.s")
        .with_file("tests/asm_files/global_link_1.s")
        .link()
        .unwrap();
    program.dump_insts();
    assert_eq!(program.run(), 0x1234);
}
