use duna::arch::*;
use duna::architectures::riscv::{RiscVProgram, RV32};
use duna::assembler::{Linker, ParseErrorReport};
use duna::program_state::Program;
use std::path::Path;

fn get_full_test_path(relative_path: &str) -> String {
    Path::new("tests/rv32_asm")
        .join(relative_path)
        .to_str()
        .unwrap()
        .to_string()
}

fn program_from_file(filename: &str) -> RiscVProgram<Width32b> {
    let program: RiscVProgram<Width32b> = Linker::with_main(&get_full_test_path(filename))
        .link::<RV32>(Default::default())
        .unwrap();
    // stdout is suppressed unless a test fails
    program.dump_insts();
    program
}

fn err_report_from_files(main_filename: &str, others: Vec<&str>) -> ParseErrorReport {
    let mut linker = Linker::with_main(&get_full_test_path(main_filename));
    for file in others {
        linker = linker.with_file(&get_full_test_path(file));
    }
    let report = linker
        .link::<RV32>(Default::default())
        .err() // needed because RiscVProgram is not Debug
        .expect("linker did not error when it should have");
    report.report();
    report
}

/// Runs a test that checks the value left in register a0 after running the program.
fn check_a0_at_end(filename: &str, exp_a0: u32) {
    let mut program = program_from_file(filename);
    let result = program.run();
    assert_eq!(result as u32, exp_a0);
}

/// Tests some basic I-type instructions.
#[test]
fn test_simple() {
    check_a0_at_end("simple.s", 4);
}

/// Tests basic aligned loads and stores.
#[test]
fn test_basic_mem() {
    check_a0_at_end("basic_mem.s", 0xABCD_0123u32);
}

/// Tests li, mv, and nop pseudo-instructions.
#[test]
fn test_pseudo() {
    check_a0_at_end("pseudo.s", 0xFFEE_DDCCu32);
}

/// Tests j, ret, and the non-pseudo version of jalr.
/// Uses only relative offsets.
#[test]
fn test_pseudo_jumps() {
    check_a0_at_end("pseudo_jumps.s", 0xDEAD);
}

/// Tests some branches to offsets, again using only relative offsets.
#[test]
fn test_offset_branches() {
    check_a0_at_end("offset_branches.s", 0xABCD_0123u32);
}

/// Tests the write syscall with some nice happy ASCII characters.
#[test]
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

/// Tests jumping and branching to locally defined labels.
#[test]
fn test_local_labels() {
    check_a0_at_end("local_labels.s", 0xABCD_0123u32);
}

/// Tests linking two files that have no label dependencies.
#[test]
fn test_basic_link() {
    let mut program = Linker::with_main(&get_full_test_path("local_labels.s"))
        .link::<RV32>(Default::default())
        .unwrap();
    program.dump_insts();
    assert_eq!(program.run() as u32, 0xABCD_0123u32);
}

/// Tests linking files that require global symbols.
#[test]
fn test_global_link() {
    let mut program = Linker::with_main(&get_full_test_path("global_link_0.s"))
        .with_file(&get_full_test_path("global_link_1.s"))
        .link::<RV32>(Default::default())
        .unwrap();
    program.dump_insts();
    assert_eq!(program.run(), 0x1234);
}

/// Tests reporting errors in multiple linked files.
#[test]
fn test_link_multi_err() {
    let report = err_report_from_files("parse_err_0.s", vec!["parse_err_1.s"]);
    let errs = report.get_errs();
    assert_eq!(errs.len(), 2);
    let report_string = format!("{:?}", report);
    assert!(report_string.contains("parse_err_0.s"));
    // check contents of errant line of first file
    assert!(report_string.contains("addi"));
    assert!(report_string.contains("parse_err_1.s"));
    // check contents of errant line of first file
    assert!(report_string.contains("jal unknown"));
}

/// Tests that a redefined label gets reported.
#[test]
fn test_redefined_label() {
    let report = err_report_from_files("redefined_label_0.s", vec!["redefined_label_1.s"]);
    let errs = report.get_errs();
    assert_eq!(errs.len(), 2);
    let report_string = format!("{:?}", report);
    // ensure that the error occurred on the second definition of the local label
    assert!(report_string.contains("redefined_label_0.s:3:0"));
    assert!(report_string.contains("bad_label"));
    // ensure that the error occurred on the second definition
    assert!(report_string.contains("redefined_label_1.s:3:0"));
    assert!(report_string.contains("end"));
}

// /// Tests labels for literal values declared by directive.
// #[test]
// fn test_directive_labels() {
//     let mut program = program_from_file("directive_labels.s");
//     // should have written 12 bytes
//     let result = program.run();
//     assert_eq!(result, 12);
//     assert_eq!(
//         String::from_utf8(program.state.get_stdout().to_vec()),
//         Ok("hello world\n".to_string())
//     );
// }
