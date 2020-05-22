use mars::program_state::RiscVProgram;
use std::path::Path;

fn program_from_file(filename: &str) -> RiscVProgram {
    RiscVProgram::from_file(
        Path::new("tests/asm_files")
            .join(filename)
            .to_str()
            .unwrap(),
    )
    .unwrap()
}

#[test]
/// Tests some basic I-type instructions.
fn test_simple() {
    let mut program = program_from_file("simple.s");
    let result = program.run();
    assert_eq!(result, 4);
}

#[test]
/// Tests basic aligned loads and stores.
fn test_basic_mem() {
    let mut program = program_from_file("basic_mem.s");
    program.dump_insts();
    let result = program.run();
    assert_eq!(result as u32, 0xABCD_0123u32);
}

#[test]
/// Tests li, mv, and nop pseudo-instructions.
fn test_pseudo() {
    let mut program = program_from_file("test_pseudo.s");
    program.dump_insts();
    let result = program.run();
    assert_eq!(result as u32, 0xFFEE_DDCCu32);
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
