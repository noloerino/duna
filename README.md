# duna
![](https://github.com/noloerino/duna/workflows/Rust/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/noloerino/duna/badge.svg?branch=master)](https://coveralls.io/github/noloerino/duna?branch=master)

An assembly simulator built in Rust.

## See also
- [Venus](https://github.com/ThaumicMekanism/venus)
- [SPIM](http://spimsimulator.sourceforge.net/)

## Current functionality
- Run by CLI with `cargo run <INPUT_FILE>`
- RISC-V
    - Supports most of RV32IM and RV64IM
    - Supports a few ecalls
- MIPS
    - WIP

## Roadmap
### RISC-V
- Finish multiplication extension
- Figure out which CSRs to support

### OS/Memory
- Fix page table/TLB lol
- Instruction decoding + instruction fetches
- Distinguish between IMEM/DMEM
- Interrupts?
- Kernel memory?
- Fence instructions
- Configurable cache hierarchy, store buffers

### Interface
- Standard library functions
- CLI configuration
- Support assembler [relocation functions](https://github.com/riscv/riscv-asm-manual/blob/master/riscv-asm.md#assembler-relocation-functions)
- Display regfile, memory, and cache info
- Ability to poke values in registers, memory, etc. (add sources/causes to diff structs?)
- Provide debugger support a la GDB, possibly valgrind-like tools as well?
- Implement peephole optimizations + visualizations

### Niceties
- Add hints/warnings
- Run [riscv compliance tests](https://github.com/riscv/riscv-compliance)

### Other ISAs?
- MIPS 32/64bit? whatever they're called?
- Atmel AVR
- ARM
- x86-64
- wasm
- some LLVM IR
