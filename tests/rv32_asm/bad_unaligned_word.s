# Tests that an unaligned load should raise a bus error.
# Assumes the page at the stack pointer is on paged in.
addi sp, sp, -8
lw a0, 1(sp)
