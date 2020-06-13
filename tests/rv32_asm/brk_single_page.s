# Tests that a brk syscall will page in at least the page containing the designated address.
# Eventually, it should page in other addresses as well.
# Set up syscall
li s0, 0x4000_0000 # Address to page in
li a7, 214
mv a0, s0
ecall
# Assume this doesn't segfault
li t1, 0xDEAD_BEEF
sw t1, 0(s0)
lw a0, 0(s0)