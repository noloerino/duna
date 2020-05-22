# Tests writing the string "deadbeef\n" to standard out.
# This test relies on sp being initialized to a value that is mapped in memory,
# and requires ecall to work.
li t1, 0x64616564
li t2, 0x66656562
li t3, 0xa
sw t1, 0(sp)
sw t2, 4(sp)
sw t3, 8(sp)
# Set up syscall
li a7, 1
li a0, 1
mv a1, sp
li a2, 9
ecall
