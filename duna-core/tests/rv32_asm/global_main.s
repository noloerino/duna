# Tests that when a global main label is declared, execution begins there
# rather than at the first instruction in the file.
#
# a0 should contain 100 rather than 110 at the end of the program.
.global main

fun:
    li a0, 10

main:
    addi a0, a0, 100
