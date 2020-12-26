# Tests that declaring main in the data section should fail.
.global main

.data
main: .asciz "no u\n"

.text
code:
    li a0, 100