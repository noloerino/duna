# Tests the usage of labels to refer to literal values.
.data
message:
    .string "hello world\n"
.text
    # Set up syscall
    li a7, 64
    li a0, 1
    la a1, message
    li a2, 12
    ecall
