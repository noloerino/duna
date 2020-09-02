# Tests the ability to reference a label that starts with a period.
# Otherwise mostly identical to directive_labels.s besides a short jump at the end.
.data
.stuff:
    .string "hello world\n"
.text
    # Set up syscall
    li a7, 64
    li a0, 1
    la a1, .stuff
    li a2, 12
    j end
end:
    ecall
