# Tests that alignment is preserved in data label declarations across files.
# Run from aligned_directive_labels_0.s.
.globl l5
.globl l6
.globl l7
.data
    l5: .byte 0xEE
    l6: .half 0xFEDC
    l7: .word 0x9876_5432