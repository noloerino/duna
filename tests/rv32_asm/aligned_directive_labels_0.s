# Tests that alignment is preserved when combinging multiple data directives.
# Some labels are declared in aligned_directive_labels_1.s
# Registers should hold the following values on completion:
# - a0: 0xAB
# - a1: 0xABCD
# - a2: 0x1234_5678
# - a3: 0xCD
# - a4: 0xEE
# - a5: 0xFEDC
# - a6: 0x9876_5432
.globl l5
.globl l6
.globl l7
.data
    l1: .byte 0xAB
    l2: .half 0xABCD
    l3: .word 0x1234_5678
    l4: .byte 0xCD # Used to test padding
.text
    la s0, l1
    lbu a0, 0(s0)
    la s0, l2
    lhu a1, 0(s0)
    la s0, l3
    lw a2, 0(s0)
    la s0, l4
    lbu a3, 0(s0)
    la s0, l5
    lbu a4, 0(s0)
    la s0, l6
    lhu a5, 0(s0)
    la s0, l7
    lw a6, 0(s0)
