# Tests branches and jumps to local labels
# The final value of a0 should be 0xABCD_0123
li a0, 0xABCD_0122
li t1, 1
li t2, -1
# j actual_start
# This line should be skipped
# bad: li a0, 0xFFFF_FFFF
# li a0, 0xFFFF_FFFE
actual_start: 
bge t1, zero, l1
addi a0, zero, -1
l1: beq zero, zero, l2
addi a0, zero, -2
l2:
blt t2, t1, end
addi a0, zero, -3
end:
    addi a0, a0, 1