# Tests branches with a fixed offset.
# Beware of 'li' between branches: since it may expand to two instructions, the resulting
# branch destination may get thrown off.
# The final value of a0 should be 0xABCD_0123
li a0, 0xABCD_0122
li t1, 1
li t2, -1
bge t1, zero, 8 # to l1
addi a0, zero, -1
# l1:
beq zero, zero, 8 # to l2
addi a0, zero, -2
# l2:
blt t2, t1, 8 # to end
addi a0, zero, -3
# end:
addi a0, a0, 1