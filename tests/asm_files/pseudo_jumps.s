# Tests jump pseudo-instructions (j, ret). Does not test the pseudo versions of jal and jalr.
# Also does not use labels, which means we have to be very careful with relative offsets.
# a0 should contain 0xDEAD after completion.
li t1, 0xDEAC
j 12 # j end
# fn:
mv a0, t1
ret
# end:
jal ra, -8 # jal fn
addi a0, a0, 1