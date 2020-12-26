# Tests jumping to an address loaded by the la pseudo-op.
la s0, good
j good
li a0, -1
li a0, -2
good:
    li a0, 100
    j end
bad:
    li a0, -100
end:
    addi a0, a0, 11
