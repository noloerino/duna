# Performs simple word-aligned loads and stores.
# At the end, a0 should contain 0xABCD_0123.
# Relies on pseudo-ops working, and SP being initialized to a mapped page.
li t1, 0xABCD_ABCD
sw t1, 0(sp)
li t1, 0xFFFF_0101
sh t1, 0(sp)
li t1, 0xFFFF_FF23
sb t1, 0(sp)
lw a0, 0(sp)
