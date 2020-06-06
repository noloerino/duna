# Checks that redefined labels get caught.
bad_label: nop
bad_label: li a0, 1
# We're going to import another file, which defines the label end
.globl end
end: nop
