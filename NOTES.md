# Notes
Just a space for jotting down random thoughts and resources.

RISCV green sheet: https://inst.eecs.berkeley.edu/~cs61c/fa18/img/riscvcard.pdf
MIPS green sheet: https://inst.eecs.berkeley.edu/~cs61c/resources/MIPS_Green_Sheet.pdf

RISCV explicitly defines signed addition to wrap, whereas MIPS will produce exceptions on overflow
(https://www.cs.unibo.it/~solmi/teaching/arch_2002-2003/AssemblyLanguageProgDoc.pdf#page=29)

MIPS allows semicolons as a separator to put multiple instructions on one line, and also allows
/\* \*/ comments

### Register names
RISCV: nothing special (x0, a0)
MIPS: dollar signs ($1, $v0)
x86 GAS: dollar signs denote constants (https://en.wikibooks.org/wiki/X86_Assembly/GAS_Syntax),
% sign prefixes registers (%eax, %ebp)
x86 AT&T: types are inferred (literal 4 is just 4, register ebp is just ebp) (https://en.wikipedia.org/wiki/X86_assembly_language#Syntax)