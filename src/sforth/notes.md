## Opcodes (http://ref.x86asm.net/geek64.html)

Assume real mode operand size defaults.

Operard order; dw dW Dw DW
               d: reg->r/m, w: op8, D: r/m->reg, W: op16/32
Group order:   ADD OR ADC SBB AND SUB XOR CMP
Jxx order:     o  c  z  cz  s  p  os osz
               JO JB JE JBE JS JP JL JLE
