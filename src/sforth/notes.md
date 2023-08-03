## Opcodes (http://ref.x86asm.net/geek64.html)

Assume real mode operand size defaults.

Operard order; dw dW Dw DW
               d: reg->r/m, w: op8, D: r/m->reg, W: op16/32
Group order:   ADD OR ADC SBB AND SUB XOR CMP
Jxx order:     o  c  z  cz  s  p  os osz
               JO JB JE JBE JS JP JL JLE

## Questions

- Particular choice of encoding, e.g. 8B C7 vs. 89 F8 for mov %edi, %edx

- Purpose of 0x40 prefix

- In prim. TYPE, `test %rax, %rax`??

- In prim. LIT, code looks weird:
    ```
    b8 6a 41 8f 07             # movl $0x078f416a, %eax   # pushq $0x41
                               #                          # popq (%rdi)
    aa                         # stosb
    ```
