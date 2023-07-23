## Opcodes (http://ref.x86asm.net/geek64.html)

Assume real mode operand size defaults.

01   ModR/M       ADD    *Evqp*   Gvqp
24   Ib           AND    *AL*     Ib
29   ModR/M       SUB    *Evqp*   Gvqp
31   ModR/M       XOR    *Evqp*   Gvqp
3A   ModR/M       CMP    Gb       Eb
3C   Ib           CMP    AL       Ib
50/r              PUSH   Zvq
58/r              POP    *Zvq*
6A   Ibss         PUSH   Ibss
74   Jbs          JE/JZ  Jbs
7C   Jbs          JL     Jbs
7F   Jbs          JG     Jbs
83/0 ModR/M Ibs   ADD    *Evqp*   Ibs
83/4 ModR/M Ibs   AND    *Evqp*   Ibs
83/5 ModR/M Ibs   SUB    *Evqp*   Ibs
85   ModR/M       TEST   Evqp     Gvqp
89   ModR/M       MOV    *Evqp*   Gvqp
8B   ModR/M       MOV    *Gvqp*   Evqp
90/r              XCHG   *Zvqp*   *rAX*
A4                MOVSB  *_Yb_*   _Xb_
A8   Ib           TEST   AL       Ib
AA                STOSB  *_Yb_*   _AL_
AB                STOSD  *_Yvqp_* _rAX_
AC                LODSB  *_rAX_*  _Xvqp_
B0/r Ib           MOV    *Zb*     Ib
B8/r Ivqp         MOV    *Zvqp*   Ivqp
C3                RETN
C7/0 ModR/M Ivds  MOV    *Evqp*   Ivds
E8   Jvds         CALL   Jvds
EB   Jbs          JMP    Jbs
FF/2 ModR/M       CALL   Eq
FF/4 ModR/M       JMP    Eq

0F 05             SYSCALL
