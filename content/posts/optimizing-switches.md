---
title: "Optimizing Switches"
date: 2023-06-19T21:51:17+02:00
---

This blog post started as a post about lexers. This involved quite a few
switch statements on characters, and eventually I disassembled a tiny
program to see what was going on.

The program looked like this:

```c
int run_switches(char *input) {
  int res = 0;
  do {
    char c = *input++;
    switch (c) {
      case '\0':
        return res;
      case 's':
        res += 1;
        break;
      case 'p':
        res -= 1;
        break;
      default:
        break;
    }
  } while (true);
}
```

It increments on seeing an 's' (for successor) and decrements on seeing a 'p' (for predecessor).

Small enough that gcc and/or clang should be able to optimize it pretty well. Maybe
optimally?

This is what clang spat out (padding noops removed, and annotated manually):


```asm
# llvm-objdump -d  --symbolize-operands --no-addresses --x86-asm-syntax=intel --no-show-raw-insn

run_switches:
    xor     eax, eax            # res = 0
L2:                             # while (true) {
    movsx   ecx, byte ptr [rdi] #   c = *input
    test    ecx, ecx            #   if (c == '\0')
    je       L0                 #     return
    add     rdi, 0x1            #   input++
    cmp     ecx, 0x70           #   if (c == 'p')
    je       L1                 #     goto L1
    cmp     ecx, 0x73           #   if (c == 's')
    jne      L2                 #     continue
    add     eax, 0x1            #   res++
    jmp      L2                 #   continue
L1:
    add     eax, -0x1           #   res--
    jmp      L2                 # }
L0:
    ret
```

GCC spat out a little more code, that ran a little faster (not much).

This code is pretty straightforward, it has three conditional branch
instructions, leading to four possible blocks, '\0', 's', 'p', and
other. The maximum number of branches taken to reach the top of the
loop is four.

As you may of may not know, branching can be inefficient, as the CPU
is pipelined, and conditional branching creates two possibile sources
for the next instructions.

x86_64 has some instructions for dealing with branches that just change
a value. They're called conditional moves.

I played around with writing this funciton out by hand using cmov
instructions, and came up with this:

```asm
run_switches:
        xor rax, rax            # res = 0
        mov rsi, 1              # need  1 in a register later
        mov rdx, -1             # need -1 in a register later
loop:                           # while (true) {
        mov r8b, BYTE PTR [rdi] #   char c = *input
        test r8b,r8b            #   if (c == '\0')
        je ret                  #     return
        inc rdi                 #   input++
        mov ecx, 0              #   n = 0
        cmp r8b,0x70            #   if (c == 'p')
        cmove rcx, rdx          #     n = -1
        cmp r8b, 0x73           #   if (c == 's')
        cmove rcx, rsi          #     n = 1
        add rax, rcx            #   res += n
        jmp loop                # }
ret:
        ret
```
