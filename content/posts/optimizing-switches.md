---
title: "Optimizing Switches"
date: 2023-06-19T21:51:17+02:00
---

I was going to create a blog post about creating the fastest lexer, using
a direct encoding of the state machine using the program counter. It was
going to show that whether you write it by hand, or generate it with a
tool, you could get optimal performance.

That post contains a lot of switching on characters, so I compiled a tiny
test program to see what gcc spat out.

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
    }
  } while (true);
}
```

Small enough that gcc should be able to optimize it pretty well. Maybe
optimally?


```asm
# llvm-objdump -d  --symbolize-operands --no-addresses --x86-asm-syntax=intel --no-show-raw-insn

run_switches:
                xor     eax, eax
L2:
                movsx   ecx, byte ptr [rdi]
                test    ecx, ecx
                je       L0
                add     rdi, 0x1
                cmp     ecx, 0x70
                je       L1
                cmp     ecx, 0x73
                jne      L2
                add     eax, 0x1
                jmp      L2
L1:
                add     eax, -0x1
                jmp      L2
L0:
                ret
```

This is what clang produces
