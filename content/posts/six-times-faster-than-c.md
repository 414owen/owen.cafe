---
title: "{n} times faster than C - part one"
date: 2023-06-19T21:51:17+02:00
tags:
- C
- performance
- x86
- assembly
---

Sometimes humans can spot optimization opportunities that a compiler ~~can't~~
doesn't. In this post, we start with a loop generated from C code by clang, and
tweak it in various ways, measuring the speedup.

<!--more-->

**Disclaimer**: *I'm not an optimization expert, by any means, in fact my
expertise is in high-level, purely-functional languages, where one
doesn't usually think about **how** a program is executed.*

Code listings for this post can be found on [GitHub](https://github.com/414owen/blog-code/tree/master/01-six-times-faster-than-c).

## The Function

We'll start with a function that loops through a string, and increments or
decrements a number, depending on the characters it sees.

```c
int run_switches(char *input) {
  int res = 0;
  while (true) {
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
  }
}
```

It increments on seeing an 's' (for successor) and decrements on seeing a
'p' (for predecessor).

It's a small enough function that gcc and/or clang should be able to optimize it
pretty well. Maybe optimally? I initially wrote this to see whether gcc produced
a jump table or a search.

This is what clang spat out (padding noops removed, and annotated manually):

{{< tabs groupId="asmc" >}}
{{% tab name="pseudocode" %}}
```asm
# llvm-objdump -d --symbolize-operands --no-addresses --x86-asm-syntax=intel --no-show-raw-insn loop-1-clang.c.o

run_switches:
      xor     eax, eax            # res = 0
loop:                             # while (true) {
      movsx   ecx, byte ptr [rdi] #   c = *input
      test    ecx, ecx            #   if (c == '\0')
      je      ret                 #     return
      add     rdi, 1              #   input++
      cmp     ecx, 'p'            #   if (c == 'p')
      je      p                   #     goto p
      cmp     ecx, 's'            #   if (c == 's')
      jne     loop                #     continue
      add     eax, 1              #   res++
      jmp     loop                #   continue
p:    add     eax, -1             #   res--
      jmp     loop                # }
ret:  ret
```
{{% /tab %}}

{{% tab name="arrows" %}}
```asm
# objdump -Mintel -d --no-addresses --no-show-raw-insn --visualize-jumps loop-1-clang.c.o

run_switches:
              xor    eax, eax
loop:
      ╭────➤ movsx  ecx, byte ptr [rdi]
      │      test   ecx, ecx
      │ ╭─── je     ret
      │ │    add    rdi, 1
      │ │    cmp    ecx, 'p'
      │ │ ╭─ je     p
      │ │ │  cmp    ecx, 's'
      ├─│─│─ jne    loop
      │ │ │  add    eax, 1
      ├─│─│─ jmp    loop
p:    │ │ ╰➤ add    eax, -1
      ╰─│─── jmp    loop
ret:    ╰──➤ ret
```
{{% /tab %}}
{{< /tabs >}}

**Runtime:** 3.23s 🐌

**Bitrate**: 295.26MiB/s

GCC spat out a little more code, that ran a little faster (not much).

This code is pretty straightforward, it has three conditional branch
instructions (je, je, jne), leading to four possible blocks, '\0', 's', 'p', and
a block for any other character.

## Rearranging branches

However, we know some things about this loop. We know that the only time we
break out of it is when we hit the null terminator ('\0'). The code clang
generates checks for the null terminator first, but this makes no sense. The
maximum number of null terminators we will ever hit in this function is 1, so
for every 'p' and 's' character, we're checking for null first. We should
optimize for 'p's, 's's and other characters over null terminators.

So, let's rearrange this loop a little.

{{< tabs groupId="asm1" >}}
{{% tab name="arrows" %}}
```asm
run_switches:
               xor    eax, eax
loop:  ╭─────➤ movsx  ecx, byte ptr [rdi]
       │       inc    rdi
       │       cmp    ecx, 'p'
       │ ╭──── je     p
       │ │     cmp    ecx, 's'
       │ │ ╭── je     s
       │ │ │   test   ecx, ecx
       ├─│─│── jne    loop
       │ │ │   ret
p:     │ ╰─│─➤ dec    eax
       ├───│── jmp    loop
s:     │   ╰─➤ inc    eax
       ╰────── jmp    loop
```
{{% /tab %}}
{{% tab name="raw" %}}
```asm
run_switches:
        xor     eax, eax
loop:   movsx   ecx, byte ptr [rdi]
        inc     rdi
        cmp     ecx, 'p'
        je      p
        cmp     ecx, 's'
        je      s
        test    ecx, ecx
        jne     loop
        ret
p:      dec     eax
        jmp     loop
s:      inc     eax
        jmp     loop
```
{{% /tab %}}
{{< /tabs >}}

Great, now we branch earlier on seeing a 'p' or an 's', than on the rare '\0'.

**Runtime:** 3.10s 🦥

**Speedup:**: 1.04x 📈

**Bitrate**: 307.64MiB/s

## Rearranging blocks

So both of our common cases ('p' and 's') jump back to the top of the loop,
so why don't we remove one of those branches by putting its target block (or
BasicBlock™, for people in compiler land), at the top of the loop?

{{< tabs groupId="asm2" >}}
{{% tab name="arrows" %}}
```asm
run_switches:
              xor    eax, eax
       ╭───── jmp    loop
s:     │ ╭──➤ inc    eax
loop:  ├─│──➤ movsx  ecx, byte ptr [rdi]
       │ │    inc    rdi
       │ │    cmp    ecx, 'p'
       │ │ ╭─ je     p
       │ │ │  cmp    ecx, 's'
       │ ╰─│─ je     s
       │   │  test   ecx, ecx
       ├───│─ jne    loop
       │   │  ret
p:     │   ╰➤ dec    eax
       ╰───── jmp    loop
```
{{% /tab %}}
{{% tab name="raw" %}}
```asm
run_switches:
        xor     eax, eax
        jmp     loop       # This is new
s:      inc     eax        # This is up here now
loop:   movsx   ecx, byte ptr [rdi]
        inc     rdi
        cmp     ecx, 'p'
        je      p
        cmp     ecx, 's'
        je      s
        test    ecx, ecx
        jne     loop
        ret
p:      dec     eax
        jmp     loop
```
{{% /tab %}}
{{< /tabs >}}

Great, now our 's' block falls through into the loop without a branch. Pretty
sweet.

You'll notice that we now have to jump into the loop from the function start,
to avoid running the 's' block. This is a pretty good tradeoff though, jumping
into the loop from the function start happens once, whereas we encounter many
's' characters.

But is it fast?

**Runtime:** 2.98s 🐢

**Overall speedup:**: 1.08x 📈

**Bitrate**: 320.02MiB/s

## Replacing jumps with arithmetic

Conditional jumps [are bad](https://en.wikipedia.org/wiki/Branch_predictor), but
how about your standard garden variety unconditional `jmp`? What if we tried to
eliminate `p:`'s jump back into the loop?

A decrement is the same as two decrements and an increment, right? So let's use
that to fall through into `s:`.

{{< tabs groupId="asm3" >}}
{{% tab name="arrows" %}}
```asm
run_switches:
               xor    eax, eax
       ╭────── jmp    loop
p:     │   ╭─➤ sub    eax, 2
s:     │ ╭─│─➤ inc    eax
loop:  ├─│─│─➤ movsx  ecx, byte ptr [rdi]
       │ │ │   inc    rdi
       │ │ │   cmp    ecx, 'p'
       │ │ ╰── je     p
       │ │     cmp    ecx, 's'
       │ ╰──── je     s
       │       test   ecx, ecx
       ╰────── jne    loop
               ret
```
{{% /tab %}}
{{% tab name="raw" %}}
```asm
run_switches:
        xor     eax, eax
        jmp     loop
p:      sub     eax, 2
s:      inc     eax
loop:   movsx   ecx, byte ptr [rdi]
        inc     rdi
        cmp     ecx, 'p'
        je      p
        cmp     ecx, 's'
        je      s
        test    ecx, ecx
        jne     loop
        ret
```
{{% /tab %}}
{{< /tabs >}}

Well, we got rid of another branch instruction, using basic arithmetic. Good for
us. Is it faster though?

**Runtime:** 2.87s 🦌

**Overall speedup:**: 1.12x 📈

**Bitrate**: 332.29MiB/s

Fun fact, we've been comparing our performance to clang 16's output this whole
time, but GCC 12 actually produced faster (but more) code. GCC's code runs in
2.87s as well, so we only just caught up with it, however our program consists
of 13 instructions, and GCC's is 19.

GCC's code seems to have unrolled the loop, and is reusing the case blocks to
some extent.

## Just don't branch

Okay, but these **conditional** branches are the real problem, right? How do you
make the branch predictor fast? I don't know, so let's just not use it.

{{< tabs groupId="asm4" >}}
{{% tab name="arrows" %}}
```asm
# rdi: char *input
# eax: ouput
# r8:  1
# edx: -1
# ecx: char c
# esi: n

run_switches:
            xor    eax, eax
            mov    r8d, 1
            mov    edx, -1
loop: 
       ╭──➤ movsx  ecx, byte ptr [rdi]
       │    test   ecx, ecx
       │ ╭─ je     ret
       │ │  inc    rdi
       │ │  mov    esi, 0
       │ │  cmp    ecx, 'p'
       │ │  cmove  esi, edx
       │ │  cmp    ecx, 's'
       │ │  cmove  esi, r8d
       │ │  add    eax, esi
       ╰─│─ jmp    loop
ret:     ╰➤ ret
```
{{% /tab %}}
{{% tab name="pseudocode" %}}
```asm
# rdi: char *input
# eax: ouput
# r8:  1
# edx: -1
# ecx: char c
# esi: n

run_switches:
        xor    eax, eax             # res = 0
        mov    r8d, 1               # need  1 in a register later
        mov    edx, -1              # need -1 in a register later
loop:                               # while (true) {
        movsx  ecx, byte ptr [rdi]  #   char c = *input
        test   ecx, ecx             #   if (c == '\0')
        je     ret                  #     return
        inc    rdi                  #   input++
        mov    esi, 0               #   n = 0
        cmp    ecx, 'p'             #   if (c == 'p')
        cmove  esi, edx             #     n = -1
        cmp    ecx, 's'             #   if (c == 's')
        cmove  esi, r8d             #     n = 1
        add    eax, esi             #   res += n
        jmp    loop                 # }
ret:    ret
```
{{% /tab %}}
{{< /tabs >}}

Wow that removed a lot of arrows from the control flow graph...

Instead of branching/jumping conditionally, we're using a different value
for the addition depending on the current character, using `cmove`, or...
✨✨**conditional move on equality**✨✨.

The rules are: by default use zero, if we're on an 's', use 1, and if we're on a
'p', use -1. Then **always** add.

Right, nice flex, but... Is it fast?

**Runtime:** 0.48s 🐆

**Overall speedup:**: 6.73x 📈

**Bitrate**: 1.94GiB/s

Yes it's pretty damn fast.

## Freeing up a register

x86_64 has another way of conditionally setting a (1 byte) register to 0 or 1.
It's called `sete`. Let's use that, and remove our use of r8d.

{{< tabs groupId="asm5" >}}
{{% tab name="arrows" %}}
```asm
run_switches:
             xor    eax, eax
             mov    edx, -1
loop:
        ╭──➤ movsx  ecx, byte ptr [rdi]
        │    test   ecx, ecx
        │ ╭─ je     ret
        │ │  inc    rdi
        │ │  mov    esi, 0
        │ │  cmp    ecx, 's'
        │ │  sete   sil
        │ │  cmp    ecx, 'p'
        │ │  cmove  esi, edx
        │ │  add    eax, esi
        ╰─│─ jmp    loop
ret:      ╰➤ ret
```
{{% /tab %}}
{{% tab name="pseudocode" %}}
```asm
run_switches:
        xor   eax, eax             # res = 0
        mov   edx, -1              # need -1 in a register later
loop:                              # while (true) {
        movsx ecx, byte ptr [rdi]  #   char c = *input
        test  ecx, ecx             #   if (c == '\0')
        je    ret                  #     return
        inc   rdi                  #   input++
        mov   esi, 0               #   n = 0
        cmp   ecx, 's'             #   c == 's'?
        sete  sil                  #     n = 0|1
        cmp   ecx, 'p'             #   if (c == 'p')
        cmove esi, edx             #     n = -1
        add   eax, esi             #   res += n
        jmp   loop                 # }
ret:    ret
```
{{% /tab %}}
{{< /tabs >}}

... But is it fast?

**Runtime:** 0.51s 🦁

**Overall speedup:**: 6.33x 📈

**Bitrate**: 1.83GiB/s

Well, that's slower than using `cmov`s. I guess there are no points for using
less registers, or for using 8-bit operations instead of 32-bit ones...

## Other attempts

I tried unrolling the loop of our best version. This slowed down the code.

I tried aligning the start of the loop to a 16-byte boundary (pro tip, you
can add `.align <bytes>` before a label, and GNU assembler will insert `nop`
instructions for you). This also slowed down the code.

## Benchmarking setup

```sh
$ uname -sr
Linux 6.1.33
$ lscpu
...
  Model name:            AMD Ryzen 5 5625U with Radeon Graphics
    CPU family:          25
    Thread(s) per core:  2
    Core(s) per socket:  6
    Socket(s):           1
$ clang --version
clang version 16.0.1
$ gcc --version
gcc (GCC) 12.2.0
```

The C versions were compiled with `-march=native`, so that the C compiler knew
to produce code that was fast on **my specific** CPU, not some generic x86_64.

The benchmark runs the function over a list of one million characters (random
'p's and 's's) one thousand times.

For each version, the benchmark was run several times, and the best result
chosen.

## Conclusion

You can (sometimes) get a 6x speedup by hand-coding your tight C loop in
assembly, and optimizing using techniques that compilers don't seem to have
automated away yet.

Of course, this post isn't the end. If this still isn't fast enough for you,
you can read [part two]({{< relref "the-same-speed-as-c" >}}).
