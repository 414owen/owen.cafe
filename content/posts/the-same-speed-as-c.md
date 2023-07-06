---
title: "{n} times faster than C - part two"
date: 2023-07-01T16:25:59+02:00
tags:
- C
- performance
- x86
- assembly
---

In the [last post](/posts/six-times-faster-than-c/), we wrote a tiny C program, compiled it, disassembled it,
then tweaked that assembly to make it six times faster. Now we're going to beat it.

<!--more-->

**Disclaimer**: *I'm not an optimization expert, by any means, in fact my
expertise is in high-level, purely-functional languages, where one
doesn't usually think about **how** a program is executed.*

Our first version was able to process 295.26MiB/s, and our best version
reached 1.94GiB/s.

The code listings for this post can be found on [Github](https://github.com/414owen/blog-code/tree/master/02-the-same-speed-as-c).

So, let's start with the first C version:


{{< tabs groupId="initial" >}}

{{% tab name="C" %}}
```C
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
{{% /tab %}}

{{% tab name="asm + pseudocode" %}}
```asm
# llvm-objdump -d --symbolize-operands --no-addresses --x86-asm-syntax=intel --no-show-raw-insn loop-1-gcc.c.o

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

{{% tab name="asm + arrows" %}}
```asm
# objdump -Mintel -d --no-addresses --no-show-raw-insn --visualize-jumps loop-2-gcc.c.o

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

## Dropping cases

The way we've expressed this problem is the pretty logical top-down way.
We're looping over out input, one character at a time, and performing a
[case analysis](https://en.wikipedia.org/wiki/Proof_by_exhaustion) over
the possible variants (or switching over the characters, if you prefer).

Based on this case analysis, we run different code.

To avoid this, let's merge the codepaths with common structure.
Both the 'p' and 's' cases add a number to the accumulator, so what if,
instead of performing case analysis to get the next code path, we do so
to get the number to add.

We can look up the number to add in an array:


{{< tabs groupId="arr-lookup" >}}
{{% tab name="C" %}}
```C
#include <stdbool.h>

static
int to_add[256] = {
  ['s'] = 1,
  ['p'] = -1,
};

int run_switches(const char *input) {
  int res = 0;
  while (true) {
    char c = *input++;
    if (c == '\0') {
      return res;
    } else {
      res += to_add[(int) c];
    }
  }
}
```
{{% /tab %}}

{{% tab name="gcc asm" %}}
```asm
# objdump -Mintel -d --no-addresses --no-show-raw-insn --visualize-jumps loop-2-gcc.c.o

run_switches:
           movsx  rax, byte ptr [rdi]
           lea    rdx, [rdi+1]
           xor    ecx, ecx
           lea    rsi, [rip+0]        # <run_switches+0x11>
           test   al,  al
      ╭─── je     ret
      │    nop    dword ptr [rax]
loop: │ ╭➤ add    rdx, 0x1
      │ │  add    ecx, dword ptr [rsi+rax*4]
      │ │  movsx  rax, byte ptr [rdx-1]
      │ │  test   al,  al
      │ ╰─ jne    loop
ret:  ╰──➤ mov    eax, ecx
           ret
```
{{% /tab %}}

{{% tab name="clang asm" %}}
```asm
# objdump -Mintel -d --no-addresses --no-show-raw-insn --visualize-jumps loop-2-clang.c.o

run_switches:
           mov    cl,  byte ptr [rdi]
           test   cl,  cl
      ╭─── je     ret
      │    add    rdi, 0x1
      │    xor    eax, eax
      │    lea    rdx, [rip+0x0]        # <run_switches+0x13>
      │    cs nop word ptr [rax+rax*1+0x0]
      │    nop    dword ptr [rax]
loop: │ ╭➤ movsx  rcx, cl
      │ │  add    eax, dword ptr [rdx+rcx*4]
      │ │  movzx  ecx, byte ptr [rdi]
      │ │  add    rdi, 0x1
      │ │  test   cl,  cl
      │ ╰─ jne    loop
      │    ret
ret:  ╰──➤ xor    eax, eax
           ret
```
{{% /tab %}}
{{< /tabs >}}


**GCC Runtime:** 0.47s 🦓

**GCC Bitrate**: 1.98GiB/s

**Clang Runtime:** 0.25s 🐆

**Clang Bitrate**: 3.72GiB/s

Sweet, well this is just as fast as our best (cmov) version from the previous
post, and clang's output is almost twice as fast as our previous best version.

Something really weird is going on with the difference between GCC and
clang. How big can the performance difference between these lines really be?

```
movzx  ecx, byte ptr [rdi]
movsx  rax, byte ptr [rdx-1]
```

I think I can get gcc to generate a version with movzx (move and zero-extend)
instead of movsx (move and sign-extend), by using an unsigned integer type
as the instruction, so `uint8_t` or `unsigned char`, instead of `char`.
Let's give it a go:


{{< tabs groupId="arr-lookup-2" >}}
{{% tab name="C" %}}
```C
#include <stdbool.h>
#include <stdint.h>

static
int to_add[256] = {
  ['s'] = 1,
  ['p'] = -1,
};

int run_switches(const uint8_t *input) {
  int res = 0;
  while (true) {
    uint8_t c = *input++;
    if (c == '\0') {
      return res;
    } else {
      res += to_add[(int) c];
    }
  }
}
```
{{% /tab %}}

{{% tab name="gcc asm" %}}
```asm
# objdump -Mintel -d --no-addresses --no-show-raw-insn --visualize-jumps loop-3-gcc.c.o

run_switches:
               movzx  eax, byte ptr [rdi]
               lea    rdx, [rdi+0x1]
               xor    ecx, ecx
               lea    rsi, [rip+0x0]
               test   al,  al
        ╭───── je     ret
        │      nop    dword ptr [rax+0x0]
loop:   │  ╭─➤ movzx  eax, al
        │  │   add    rdx, 1
        │  │   add    ecx, dword ptr [rsi+rax*4]
        │  │   movzx  eax, byte ptr [rdx-1]
        │  │   test   al,  al
        │  ╰── jne    loop
ret:    ╰────➤ mov    eax,ecx
               ret
```
{{% /tab %}}

{{% tab name="clang asm" %}}
```asm
# objdump -Mintel -d --no-addresses --no-show-raw-insn --visualize-jumps loop-3-clang.c.o

run_switches:
               mov    cl,  byte ptr [rdi]
               test   cl,  cl
        ╭───── je     ret
        │      add    rdi, 1
        │      xor    eax, eax
        │      lea    rdx, [rip+0x0]
        │      cs nop word ptr [rax+rax*1+0x0]
        │      nop    dword ptr [rax]
loop:   │  ╭─➤ movzx  ecx, cl
        │  │   add    eax, dword ptr [rdx+rcx*4]
        │  │   movzx  ecx, byte ptr [rdi]
        │  │   add    rdi, 1
        │  │   test   cl,  cl
        │  ╰── jne    loop
        │      ret
ret:    ╰────➤ xor    eax,eax
               ret
```
{{% /tab %}}
{{< /tabs >}}

Well that did the trick, so is it faster?

**GCC Runtime:** 0.47s 🦓

**GCC Bitrate**: 1.98GiB/s

**Clang Runtime:** 0.25s 🐆

**Clang Bitrate**: 3.72GiB/s

Nope. It's exactly the same. Okay, so what's the difference between these (tabs):

{{< tabs groupId="arr-lookup-2-diff" >}}
{{% tab name="gcc asm" %}}
```asm
loop: ╭─➤ movzx  eax, al
      │   add    rdx, 1
      │   add    ecx, dword ptr [rsi+rax*4]
      │   movzx  eax, byte ptr [rdx-1]
      │   test   al,  al
      ╰── jne    loop
```
{{% /tab %}}

{{% tab name="clang asm" %}}
```asm
loop: ╭─➤ movzx  ecx, cl
      │   add    eax, dword ptr [rdx+rcx*4]
      │   movzx  ecx, byte ptr [rdi]
      │   add    rdi, 1
      │   test   cl,  cl
      ╰── jne    loop
```
{{% /tab %}}
{{< /tabs >}}

Three things:
* Order
* `[rdi]` vs `[rdx - 1]`
* Register choices

## Skipping an extend

Both compilers produced a movzx (move and zero-extend) instruction,
to turn the character of the input into an int.

I don't think this instruction is necessary on either compiler, as
32-bit operations implicitly zero-extend on an x86_64, and I can't
see any code paths where the upper bits would have been set.

That said, some vague intuition tells me we can get rid of this instruction
on the C side by changing

```c
uint8_t c = *input++;
```

to

```c
uint64_t c = *input++;
```

Thus removing all identification of this as an 8-bit value that needs zero-
extending.

{{< tabs groupId="avoid-ze" >}}
{{% tab name="gcc asm" %}}
```asm
loop: /-> add    rdx, 1
      |   add    ecx, dword ptr [rsi+rax*4]
      |   movzx  eax, byte ptr [rdx-1]
      |   test   rax, rax
      \-- jne    loop

```
{{% /tab %}}

{{% tab name="clang asm" %}}
```asm
loop: /-> movzx  ecx,cl
      |   add    eax,dword ptr [rdx+rcx*4]
      |   movzx  ecx,byte ptr [rdi]
      |   add    rdi,0x1
      |   test   cl,cl
      \-- jne    <run_switches+0x20>
```
{{% /tab %}}
{{< /tabs >}}

Well it... worked for GCC.

One less instruction, without dropping down to assembly. Pretty good, huh?

**GCC Runtime:** 0.47s 🦓

**GCC Bitrate**: 1.98GiB/s

Er... that did nothing.

## The asm rewrite

Let's rewrite this in assembly to find out what's going on.

No you can't really reliably reassemble disassembled code... You'll need to
at least tweak the output. In this case, I wrote the array that it's reading
from in terms of GCC assembler directives, and added jump labels, to make it
readable.

```asm
.text
run_switches:
          xor   eax, eax                   # res = 0
loop:                                      # while (true) {
  /---->  movsx rcx, byte ptr [rdi]        #   char c = *input
  |       test  rcx, rcx                   #   if (c == '\0')
  |  /--  je    ret                        #     return
  |  |    add   eax, dword ptr [arr+rcx*4] #   res += arr[c]
  |  |    inc   rdi                        #   input++
  \--|--  jmp   loop                       # }
ret: \->  ret

.data
arr:
        .fill 'p', 4, 0
        .long -1, 0, 0, 1
        .fill (256 - 's'), 4, 0
```

This is my first pass, and it's a very literal translation of the C version.
There's even two jumps (an extra unconditional one) in the hotpath compared to
GCC and clang's output. So, how does it fare?

**Runtime:** 0.24s 🐆

**Bitrate**: 3.88GiB/s

Well, it... beat both GCC and clang, on the first attempt. I would have expected
it to be harder to write faster code than modern compilers, with their plethora
of optimization passes, yet here we are...

## Tripping over encoding

GCC's output loads the array's address into a register, and uses that to
reference the array in the hotpath:

```asm
lea    rsi, [add_arr]
...
add    ecx, dword ptr [rsi+rax*4]
```

Well, this seems redundant, so we should be able to get rid of the extra `lea`
instruction by using the array's address directly.

```asm
add    ecx, dword ptr [arr+rax*4]
```

We've removed one instruction (`lea`) from the non-hotpath, right? That's true,
but if we look at the instruction encodings side-by-side, we see that we've
increased the number of bytes needed to encode the `add`, and the `add` is the
instruction in the hotpath.

{{< tabs groupId="encodings" >}}
{{% tab name="before" %}}
```asm
48 8d 34 25 00 00 00    lea    rsi,ds:0x0
...
03 0c 86                add    ecx,DWORD PTR [rsi+rax*4]
```
{{% /tab %}}

{{% tab name="after" %}}
```asm

...
03 0c 85 00 00 00 00    add    ecx,DWORD PTR [rax*4+0]
```
{{% /tab %}}
{{< /tabs >}}

I guess loading the array's base address into rsi is a nice optimization to
have, if you have registers to spare. Let's ignore this for now.

## Fixing GCC's code

What we really want to know is why GCC's inner loop is almost twice as slow as clang's
even though they look so similar. To this end, I've copy-pasted GCC's code into an
assembly program [here](https://github.com/414owen/blog-code/blob/master/02-the-same-speed-as-c/loop-6.x64.s),
and made the changes necessary to get it working.

If we run the benchmark with our dissassembled version, it should take the same amount
of time as GCC's (0.47s).

Aaaaand it took 0.26s, almost twice as fast. I guess I've made a mistake?
Let's find the difference in output.

{{< tabs groupId="gcc-reassembly" >}}
{{% tab name="gcc's output" %}}
```asm
   9:   48 8d 35 00 00 00 00    lea    rsi,[rip+0]
  10:   48 85 c0                test   rax,rax
  13:   74 13                   je     28 ret
```
{{% /tab %}}
{{% tab name="reassembled version" %}}

```asm
   9:   48 8d 34 25 00 00 00    lea    rsi,ds:0
  10:   00
  11:   48 85 c0                test   rax,rax
  14:   74 13                   je     29 ret
```
{{% /tab %}}
{{< /tabs >}}

There! a slightly different encoding of `lea`.

I'm not entirely sure what causes this, as both versions seem to have
the array stored in the `.rodata` section of the binary. If someone knows the
difference between these two encodings, please email me.

So, as the only tangible difference is before the hotpath, I'm guessing we're
seeing a difference in alignment affect our performance.

Sure enough, if we add five innocent noops before out tight loop, we're back to
gcc's performance.

Let's do one better, and graph added padding vs runtime.

![padding-graph-50](/img/graph-asm-padding-50.svg)

So starting at five noops (one byte each), we get a ~2x slowdown, and subsequent
noops maintain that slowdown, until 20 noops, where we speed back up. Is this
a repeating pattern?

![padding-graph-1000](/img/graph-asm-padding-1000.svg)

...Yes it's definitely a repeating pattern. What's the period? I hope it's a nice power of two...

...It's 64. Very satisfying.

It's slow for 15, then fast for 49... er... *bytes of padding*.

My inner loop measures 16 bytes exactly. So that repeating pattern of 15 slow
versions, those must be when GCC's inner loop is spread across the boundary of
two cache lines. What's the size of a cache line on my CPU? 64 bytes :)

Good news, we figured out the speed difference! 🎉 🎉 🎉

So, how do we convince GCC to align our loop better?

I tried

```
CFLAGS=-falign-jumps=<n> make bench-c-4-gcc
```

For various values of `n`, but nothing happened.
{{< figure src="/img/align-jumps-effectiveness.png" alt="Pokemon alignment not very effective meme" class="pix gb" >}}

The reason seems to be that, as well as being a branch target, the top of our
loop is fallen into from the code above.

From the GCC docs:

> Align branch targets to a power-of-two boundary, for branch targets where the targets can only be reached by jumping

Luckily, `CFLAGS=-falign-loops=16` works!

The docs say that `-falign-loops` is enabled by default with optimizations set
to `>= -O2`, but it also says `if n is not specified or is zero, use a
machine-dependent default`. I guess this default is less than 16 on my machine :/

It also seems overkill to bump this option for a whole compilation unit. Ideally we'd only
set the alignment manually for our inner loop. GCC provides alignment attributes, eg.
`__attribute__((aligned(16)))`, but they don't apply to labels / statements, only functions.

Our entire function does actually fit within one cache line, and indeed setting
`__attribute__((aligned(64)))` on `run_switches` works. I guess that's the most
granular control you can get without dropping down to assembly.
Maybe someone knows a better way though?

## Benchmarking setup

See [part one](/posts/six-times-faster-than-c).

## Conclusion

* Tight, aligned loops that fit in a cache line go absolutely lightning fast.
* Tight loops spread across two cache lines may be ~2x slower than their aligned counterparts.
* GCC doesn't seem to align code very aggressively, by default.
* When writing GNU assembly, `.align <bytes>` is your friend.
* You can write two whole blog posts about a tiny loop.
