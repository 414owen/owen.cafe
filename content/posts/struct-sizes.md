---
title: "Quantifying pass-by-value overhead"
date: 2025-10-17T18:43:18+01:00
tags:
- C
- performance
- x86
- assembly
---

Should you pass data by value, or by reference?

<!--more-->

<link href="https://cdn.jsdelivr.net/npm/smolgraph@2.1.7/graph.min.css" rel="stylesheet">

<!-- NOTE Assembly dump command: objdump --visualize-jumps -M intel intel-mnemonic --no-show-raw-insn --no-addresses -d benchmark -->

When you're a certain kind of perfectionist, the kind that prevents you from
being productive, this is a question that might plague you every time you write
a function.

I've heard a few opinions on this, floating around. I've heard *"stack-to-stack
copies are super cheap"*, but.. how cheap are they?

---

Fine, I'll write a benchmark. Can't take too long.

3... 2... (several months pass[^1]) 1... 🪄

And that's how I ended up writing a graphing library, and a benchmark.

---

If you want to run these benchmarks on your own machine, or dump the
assembly, you can check out the [benchmark code](https://github.com/414owen/call-by-value-benchmarks).

---

<noscript>
This part requires JS. Sorry.
I promise all it's doing it creating a graph.
</noscript>


## All the data

This graph shows the overhead of passing structs of different sizes by value,
on different machines.

In general, passing any struct by reference incurs the same overhead as passing
a pointer-sized struct by value.

<div class="graph">
<img alt="spinner" src="/img/hourglass-d.webp">
</div>

Okay, on a macro level, passing a function parameter by value takes time
proportional to the size of the data.

Makes sense.


## Minuscule

{{< tabs groupId="minuscule" >}}
{{% tab name="graph" %}}
<div class="graph" data-initial-min="0" data-initial-max="128" data-series="amd">
<img alt="spinner" src="/img/hourglass-d.webp">
</div>
{{% /tab %}}
{{% tab name="asm - 1 byte" %}}
```asm
╭─➤ xor    edi,edi
│   call   feed30 <pass_1_fields_by_value>
│   sub    ebx,0x1
╰── jne    4786f0 <bench_1+0x20>
```
{{% /tab %}}
{{% tab name="asm - 32 bytes" %}}
```asm
╭─➤ sub    rsp,0x20
│   movdqa xmm0,XMMWORD PTR [rsp+0x20]
│   movups XMMWORD PTR [rsp],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x30]
│   movups XMMWORD PTR [rsp+0x10],xmm0
│   call   fef080 <pass_32_fields_by_value>
│   add    rsp,0x20
│   sub    ebx,0x1
╰── jne    479510 <bench_32+0x40>
```
{{% /tab %}}
{{< /tabs >}}

For smaller struct sizes, it doesn't look like there's a huge difference
between passing 8 bytes, or passing 32.

In the tabs above, you can inspect the assembly for the benchmark loops
of 1-byte and 32-byte structs.

The assembly says there's a difference, but seemingly not one you'd notice.
Technically the 32-byte value is being passed on the stack, using vector
registers, whereas the 1-byte value is being passed in a register,
but the speed difference is negligible.

## Small

{{< tabs groupId="small" >}}
{{% tab name="graph" %}}
<div class="graph" data-initial-min="0" data-initial-max="500" data-series="amd">
<img alt="spinner" src="/img/hourglass-d.webp">
</div>
{{% /tab %}}
{{% tab name="asm - 256 bytes" %}}
```asm
╭─➤ sub    rsp,0x100
│   movdqa xmm0,XMMWORD PTR [rsp+0x100]
│   movups XMMWORD PTR [rsp],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x110]
│   movups XMMWORD PTR [rsp+0x10],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x120]
│   movups XMMWORD PTR [rsp+0x20],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x130]
│   movups XMMWORD PTR [rsp+0x30],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x140]
│   movups XMMWORD PTR [rsp+0x40],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x150]
│   movups XMMWORD PTR [rsp+0x50],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x160]
│   movups XMMWORD PTR [rsp+0x60],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x170]
│   movups XMMWORD PTR [rsp+0x70],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x180]
│   movups XMMWORD PTR [rsp+0x80],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x190]
│   movups XMMWORD PTR [rsp+0x90],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x1a0]
│   movups XMMWORD PTR [rsp+0xa0],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x1b0]
│   movups XMMWORD PTR [rsp+0xb0],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x1c0]
│   movups XMMWORD PTR [rsp+0xc0],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x1d0]
│   movups XMMWORD PTR [rsp+0xd0],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x1e0]
│   movups XMMWORD PTR [rsp+0xe0],xmm0
│   movdqa xmm0,XMMWORD PTR [rsp+0x1f0]
│   movups XMMWORD PTR [rsp+0xf0],xmm0
│   call   fefe80 <pass_256_fields_by_value>
│   add    rsp,0x100
│   sub    ebx,0x1
╰── jne    4927c0 <bench_256+0x100>
```
{{% /tab %}}
{{% tab name="asm - 257 bytes" %}}
```asm
╭─➤ sub    rsp,0x110
│   mov    ecx,0x20
│   mov    rsi,rbp
│   mov    rdi,rsp
│   rep movs QWORD PTR es:[rdi],QWORD PTR ds:[rsi]
│   movzx  eax,BYTE PTR [rsi]
│   mov    BYTE PTR [rdi],al
│   call   fefe90 <pass_257_fields_by_value>
│   add    rsp,0x110
│   sub    ebx,0x1
╰── jne    492a00 <bench_257+0x110>
```
{{% /tab %}}
{{< /tabs >}}

There's a beautiful clean cliff here at 257 bytes. This seems to represent the
difference between an unrolled, vectorized memcpy routine, and one using
`rep movs` (ie. a microcoded for loop).

In the tabs above, you can inspect the assembly for 256 and 257 bytes.


## Periodic

{{< tabs groupId="periodic" >}}
{{% tab name="graph" %}}
<div class="graph" data-initial-min="1600" data-initial-max="1800" data-series="amd">
<img alt="spinner" src="/img/hourglass-d.webp">
</div>
{{% /tab %}}
{{% tab name="asm - 1682 bytes" %}}
```asm
╭─➤ sub    rsp,0x690
│   mov    ecx,0xd1
│   mov    rsi,rbp
│   mov    rdi,rsp
│   rep movs QWORD PTR es:[rdi],QWORD PTR ds:[rsi]
│   call   <pass_1672_fields_by_value>
│   add    rsp,0x690
│   sub    ebx,0x1
╰── jne    <bench_1672+0x120>
```
{{% /tab %}}
{{% tab name="asm - 1696 bytes" %}}
```asm
╭─➤ sub    rsp,0x6a0
│   mov    ecx,0xd4
│   mov    rsi,rbp
│   mov    rdi,rsp
│   rep movs QWORD PTR es:[rdi],QWORD PTR ds:[rsi]
│   call   <pass_1696_fields_by_value>
│   add    rsp,0x6a0
│   sub    ebx,0x1
╰── jne    <bench_1696+0x100>
```
{{% /tab %}}
{{< /tabs >}}

Well this is interesting.

The period is 32, with 8 fast, and 24 slow struct widths per period.

Now, you may think that this would be something to do with having to copy
other leftover memory after `rep movs`, but that doesn't seem to be the
case. I've dumped the assembly of two functions above, one from the valley,
and one from the hill, which have near-identical assembly.

Instead, it looks like the performance of `rep movs` itself is periodic.

## Spikes

{{< tabs groupId="spikes" >}}
{{% tab name="graph" %}}
<div class="graph" data-initial-min="0" data-initial-max="12000" data-series="amd">
<img alt="spinner" src="/img/hourglass-d.webp">
</div>
{{% /tab %}}
{{% tab name="asm - 4064 bytes" %}}
```asm
╭─➤ sub    rsp,0xfe0
│   mov    ecx,0x1fc
│   mov    rsi,rbp
│   mov    rdi,rsp
│   rep movs QWORD PTR es:[rdi],QWORD PTR ds:[rsi]
│   call   <pass_4064_fields_by_value>
│   add    rsp,0xfe0
│   sub    ebx,0x1
╰── jne    <bench_4064+0x100>
```
{{% /tab %}}
{{% tab name="asm - 4072 bytes" %}}
```asm
╭─➤ sub    rsp,0xff0
│   mov    ecx,0x1fd
│   mov    rsi,rbp
│   mov    rdi,rsp
│   rep movs QWORD PTR es:[rdi],QWORD PTR ds:[rsi]
│   call   <pass_4072_fields_by_value>
│   add    rsp,0xff0
│   sub    ebx,0x1
╰── jne    <bench_4072+0x120>
```
{{% /tab %}}
{{< /tabs >}}

Alright, what's going on here?

Passing a struct of 4064 bytes takes 53ns, but passing one of 4065 bytes takes 222ns,
in other words 4x longer.

And yes, these results are reproducible. I can run the benchmark as
many times as I want, and always get the same spikes.

Again, I've dumped what is essentially matching assembly from the non-hill and
from the hill, in the tabs above.

Since the spike doesn't persist for greater struct widths, this probably isn't
a case of hitting a [CPU cache](https://en.wikipedia.org/wiki/CPU_cache)
limit.

It would appear that the `rep movs` instruction has a serious performance bug
for these very specific ranges, as implemented in AMD Zen* CPU microcode.
If any AMD engineers know what's going on, please shoot me an email :)

If you work on GCC, Clang, or another compiler, and want to add an
absolutely disgusting hack that removes 1-2 `rep movs` iterations, and
adds a few extra manual `mov`s, on my CPU (and probably other AMD CPUs),
I'd also love to hear about it.

## Conclusions

* Passing structs up to size 256 is very cheap, and uses SIMD registers.
* Passing structs larger than 256 uses `rep movs`.
* Unrolled vectorized moves seem to be faster than `rep movs`.
* The performance of `rep movs` is probably periodic.
* You can pass **730 million** structs of size **16** by value per second.
* You can pass **26 million** structs of size **2048** by value per second.
* **Don't** pass around data of size 4046-4080 bytes or 8161-8176 bytes,
  by value (at least not on an AMD Ryzen 3900X).

<script type="module">

import { drawGraph } from "https://cdn.jsdelivr.net/npm/smolgraph@2.1.7/+esm";

const cachePromise = promise => {
  let res = undefined;
  let err = undefined;

  const handlers = [];

  promise.then(a => {res = a;}).catch(a => {err = a;}).finally(() => {
    for (const handler of handlers) {
      handler();
    }
  });

  const immediate = () => err ? Promise.reject(err) : Promise.resolve(res);
  return () => {
    if (res || err) {
      return immediate();
    }
    return new Promise((res, rej) => {
      handlers.push(() => {
        immediate().then(res, rej);
      })
    });
  };
};

const loadDataFile = (label, url) => cachePromise(
  fetch(url)
    .then(res => res.json())
    .then(data => ({ data: data.map((n, i) => [i + 1, n]), label }))
);

const seriesData = {
  amd: ["#70a7c2", loadDataFile("AMD Ryzen 3900x", "/struct-size-data/3900x.json")],
  arm: ["#a7c270", loadDataFile("Apple M2", "/struct-size-data/arm-m2.json")],
  // dave_amd: loadDataFile("amd ryzen 7 7735U", "/struct-size-data/dave-laptop-amd.json"),
  dave_intel: ["#c270a7", loadDataFile("Intel Core i5-12400F", "/struct-size-data/dave-desktop-intel.json")],
  // intel: loadDataFile("intel core 7 ultra", "/struct-size-data/intel-core-ultra-7.json")
};

const mkEWMA = period => {
  let average = null;
  const alpha = 2 / (period + 1);
  return value => {
    if (typeof value !== 'number' || isNaN(value)) {
      return average;
    }
    if (average === null) {
      // First value. The average is just this value.
      average = value;
    } else {
      // EWMA formula
      average = (alpha * value) + (1 - alpha) * average;
    }
    return average;
  };
};

[...document.getElementsByClassName("graph")].forEach(element => {
  let { initialMin, initialMax, series } = element.dataset;
  if (initialMin) {
    initialMin = parseInt(initialMin);
  }
  if (initialMax) {
    initialMax = parseInt(initialMax);
  }
  if (series) {
    series = [series];
  } else {
    series = ["arm", "amd", "dave_intel"];
  }
  const fidelity = numPoints => {
    let res = 1;
    while (numPoints / res > 2000) {
      res += 1;
    }
    return res;
  };

  const abs = Math.abs;

  Promise.all(series.map(k => seriesData[k][1]().then(a => [k, a]))).then(data => {

    const lineColors = data.map(a => seriesData[a[0]][0]);
    data = data.map(a => a[1]);
    initialMin ||= 0;
    initialMax ||= data[0].data.at(-1)[0];


    const boundData = (minX, maxX) => {
      const numPoints = maxX - minX + 1;
      const skip = fidelity(numPoints);
      const ewma = mkEWMA(numPoints/50);
      return data.map(({data,label}) => {
        const points = data.slice(Math.max(0, minX), maxX).filter(([x, y], i) => {
          return i % skip == 0 || abs(ewma(y) - y) > 70;
        });
        return {
          label,
          data: points
        };
      });
    };
    data = boundData(initialMin, initialMax);
    element.innerText = "";

    const graphConfig = {
      width: 800,
      height: 500,
      maxTicks: {x: 10, y: 10},
      lineColors,
      axisLabels: {
        x: 'Struct size',
        y: 'Time (ns/call)'
      },
      data,
      loadData: boundData
    };

    let svg = drawGraph(graphConfig);
    element.appendChild(svg);
  });
})
</script>

[^1]: Why on earth do I have to [work around bugs](https://github.com/414owen/smolgraph/commit/39d21a48733c3c57877ee880febb78a8dac9f318) in modern browsers in 2025.
