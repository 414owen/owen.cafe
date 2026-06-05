---
title: "A faster bump allocator for rust"
date: 2026-06-02T23:02:39+01:00
summary: "Announcing stumpalo, an extremely fast bump allocator for rust, with chunk reuse and safe scoped stack support."
---

<a href="https://codeberg.org/414owen/stumpalo"><img alt="Badge" class="left" src="https://img.shields.io/badge/codeberg-repo-blue?logo=codeberg&logoColor=white"></a>
<a href="https://docs.rs/stumpalo/"><img alt="Badge" class="left" src="https://docs.rs/stumpalo/badge.svg"></a>
<a href="https://crates.io/crates/stumpalo"><img alt="Badge" class="left" src="https://img.shields.io/crates/v/stumpalo.svg"></a>
<a href="https://lobste.rs/s/vta6wp/faster_bump_allocator_for_rust"><img alt="Badge" class="left" src="https://img.shields.io/badge/lobste.rs-discuss-blue"></a>


Say hello to stumpalo.

Stumpalo is a bump allocator.

Stumpalo has scoped stack support.

Stumpalo is extremely fast.

Stumpalo has a logo, created very hastily.

Stumpalo's logo is stumpy:

<img width="200px" alt="stumpy, the logo" src="/img/stumpy-d.svg">

## Speed

You're probably using a bump allocator because you want raw
allocation throughput.

Let's see how fast stumpalo is, compared to other libraries.

| operation               | stumpalo    | blink        | bumpalo   |
|:---                     |:---         |:---          |:---       |
| alloc_u8                | ✅  1.00x   | 🔴  2.14x    | 🟠  1.54x |
| alloc_u16               | ✅  1.00x   | 🔴  2.46x    | 🟥  2.54x |
| alloc_u32               | ✅  1.00x   | 🟥  3.36x    | 🟥  3.34x |
| alloc_u64               | ✅  1.00x   | 🟥  3.35x    | 🟥  3.34x |
| alloc_u128              | ✅  1.00x   | 🟡  1.19x    | 🟡  1.18x |
| alloc_multiple_u8       | ✅  1.00x   | 🔴  1.82x    | 🔴  1.85x |
| alloc_multiple_u16      | ✅  1.00x   | 🔴  2.30x    | 🔴  2.34x |
| alloc_multiple_u32      | ✅  1.00x   | 🟥  3.12x    | 🟥  3.14x |
| alloc_multiple_u64      | ✅  1.00x   | 🟥  3.23x    | 🟥  3.25x |
| alloc_multiple_u128     | ✅  1.00x   | 🟥  2.70x    | 🟥  2.61x |
| alloc_array_u8_8        | ✅  1.00x   | 🔴  1.99x    | 🔴  2.11x |
| alloc_array_u8_32       | ✅  1.00x   | 🟢  1.15x    | 🟡  1.20x |
| alloc_array_u8_64       | ✅  1.00x   | 🟠  1.55x    | 🟠  1.59x |
| alloc_array_u8_128      | ✅  1.00x   | 🟡  1.30x    | 🟠  1.50x |
| alloc_slice_u8_8        | 🟢  1.11x   | 🟡  1.27x    | ✅  1.00x |
| alloc_slice_u8_32       | 🟢  1.06x   | ✅  1.00x    | 🟢  1.08x |
| alloc_slice_u8_64       | ✅  1.05x   | ✅  1.00x    | 🟢  1.09x |
| alloc_slice_u8_128      | ✅  1.00x   | 🟢  1.06x    | ✅  1.04x |
| alloc_slice_u16_8       | ✅  1.00x   | 🟡  1.33x    | 🟡  1.16x |
| alloc_slice_u16_32      | ✅  1.00x   | 🟢  1.14x    | 🟢  1.11x |
| alloc_slice_u16_64      | ✅  1.00x   | 🟢  1.14x    | 🟢  1.10x |
| alloc_slice_u16_128     | ✅  1.04x   | ✅  1.00x    | ✅  1.02x |
| alloc_slice_u32_8       | ✅  1.00x   | 🟢  1.14x    | 🟢  1.09x |
| alloc_slice_u32_32      | ✅  1.00x   | 🟢  1.14x    | 🟢  1.10x |
| alloc_slice_u32_64      | ✅  1.05x   | ✅  1.00x    | 🟢  1.06x |
| alloc_slice_u32_128     | 🟢  1.09x   | ✅  1.00x    | 🟢  1.13x |
| alloc_slice_u64_8       | ✅  1.00x   | 🟡  1.25x    | 🟢  1.11x |
| alloc_slice_u64_32      | ✅  1.04x   | ✅  1.00x    | ✅  1.02x |
| alloc_slice_u64_64      | 🟢  1.08x   | ✅  1.00x    | 🟢  1.10x |
| alloc_slice_u64_128     | 🟢  1.07x   | ✅  1.00x    | 🟢  1.08x |
| alloc_slice_u128_8      | ✅  1.00x   | 🟢  1.12x    | 🟢  1.11x |
| alloc_slice_u128_32     | 🟢  1.08x   | ✅  1.00x    | 🟢  1.12x |
| alloc_slice_u128_64     | 🟢  1.07x   | ✅  1.00x    | 🟢  1.08x |
| alloc_slice_u128_128    | ✅  1.03x   | ✅  1.00x    | ✅  1.04x |
| alloc_struct_13         | ✅  1.00x   | 🟠  1.55x    | 🟠  1.39x |
| alloc_struct_24         | ✅  1.00x   | 🔴  1.94x    | 🔴  1.97x |
| alloc_struct_26         | ✅  1.00x   | 🟠  1.56x    | 🟠  1.52x |
| alloc_struct_30         | ✅  1.00x   | 🟠  1.54x    | 🟠  1.45x |
| alloc_struct_32         | ✅  1.00x   | 🟠  1.35x    | 🟠  1.40x |
| alloc_struct_64         | ✅  1.00x   | 🟠  1.44x    | 🟠  1.48x |
| alloc_struct_96         | ✅  1.00x   | 🟢  1.13x    | 🟡  1.18x |
| alloc_struct_128        | ✅  1.00x   | 🟡  1.33x    | 🟡  1.17x |
| alloc_struct_192        | ✅  1.02x   | ✅  1.00x    | 🟢  1.09x |
| alloc_struct_256        | ✅  1.00x   | 🟡  1.16x    | ✅  1.01x |
| alloc_struct_512        | 🟢  1.06x   | ✅  1.00x    | ✅  1.02x |
| alloc_struct_1k         | ✅  1.00x   | 🟢  1.05x    | ✅  1.01x |
| alloc_str_8             | 🟢  1.11x   | ✅  1.05x    | ✅  1.00x |
| alloc_str_16            | 🟢  1.07x   | ✅  1.02x    | ✅  1.00x |
| alloc_str_32            | ✅  1.04x   | ✅  1.00x    | 🟢  1.07x |
| alloc_str_40            | ✅  1.00x   | 🟢  1.08x    | 🟢  1.06x |
| alloc_str_48            | ✅  1.00x   | ✅  1.03x    | 🟢  1.06x |
| alloc_str_64            | ✅  1.00x   | ✅  1.04x    | 🟢  1.06x |
| alloc_str_72            | ✅  1.04x   | ✅  1.00x    | 🟢  1.07x |
| alloc_str_80            | ✅  1.03x   | ✅  1.00x    | 🟢  1.07x |
| alloc_str_128           | ✅  1.00x   | 🟢  1.11x    | 🟢  1.08x |
| alloc_slice_lit_u8_8    | ✅  1.00x   | 🔴  2.47x    | 🔴  2.23x |
| alloc_slice_lit_u8_32   | ✅  1.00x   | 🔴  1.83x    | 🟠  1.71x |
| alloc_slice_lit_u8_64   | ✅  1.00x   | 🟡  1.34x    | 🟠  1.42x |
| alloc_slice_lit_u8_128  | ✅  1.00x   | 🟡  1.31x    | 🟡  1.31x |
| alloc_str_lit_8         | ✅  1.00x   | 🔴  2.02x    | 🔴  1.82x |
| alloc_str_lit_16        | ✅  1.00x   | 🔴  1.78x    | 🟠  1.60x |
| alloc_str_lit_32        | ✅  1.00x   | 🟠  1.51x    | 🟠  1.42x |
| alloc_str_lit_40        | ✅  1.00x   | 🔴  1.76x    | 🔴  1.93x |
| alloc_str_lit_48        | ✅  1.00x   | 🟠  1.74x    | 🔴  1.82x |
| alloc_str_lit_64        | ✅  1.00x   | 🔴  1.75x    | 🟠  1.69x |
| alloc_str_lit_72        | ✅  1.00x   | 🟠  1.53x    | 🟠  1.61x |
| alloc_str_lit_80        | ✅  1.00x   | 🟠  1.54x    | 🟠  1.63x |
| alloc_str_lit_128       | ✅  1.00x   | 🟠  1.36x    | 🟠  1.35x |
| clear                   | ✅  1.00x   | ✅  1.04x    | ✅  1.04x |
| clear_and_reuse         | ✅  1.00x   | 🟥  3.35x    | 🟥  3.35x |

Benchmark machine: AMD Ryzen 3900x, Arch Linux, kernel 7.0.3

### Where does the speed come from

In an arena allocator, the fast path is everything.
The fast path has to check whether there's room in the current chunk, if so,
allocate the value in the current chunk, and if not, jump to the slow path.

#### Using more information

Rustc / LLVM is able to erase if/else statements whose conditions are expressions known
at compile-time.

Different types have different information available at compile-time. Think alignment and size.
When this information is available, stumpalo uses it, as well as information about the hardware
you're running on, to avoid overflow/underflow checks, when overflow/underflow couldn't
possibly occur anyway.

Generally, stumpalo's fast-paths contain a single conditional branch, and as few as six
instructions.

#### Less indirection

A stumpalo arena contains pointers to the top and bottom of the chunk.
Other libraries contain a pointer to a chunk, whose header contains pointers to their top.
Stumpalo goes through one less layer of indirection to read the top.

#### Example

The following function:
```rust
fn alloc_u32(a: &mut Arena, n: u32) -> &mut u32 {
    a.alloc(n)
}
```

Compiles down to this fast path:

```asm
alloc_u32:
  mov     rcx, qword ptr [rdi]
  and     rcx, -4
  lea     rax, [rcx - 4]
  cmp     rax, qword ptr [rdi + 8]
  jb      example::ArenaRef::alloc_slow_with::h903e68372b5b408b
  mov     dword ptr [rcx - 4], esi
  mov     qword ptr [rdi], rax
  ret
```

That's:

1. Load the top pointer
2. Round top down to a multiple of alignment (4)
3. Subtract size (4) from top
4. Compare top against bottom
5. If less, tail call to the slow path
6. Write the value to the chunk
7. Store the new top
8. Return

If there are multiple allocations in a row, then the first instruction,
loading the top, is avoided. The stub for the slow path is expanded to
update the register.

This is a fairly simple example, but stumpalo produces tiny, fast code
across the board.

## Scoped stacks

Scoped stacks let you use the arena temporarily, and revert it to a previous state once
you're done.

This can be useful as a sort of scratch area. Using the arena after it's been used
as a scratch area will reuse allocations made for the scratch area.

If you're using a bump allocator, you probably like amortizing allocations.
How about amortizing the allocations across uses of your allocation amortizer?
I'm dizzy.

```rust
let mut arena = Arena::new();

// This is necessary if you need to keep references alive from
// the outer scope, after the inner scope returns.
// Otherwise, just call `with_scope` on the arena directly.
let arena = arena.as_arena_ref_mut();

let a = arena.alloc(1u32);

arena.with_scope(|scope: &mut ArenaRef| {
    let temporary = scope.alloc(2u32);
    scope.with_scope(|scope: &mut ArenaRef| {
        // Wow, you can have nested scopes.
        // That might be useful... I guess...
    });
});
// After a scope returns, the arena is reset to its previous position.
// Any chunks allocated for the inner scope are added to a free list for reuse.

// 'a' is still accessible
assert_eq!(*a, 1);
```

A lot of effort went into coming up with a safe API for scoped stacks.
There are tests that various misuses of scopes fail to compile in the
[ui tests directory](https://codeberg.org/414owen/stumpalo/src/commit/main/tests/ui).

---

<br>

Thanks for reading, and thank you to bumpalo, a fantastic bump allocator I've been
using for years.
