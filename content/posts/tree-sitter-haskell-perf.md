---
title: "Speeding up tree-sitter-haskell 50x"
date: 2023-08-05T19:48:15+02:00
tags:
- performance
- C
- C++
- lexing
---

A while ago I sped up [tree-sitter-haskell](https://github.com/tree-sitter/tree-sitter-haskell) by 50x. I'm going to go through the
process of discovering and fixing the worst bottleneck.

<!--more-->

This post probably won't help you speed up other
[tree-sitter](https://tree-sitter.github.io/tree-sitter/) grammars.

---

A huge thank-you to everyone who contributes to tree-sitter-haskell, especially
the current maintainer, Torsten Schmits. Haskell has a lot of syntax, and
tree-sitter-haskell is a hugely impressive project.

## How it started

At some point in 2021 I switched to the [helix](https://github.com/helix-editor/helix/)
code editor. Helix is a modal editor, with kakoune-inspired keybindings.
The editor is a single binary, written in rust, whereas kakoune shells out
to bash to implement extensions. They're both great editors. If you're a vim user,
I seriously urge you to consider giving helix a try. Why? Because
`selection then action`, is a far more visual and intuitive editing paradigm than
`action then region`, in my opinion[^1].

Helix uses tree-sitter to implement syntax highlighting.

## What is Tree-sitter

Tree-sitter is a library for
generating incremental parsers. An incremental parser is one which can reuse
previous parse results when only a part of the buffer changes.

For instance, if I've declared functions `a`, `b`, and `c`, and then I change
(the text in the body of) function `c`, a tree-sitter re-parse might only have
to run through the tokens in function `c`. The memoization is as granular as
the ambiguity of your grammar allows for.

## What's the problem

While editing some haskell, I realised that helix was lagging. Inserting a
character was taking a ridiculous amount of time. On some buffers, it took half
a second.

{{< figure src="/memes/editor-lag.jpg" alt="editor lag is not very cash money" class="smallimg" >}}

The main reason I was using helix was because it was meant to be fast.
No lua, no vimscript, no elisp, no bash, just a fresh stream of organic
x86_64 instructions.

It didn't take long to figure out that disabling syntax highlighting (and
tree-sitter along with it) got rid of the lag.

## Where did the cycles go?

[A bug report](https://github.com/tree-sitter/tree-sitter-haskell/issues/41)
had already been posted in the tree-sitter-haskell project.

I ran a quick profile of helix, using [cargo-flamegraph](https://github.com/flamegraph-rs/flamegraph).
Here's where the vast majority of the time was going:

{{< figure src="/img/helix-flamegraph-crosssection.svg" alt="helix flamegraph" >}}

Most of the time in my profile was taken up by
`tree_sitter_haskell_external_buffer_scan`.

Scanning, or tokenizing, or lexing, means breaking the buffer up into discrete
tokens. I'll use the term lexer for this.

In tree-sitter-haskell, the lexer was
[a C++ function](https://github.com/tree-sitter/tree-sitter-haskell/blob/2e6acc02e3f92213576209bf31ddea971b0f45ee/src/scanner.cc#L1619).

If we zoom into where this time was taken, we get to some leaf nodes:

{{< figure src="/img/helix-flamegraph-leaf.svg" alt="helix flamegraph" >}}

Aaaand it was malloc. Something in the lexer was calling malloc. A lot.

I'm not a C++ expert, or even an amateur, but it looks like `std::function<Result (State&)>::function`
is one of the [std::function](https://gcc.gnu.org/onlinedocs/libstdc++/libstdc++-html-USERS-4.3/a00931.html)
constructors, and `std::_Function_handler< ... >::_M_manager` is [this thing](https://gcc.gnu.org/onlinedocs/libstdc++/libstdc++-html-USERS-4.1/class___function__handler_3_01void(___g_l_i_b_c_x_x___t_e_m_p_l_a_t_e___a_r_g_s)_00_01___member_01___class_1_1_5_01_4.html#1f77840cd286a62c5e3478a2a2347173).

## How the lexer worked

The lexer was defined by a parser combinator library. The available
function types to be combined were:

```c++
typedef function<Result(State&)> Parser;
typedef function<Parser(uint32_t)> CharParser;
typedef function<Parser(Parser)> Modifier;
typedef function<void(State&)> Effect;
typedef function<bool(uint32_t)> Peek;
typedef function<bool(State&)> Condition;
typedef function<pair<bool, uint32_t>(State &)> PeekResult;
```

The first thing to note, is that the lexer uses the term `Parser`.
This isn't a problem when you're working on the project, as the
lexer is self-contained, but in this blog post, please remember
that a `Parser` (with a capital `P`) means a function that lexes
a single token.

So a `Parser` is a function from some state to a result. A `Result`
indicates whether the parse was successful, and if it was, has some
data about the token.

In functional programming languages, it's very common to write parsers
using parser combinators.

The type of a parser in such a library might look this (in Haskell):

```haskell
type Parser a = [Char] -> Either ParseError (a, [Char])
```

Which reads "a parser that returns an `a`, is a function that takes a
list of characters, and returns either a parse error, or the result
(`a`), and the input characters that weren't consumed".

This was more or less how the lexer worked, albeit with some tweaks,
such as the parsers taking and mutating a state reference.

## Dropping parser combinators

IMO a lexer shouldn't call malloc, unless it returns all tokens in
one call, and has to allocate space to store them in.

An obvious solution to this was to drop the use of `std::function`.

The parsers are formulated like this:

```c++
Parser main =
  skipspace +
  eof +
  mark("main") +
  ...
```

Where `+` is an operator for combining the parsers:

```c++
Parser operator+(Parser fa, Parser fb) {
  return [=](State & state) {
    auto res = fa(state);
    return res.finished ? res : fb(state);
  };
}
```

This means, try the first parser, and if it doesn't succeed,
try the second parser.

---

It's probably worth noting that this is quite an unusual design for
a lexer. For instance, if you were to define two tokens with regex:

```js
import_kw = /import/;
binding = /[a-zA-Z][a-zA-Z0-9]*/;
```

And give it the string `important`, most lexer generators would
generate code that parses a binding with the value `"important"`,
not the `import` keyword, followed by the binding `"ant"`.

`tree-sitter-haskell` on the other hand, combines `Parsers`
(meaning lexers) in an order-dependent manner, with backtracking,
so which version you would get depends on whether you wrote
`import_kw + binding` or `binding + import_kw`.

Luckily, most keywords aren't dealt with in the lexer, but in the
actual parser (note the *lowercase* `p`).

---

If we inline the definition of the `+` combinator at its callsites,
it avoids the creation of `function` objects, and the parser
`main` above becomes:

```c++
Result main(State &state) {
  auto res = skipspace(state);
  if (res.finished) return res;
  res = eof(state);
  if (res.finished) return res;
  mark("main", state);
  ...
```

The new version of the `main` parser is compatible with the old
version, as it's still a function that takes a `State&`
and returns a `Result`. This means I could rewrite enough of the
lexer and see if there was any speedup, without rewriting the
whole thing.

The benchmark I was using originally took `1.06s`.

After applying similar transformations to a few of the definitions
in the lexer, the benchmark took `0.29s`, a `3.65x` speedup.

This first proof-of-concept was made into [a pull-request](https://github.com/tree-sitter/tree-sitter-haskell/pull/52).

I completed the removal of all parser combinators with the 85-commit [part 2](https://github.com/tree-sitter/tree-sitter-haskell/pull/54).

This brought the (maximum tested) speedup to `48.2x`. It would be nice to
get that up to a round 50 though.

## Downgrading specs

`Tree-sitter-haskell` was actually [disabled in `helix`](https://github.com/helix-editor/helix/issues/1384)
by default, because it used `C++14` features, and so wouldn't compile on macOS.

To fix this I [downgraded the codebase's requirement to C++03](https://github.com/tree-sitter/tree-sitter-haskell/pull/58),
but in for a penny, in for a pound, why not just [ditch C++ and go straight to C](https://github.com/tree-sitter/tree-sitter-haskell/pull/62)?

This makes wasm builds easier, and brought the `haskell-language-server` benchmark speedup to `52.8x`.

## Loose ends

### Are parser combinators slow?

Well, not necessarily. In Haskell, data is allocated in blocks, and divvied
out by the RTS. If I understand correctly, in (some?) garbage
collected languages, allocation performance can be more similar to an
[arena allocator](https://en.wikipedia.org/wiki/Region-based_memory_management)
than to a general-purpose allocator (like `malloc`).

The definition of Haskell's `allocate()` seems to be [here](https://gitlab.haskell.org/ghc/ghc/-/blob/3b373838e08e2e2b43fab9f0a008fb60325d31e0/rts/sm/Storage.c#L1088),
and looking at the hotpath, it would appear to be quite fast.

This is somewhat anecdotal evidence, but, while I have worked on Haskell
codebases where garbage collection was taking upto 10% of the runtime[^2],
allocation itself has never shown up on one of my profiles[^3].

### Is std::function slow?

I read that it can avoid the allocations in some
cases, like when it's given a normal top-level function pointer.

It probably also depends on usage. I would hope that it's the construction of
`std::function`s that was allocating, not the invocation.

### Is there a better way to write this code

Probably, when I write lexers from scratch in C, I generate a
state machine with [re2c](https://re2c.org/) (regex to c).

In the generated code, the current state is represented by
the Program Counter, and transitions are implemented as switches
on characters.

So instead of trying `Parser`s in order, you could write a declarative
description of tokens using their regex, and generate an efficient,
non-backtracking lexer. This might not work for the whole lexer, but
when choosing between the different tokens that are valid in a position,
this is probably the right way to go.

## Takeaways

* Flamegraphs will lead you to the low-hanging fruit.
* Some abstractions work better in different languages.


[^1]: But also objectively

[^2]: This is not normal, usually I observe 2-3%

[^3]: Although I'd probably have to run through `perf` for this to show up,
and I usually use the debugging information provided by the RTS, instead of
profiling the RTS itself.
