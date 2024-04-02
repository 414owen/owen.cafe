---
title: "Teaching GHC about Arithmetic Closed Forms"
date: 2024-01-28T12:11:19+01:00
---

I'm going to attempt to teach GHC about the closed form for the sum of a range from 1:

```haskell
sum [1..n] = n * (n + 1) `div` 2
```

There's a closed form for a range that doesn't start at one, too, but it's always a
good idea to start simple.

This blog post serves as a log of the process of attempting to make this change.
I don't know if I'll succeed, but I'll try to keep it pithy.

## Does this need doing?

A good first step is to make sure GHC doesn't already do this.
Let's make a test program:

```haskell
{-# LANGUAGE NumericUnderscores #-}

module Main where

main :: IO ()
main = print $ sum [1 :: Int .. 1_000_000_000]
```

And compile and run it:

```sh
$ ghc Test.hs -O2 -o main
$ time ./main
500000000500000000

real    0m0.242s
user    0m0.239s
sys     0m0.003s
```

This takes a whole fifth of a second to run, clearly showing that the sum is running
in O(n) time. If I halve the value of n (here 1e9), it takes half the time.

## Rewrite rules

GHC has a very neat little system that runs at compile time, called rewrite rules.
They're essentially patterns that match AST fragments, and let you, well, rewrite them.
They're used to implement ad-hoc optimizations, such as "mapping twice is the same as
mapping with the composition of the two map functions". This is written:

```
{-# RULES "map/map" forall f g xs. map f (map g xs) = map (f . g) xs #-}
```

The pattern valiables `f`, `g`, and `xs`, introduced by the `forall`, match any terms
(think runtime values). The concrete terms `map` and `.` are self-explanatory.

## First attempt

At first glance, it should be easy enough to add a rewrite rule:

```
{-# RULES "sum of range from 1" forall (n :: Integral a => a). sum [1 .. n] = n * (succ n) `div` 2 #-}
```

This doesn't seem to work.

{{< figure src="/img/rules-effectiveness.png" alt="Pokemon ghc RULES not very effective meme" class="pix gb smallimg" >}}


Luckily, GHC is helpful, and gives us some hints:

```
Test.hs:5:11: warning: [GHC-87502] [-Winline-rule-shadowing]
    Rule "sum of range from 1" may never fire
      because rule "Class op fromInteger" for ‘fromInteger’ might fire first
    Suggested fix: Add phase [n] or [~n] to the competing rule
  |
5 | {-# RULES "sum of range from 1" forall (n :: Integral a => a). sum [1 .. n] = n * (succ n) `div` 2 #-}
  |           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Test.hs:5:11: warning: [GHC-87502] [-Winline-rule-shadowing]
    Rule "sum of range from 1" may never fire
      because rule "Class op enumFromTo" for ‘enumFromTo’ might fire first
    Suggested fix: Add phase [n] or [~n] to the competing rule
  |
5 | {-# RULES "sum of range from 1" forall (n :: Integral a => a). sum [1 .. n] = n * (succ n) `div` 2 #-}
  |           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

Our rule contains overloaded functions, and functions defined in typeclasses implicitly have rules defined
for them implicitly.

`enumFromTo` isn't in our rule at all, but it's simply what the syntactic sugar `[1 .. n]` is desugared to.
This gives us some info in itself. It means that the desugarer is running on our rule.

## Following the advice

The GHC user guide contains a section called [How rules interact with class methods](https://downloads.haskell.org/ghc/9.8.1/docs/users_guide/exts/rewrite_rules.html#how-rules-interact-with-class-methods)
It says that we can work around GHC's implicit `Class op` rule by doing something like this:

```
instance Enum Int  where
  enumFromTo = eftInt
```

This is no longer generic. It will only fire for `Int`, I'll leave the other versions as an exercise for the reader.
The docs do say that there's a way to do it generically, but it would require changing the `Enum` instance, so
that's not going to happen.


