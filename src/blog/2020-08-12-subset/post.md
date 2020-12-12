# Subset

## Set Refresher

```
sets are written like this:
A = {1, 2}
B = {1, 2, 3}

A is a subset of B
A ⊆ B
B is a superset of A
B ⊇ A

subsets are transitive, ie.
A ⊆ B and B ⊆ C ⇒ A ⊆ C
```

In programming, **types represent sets of values**.

## Encoding

### Sets

The mathematical notation

```
Bool = { True, False }
```

can be written in Haskell as

<!-- listing 01-bool-set.hs -->

Haskell datatypes are encoded as the sum of products, however our examples will only cover simple sum types.

### Set Relations

A subset is a relation between sets.

In Haskell, we relate types using typeclasses.

We can encode a subset as a typeclass like this:

<!-- listing 02-subset-class.hs -->

This reads:

Every element of set `a` for which there is a superset `b`, can be recontextualized into `b` using `grow`.

Some elements of set `b` for which there is a subset `a`, can be recontextualized into `a` using `shrink`.

#### Let's try it out:

<!-- listing 03-subset-instances.hs -->

Neat! We can now call it with:

```
> grow @Int8 @Int16 1
1

> :t grow @Int8 @Int16 (1 :: Int8)
grow @Int8 @Int16 (1 :: Int8) :: Int16

> grow @Int16 @Int32 $ grow @Int8 @Int16 2
2

> shrink @Int16 @Int32 1
Just 1

> shrink @Int16 @Int32 100000
Nothing
```

### Transitivity

The transitive property can be expressed as the composition of set growth, and the fail-propagated composition of shrinking kleisli arrows.

<!-- listing 04-transitive-property.hs -->

Unfortunately, this overlaps with our previous instances.
This is because the type-checker chooses instances purely based on the right hand side of the constraint arrow (=>),
so for example `grow @Int8 @Int16` could refer to the explicit implementation over those types, or `a` could unify with `Int8`, and `b` could unify with `Int16` in our transitive instance.

How do we solve this? Well, we can add `{-# Overlapping #-}` pragmas to our instances, and the typechecker will by default choose the most specific instance it finds.

<!-- listing 05-overlapping-pragmas.hs -->

Sweet, that typechecks, and our single-jump instances still work, but the transitive property still isn't usable:

```
> grow @Int8 @Int16 1
1

> grow @Int8 @Int32 1
• Overlapping instances for Subset b0 Int32
    arising from a use of ‘grow’
  Matching instances:
    instance [safe] (Subset a b, Subset b c) => Subset a c
    instance [overlapping] [safe] Subset Int16 Int32
```

What's going on here?

Well, the type checker is working backwards to `Int8` from `Int32` (type checking is a form of ![constraint satisfaction](https://en.wikipedia.org/wiki/Constraint_satisfaction_problem), and has no formally defined direction). It sees two options, either `b` is `Int16`, or `b` is not `Int16`. 

We have two paths forward here. We can use `FunctionalDependencies` to make sure the type checker always chooses an instance over concrete types, by stating that for every type there is at most one subset, or at most one superset.

This would look like:

<!-- listing 06-functional-dependencies-superset.hs -->

or this:

<!-- listing 07-functional-dependencies-subset.hs -->

In the `a -> b` case, the type checker unifies `a ~ Int8`. `a` uniquely identifies `b`, therefore `Subset a b` can't be chosen, as it overlaps with `Subset Int8 Int16`.

Unfortunately, sets can have multiple subsets, and multiple supersets, in other words:

```
A ⊆ B ∧ A ⊆ C ∧ B ⊆ C ⇏ F
```
