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

```haskell
data Bool = True | False
```

Haskell datatypes are encoded as the sum of products, however our examples will only cover simple sum types.

### Set Relations

A subset is a relation between sets.

In Haskell, we relate types using typeclasses.

We can encode a subset as a typeclass like this:

<!-- listing 01-subset-class.hs -->

This reads:

Every element of set `a` for which there is a superset `b`, can be recontextualized into `b` using `grow`.

Some elements of set `b` for which there is a subset `a`, can be recontextualized into `a` using `shrink`.

#### Let's try it out:

```haskell
# Try to shrink an integral
shrinkInt :: forall a b. (Integral a, Bounded b, Integral b) => a -> Maybe b
shrinkInt a | a > fromIntegral (maxBound @b) = Nothing
            | otherwise = pure $ fromIntegral a
            
instance Subset Int8 Int16 where
  grow = fromIntegral
  shrink = shrinkInt @Int16 @Int8

instance Subset Int16 Int32 where
  grow = fromIntegral
  shrink = shrinkInt @Int32 @Int16
```

Neat! We can now call it like this:

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

```haskell
instance (Subset a b, Subset b c) => Subset a c where
  grow = grow @b @c . grow @a @b
  shrink = shrink @b @c <=< shrink @a @b
```

Unfortunately, this overlaps with our previous instances.
This is because the type-checker chooses instances purely based on the right hand side of the constraint arrow (=>),
so for example `grow @Int8 @Int16` could refer to the explicit implementation over those types, or `a` could unify with `Int8`, and `b` could unify with `Int16` in our transitive instance.

How do we solve this? Well, we can add `{-# Overlapping #-}` pragmas to our instances, and the typechecker will by default choose the most specific instance it finds.

Sweet, that typechecks, and our single-jump instances still work, but if we try to use the transitive property:


```
> grow @Int8 @Int32 1
• Overlapping instances for Subset b0 Int32
    arising from a use of ‘grow’
  Matching instances:
    instance [safe] (Subset a b, Subset b c) => Subset a c
    instance [overlapping] [safe] Subset Int16 Int32
```

```haskell
instance {-# Overlapping #-} Subset Int (Maybe Int) where
  grow = pure
  shrink = id

instance {-# Overlapping #-} Subset Int Int where
  grow = id
  shrink = pure

instance {-# Overlapping #-} Subset Int8 Int8 where
  grow = id
  shrink = pure

instance {-# Overlapping #-} Subset Int16 Int16 where
  grow = id
  shrink = pure

instance {-# Overlapping #-} Subset Int32 Int32 where
  grow = id
  shrink = pure

instance {-# Overlapping #-} Subset Int64 Int64 where
  grow = id
  shrink = pure

instance {-# Overlapping #-} Subset Word8 Word8 where
  grow = id
  shrink = pure

instance {-# Overlapping #-} Subset Word16 Word16 where
  grow = id
  shrink = pure

instance {-# Overlapping #-} Subset Word32 Word32 where
  grow = id
  shrink = pure

instance {-# Overlapping #-} Subset Word64 Word64 where
  grow = id
  shrink = pure

instance {-# Overlapping #-} Subset Int8 Int16 where
  grow = fromIntegral
  shrink = shrinkInt @Int16 @Int8 

instance {-# Overlapping #-} Subset Int16 Int32 where
  grow = fromIntegral
  shrink = shrinkInt @Int32 @Int16 

instance {-# Overlapping #-} Subset Int32 Int64 where
  grow = fromIntegral
  shrink = shrinkInt @Int64 @Int32 

instance {-# Overlapping #-} Subset Int64 Int where
  grow = fromIntegral
  shrink = shrinkInt @Int @Int64 

instance {-# Overlapping #-} Subset Int Integer where
  grow = fromIntegral
  shrink = shrinkInt @Integer @Int

instance {-# Overlapping #-} Subset Word8 Word16 where
  grow = fromIntegral
  shrink = shrinkInt @Word16 @Word8 

instance {-# Overlapping #-} Subset Word16 Word32 where
  grow = fromIntegral
  shrink = shrinkInt @Word32 @Word16 

instance {-# Overlapping #-} Subset Word32 Word64 where
  grow = fromIntegral
  shrink = shrinkInt @Word64 @Word32 

instance {-# Overlapping #-} Subset Word64 Word where
  grow = fromIntegral
  shrink = shrinkInt @Word @Word64
```
