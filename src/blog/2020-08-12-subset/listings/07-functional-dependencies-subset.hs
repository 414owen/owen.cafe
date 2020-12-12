{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

import Data.Int

import Control.Monad ((<=<))

-----

-- each type has at most one subset
class Subset a b | b -> a where
  grow   :: a -> b
  shrink :: b -> Maybe a

-----

-- Try to shrink an integral
shrinkInt :: forall a b. (Integral a, Bounded b, Integral b) => a -> Maybe b
shrinkInt a | a > fromIntegral (maxBound @b) = Nothing
            | otherwise = pure $ fromIntegral a

instance (Subset a b, Subset b c) => Subset a c where
  grow   = grow @b @c . grow @a @b
  shrink = shrink @a @b <=< shrink @b @c
           
instance {-# Overlapping #-} Subset Int8 Int16 where
  grow   = fromIntegral
  shrink = shrinkInt @Int16 @Int8

instance {-# Overlapping #-} Subset Int16 Int32 where
  grow   = fromIntegral
  shrink = shrinkInt @Int32 @Int16
