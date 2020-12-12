{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

import Data.Int

import Control.Monad ((<=<))

class Subset a b where
  grow   :: a -> b
  shrink :: b -> Maybe a

-- Try to shrink an integral
shrinkInt :: forall a b. (Integral a, Bounded b, Integral b) => a -> Maybe b
shrinkInt a | a > fromIntegral (maxBound @b) = Nothing
            | otherwise = pure $ fromIntegral a
            
instance Subset Int8 Int16 where
  grow   = fromIntegral
  shrink = shrinkInt @Int16 @Int8

instance Subset Int16 Int32 where
  grow   = fromIntegral
  shrink = shrinkInt @Int32 @Int16

-----

instance (Subset a b, Subset b c) => Subset a c where
  grow   = grow @b @c . grow @a @b
  shrink = shrink @a @b <=< shrink @b @c
