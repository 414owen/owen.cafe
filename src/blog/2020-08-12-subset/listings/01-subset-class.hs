{-# LANGUAGE MultiParamTypeClasses #-}

-----

class Subset a b where
    grow :: a -> b
  shrink :: b -> Maybe a
