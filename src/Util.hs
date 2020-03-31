{-# LANGUAGE OverloadedStrings #-}

module Util where

import Text.Blaze.Internal (attribute, Attribute, AttributeValue)
 
both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

xmlnsXlink :: AttributeValue  -- ^ Attribute value.
           -> Attribute       -- ^ Resulting attribute.
xmlnsXlink = attribute "xmlns:xlink" " xmlns:xlink=\""
