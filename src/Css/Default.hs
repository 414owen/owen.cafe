{-# LANGUAGE OverloadedStrings #-}

module Css.Default (defaultStyle) where

import Clay

defaultStyle :: Css
defaultStyle = img ? do
  width (px 100)
  padding (px 20) (px 20) (px 20) (px 20)
