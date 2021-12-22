{-# LANGUAGE OverloadedStrings #-}

module Css.Criterion (criterionStyle) where

import qualified Data.Text as T
import Clay as C
import qualified Clay.Media as CM
import qualified Clay.Flexbox as CF
import Css.Default

criterionStyle :: Css
criterionStyle = do
  "#reports" ? do
    "h2" ? do
      "img" ? do
        height $ px 24
        paddingLeft $ em 1
        paddingRight $ em 1

    "section" <? do
      marginTop $ px 50

  "table" ? do
    borderCollapse collapse
    width $ pct 100

  "tr:not(:last-child)" ? do
    borderBottom solid (px 1) "#333"

  ".top-overview" ? do
    display flex
    justifyContent spaceBetween
