{-# LANGUAGE OverloadedStrings #-}

module Css.Index (indexStyle) where

import qualified Data.Text as T
import Clay as C
import qualified Clay.Flexbox as CF
import Css.Default

indexStyle :: Css
indexStyle = do
  "#techs" ? do
    display flex
    flexWrap CF.wrap
    alignItems CF.stretch

    star <? do
      opacity 0.5
      padAll (px 25)
      width (px 110)
      position relative
      transition "opacity" (sec 0.1) linear (sec 0)
      display flex
      flexDirection column
      justifyContent CF.flexEnd
      alignItems center
      textAlign center

      img <? do
        width (pct 100)
        marginBottom (px 10)

      label <? do
        position absolute
        width (pct 100)
        height (pct 100)
        cursor pointer

  "#tech-none" # ":checked" |~ "#techs" |> star ?
    opacity 1.0
