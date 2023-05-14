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

    star <? do
      opacity 0.3
      sym2 padding (px 20) (px 30)
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
        userSelect none

      label <? do
        position absolute
        width (pct 100)
        height (pct 100)
        cursor pointer

  "#techs-container" ? do
    display flex
    flexDirection columnReverse
    C.input |+ star <? do
      animationName "fade-in"
      animationDuration (sec 0.3)
      textAlign center
      maxWidth (px 300)
      margin px20 auto nil auto
      img <? do
        display block
        height (px 150)
        sym2 margin px20 auto


      C.div |> C.div # ".links" |> a <?
        sym2 margin nil (px 10)

  "#tech-none" # ":checked" |~ "#techs" |> star ? opacity 1.0
