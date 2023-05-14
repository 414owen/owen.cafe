{-# LANGUAGE OverloadedStrings #-}

module Css.Frames (frameStyleRoute) where

import qualified Data.Text as T
import Clay as C
import qualified Clay.Media as CM
import qualified Clay.Flexbox as CF
import Css.Default

import RouteTree

framesStyle :: Css
framesStyle = do
  ".frames" ? do
    ".a16-9" <? paddingTop (pct 56.25)
    ".a10-8" <? paddingTop (pct 80)
    ".aspect" <? do
      position relative
      height nil
      star <? do
        position absolute
        top nil

    iframe ? do
      width (pct 100)
      height (pct 100)

    h3 <? do
      marginTop (px 50)
      ":first-child" &
        marginTop nil

frameStyleRoute :: CssRoute
frameStyleRoute = CssRoute ["frames"] framesStyle
