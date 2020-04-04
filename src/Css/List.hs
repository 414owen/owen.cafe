{-# LANGUAGE OverloadedStrings #-}

module Css.List (listStyleRoute) where

import qualified Data.Text as T
import Clay as C
import qualified Clay.Media as CM
import qualified Clay.Flexbox as CF
import Css.Default

import RouteTree

highlightSep :: Css
highlightSep = backgroundColor "#777"

isVeryMobile :: Css -> Css
isVeryMobile = query CM.screen [CM.maxWidth $ px 320]

listStyle' :: Css
listStyle' = do
  ".list" ? do
    width (pct 100)
    listStyleType none
    sym padding nil
    sym margin nil
    star # ":hover" |+ star |> ".hsep" # ":first-child" ?
      highlightSep
    star <? do
      ".hsep" <?
        transition "background-color" (sec 0.3) linear (sec 0)
      ":hover" &
        ".hsep" <?
          highlightSep
      ".media" <? do
        display flex
        alignItems center
        sym padding (px 20)
        star |> C.a <? do
          textDecoration underline
          fontWeight bold
        a |> img <? do
          height (px 80)
          paddingRight (px 20)
        isVeryMobile $ do
          flexDirection column
          a |> img <? do
            paddingRight nil
            paddingBottom (px 10)
          star <? do
            textAlign center

listStyleRoute :: CssRoute
listStyleRoute = CssRoute ["list"] listStyle'
