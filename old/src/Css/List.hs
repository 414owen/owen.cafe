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

imgSel :: Selector
imgSel = img <> object

listStyle' :: Css
listStyle' = do
  ".list" ? do
    width (pct 100)
    listStyleType none
    sym padding nil
    sym margin nil
    star # ":hover" |+ star |> ".hsep" # ":first-child" ?
      highlightSep
    ".url" ? do
      float floatRight
      marginLeft (px 20)
      fontSize (em 0.8)
    ".url::before" ? content (stringContent "(")
    ".url::after" ? content (stringContent ")")
    star <? do
      ".hsep" <?
        transition "background-color" (sec 0.3) linear (sec 0)
      ":hover" &
        ".hsep" <?
          highlightSep
    ".media" ? do
      display flex
      alignItems center
      sym padding (px 20)
      C.div <? width (pct 100)
      star |> C.a <? do
        textDecoration underline
        fontWeight bold
      a # ":first-child" <? do
        textAlign center
        minWidth (px 100)
      a |> imgSel <? do
        height (px 80)
        paddingRight (px 20)
      isVeryMobile $ do
        flexDirection column
        a |> imgSel <? do
          paddingRight nil
          paddingBottom (px 10)
        star <? do
          textAlign center
        ".url" ? do
          marginLeft nil
          float none

listStyleRoute :: CssRoute
listStyleRoute = CssRoute ["list"] listStyle'
