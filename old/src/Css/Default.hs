{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Css.Default where

import qualified Data.Text as T
import Clay as C hiding (id, sticky)
import qualified Clay.Stylesheet as CS
import qualified Clay.Media as CM
import qualified Clay.Background as BG
import Clay.Gradient
import Clay.Property (unPrefixed, unValue)

hbSize :: Size LengthUnit
hbSize = (px 50)

type FourStyle a = Size a -> Size a -> Size a -> Size a -> Css

reset :: Css
reset = do
  sym margin nil
  sym padding nil

fg :: Color
fg = "#eee"

bg :: Color
bg = "#111"

px20 :: Size LengthUnit
px20 = px 20

sepGradPts :: [(Color, Size Percentage)]
sepGradPts = [(transparent, pct 0), ("#fff", pct 50), (transparent, pct 100)]

sepGrad :: Double -> BackgroundImage
sepGrad ang = linearGradient (angular $ deg ang) sepGradPts

vsepGrad :: BackgroundImage
vsepGrad = sepGrad 0

hsepGrad :: BackgroundImage
hsepGrad = sepGrad 90

pad20 :: Css
pad20 = sym padding px20

mobileBreak :: Double
mobileBreak = 600

isMobile :: Css -> Css
isMobile = query CM.screen [CM.maxWidth $ px (mobileBreak - 1)]

isNotMobile :: Css -> Css
isNotMobile = query CM.screen [CM.minWidth $ px mobileBreak]

hide :: Css
hide = display none

defaultStyle :: Css
defaultStyle = do
  html <> body <> "#page" ? do
    reset
    height (vh 100)

  html ? do
    background bg
    color fg
    fontFamily [] [monospace]
    fontSize (px 13)

  body ? do
    isNotMobile $ display flex

  section # C.not ":first-child" ? marginTop px20

  star ?
    boxSizing borderBox

  keyframesFromTo "fade-in" (opacity 0) (opacity 1)

  ".sidebar" ? do
    display flex

    ".links" <? do
       width (pct 100)
       padding (px 40) (px 40) (px 40) (px 40)

       a <? do
         color fg
         display block
         fontSize px20
         pad20
         textDecoration none

    ".desk" & do
      width (px 350)
      isMobile hide
      textAlign end

      ".links" |> a ?
        pad20

    ".mob" & do
      isNotMobile hide
      background bg
      width (pct 100)
      transition "transform" (sec 0.3) easeInOut (sec 0)

      ".links" |> a ? do
        backgroundColor "#333"
        width (pct 100)
        C.not ":first-child" & marginTop px20
        animationName "fade-in"
        animationDuration (sec 0.3)

    ".vsep" <? do
      display inlineBlock
      right nil

  "#hb" # C.not ":checked" |+ star |+ ".sidebar" # ".mob" ? hide
  "#hb" # ":checked" |~ "#content" ? hide

  ".hsep" ? do
    height $ px 1
    width (pct 100)
    background hsepGrad

  ".vsep" ? do
    width $ px 1
    minWidth (px 1)
    height (pct 100)
    background vsepGrad

  star # ("for" @= "hb") ? do
    zIndex 11
    display block
    background bg
    lineHeight nil
    position relative
    CS.key "position" $ Value (browsers <> "sticky")
    top nil
    svg <? do
      width hbSize
      height hbSize
    isNotMobile hide

  ".hidden" ? hide

  "#content" ? do
    overflow auto
    width (pct 100)
    main_ <? do
      maxWidth (px 600)
      sym padding (px 60)
    ":first-child" & marginTop nil
    isMobile $ do
      animationName "fade-in"
      animationDuration (sec 0.3)
      main_ <? do
        sym padding (px 40)
        paddingLeft (pct 8)
        paddingRight (pct 8)
  "#test" ? do
    display flex
    justifyContent center

  h1 # ":first-child" ? marginTop nil
  h1 ? do
    fontSize $ px 30

  h2 # ":first-child" ? marginTop nil
  h2 ? do
    fontSize $ px 20

  a ? do
    color inherit

