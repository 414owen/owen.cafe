{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Css.Default where

import qualified Data.Text as T
import Clay as C hiding (id, sticky)
import qualified Clay.Stylesheet as CS
import qualified Clay.Media as CM

hbSize :: Size LengthUnit
hbSize = (px 50)

type FourStyle a = Size a -> Size a -> Size a -> Size a -> Css

applyFour :: (b -> Size LengthUnit) -> FourStyle LengthUnit -> b -> b -> b -> b -> Css
applyFour measure rule a b c d = rule (measure a) (measure b) (measure c) (measure d)

applyFourSame :: (b -> Size LengthUnit) -> FourStyle LengthUnit -> b -> Css
applyFourSame measure rule a = applyFour measure rule a a a a

reset :: Css
reset = do
  applyFour id margin nil nil nil nil
  applyFour id padding nil nil nil nil

fg :: Color
fg = "#eee"

bg :: Color
bg = "#111"

px20 :: Size LengthUnit
px20 = px 20

vsepGrad :: BackgroundImage
vsepGrad = linearGradient (angular $ deg 0) [(transparent, pct 0), ("#fff", pct 50), (transparent, pct 100)]

padAll :: Size LengthUnit -> Css
padAll = applyFourSame id padding

margAll :: Size LengthUnit -> Css
margAll = applyFourSame id margin

pad20 :: Css
pad20 = padAll px20

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
    fontFamily [] [sansSerif]

  star ?
    boxSizing borderBox

  ".sidebar" ? do
    display flex

    ".desk" & do
      width (px 300)
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


    ".vsep" <? do
      display inlineBlock
      right nil

  "#hb" # C.not ":checked" |+ "#page" C.** ".sidebar" # ".mob" ?
    hide
  "#hb" ? hide
  "#hb" # ":checked" |+ "#page" |> "#mobcontainer" |> "#content" ? hide


  ".vsep" ? do
    width $ px 1
    minWidth (px 1)
    height (pct 100)
    background vsepGrad

  ".links" <? do
     width (pct 100)
     padding (px 40) (px 40) (px 40) (px 40)

     a <? do
       color fg
       display block
       fontSize px20
       pad20
       textDecoration none

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

  "#mobcontainer" ? do
    flexGrow 1
    isNotMobile $ do
      height (vh 100)
      overflow auto

  "#content" ? do
    padAll (px 60)
    maxWidth (px 600)
    main_ |> p <? do
      marginTop (px 40)
      marginBottom nil
    ":first-child" & marginTop nil

  a ? do
    color inherit

  "#page" ? do
    isNotMobile $ display flex
