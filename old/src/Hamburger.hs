{-# LANGUAGE OverloadedStrings #-}

module Hamburger (hamburger, duration) where

import qualified Data.Text as T
import Text.Blaze.Svg11 as S
import Text.Blaze.Svg11.Attributes as A
import qualified Text.Blaze.XHtml5.Attributes as HA

import Util

hPath :: T.Text
hPath = "M2,3L5,3L8,3M2,5L8,5M2,7L5,7L8,7"

xPath :: T.Text
xPath = "M3,3L5,5L7,3M5,5L5,5M3,7L5,5L7,7"

withDims :: Svg -> Svg
withDims a = a ! width "10" ! height "10"

duration :: AttributeValue
duration = "0.3s"

freeze :: Attribute
freeze = fill "freeze"

click :: Attribute
click = begin "click"

hamxAnimate :: Svg
hamxAnimate = animate ! freeze ! dur duration ! attributename "d"

overAnim :: Svg
overAnim = animate ! dur "0.01s" ! attributename "width"

hamburger :: Svg
hamburger
  = svg
  ! HA.xmlns "http://www.w3.org/2000/svg"
  ! xmlnsXlink "http://www.w3.org/1999/xlink"
  ! version "1.1"
  ! viewbox "0 0 10 10"
  ! stroke "none"
  ! strokeWidth ".6"
  ! fill "rgba(0,0,0,0)"
  ! strokeLinecap "round"
  ! A.style "cursor: pointer"
  $ do
    defs $ do
      hamxAnimate
        ! begin "hamsta.begin"
        ! xlinkHref "#hamx"
        ! values (textValue $ hPath <> ";" <> xPath)
      hamxAnimate
        ! begin "hamrev.begin"
        ! xlinkHref "#hamx"
        ! values (textValue $ xPath <> ";" <> hPath)
      overAnim
        ! freeze
        ! click
        ! xlinkHref "#hamover"
        ! id_ "hamsta"
        ! values "10;0"
      overAnim
        ! click
        ! xlinkHref "#hamunder"
        ! id_ "hamrev"
      overAnim
        ! freeze
        ! begin "hamrev.begin"
        ! xlinkHref "#hamover"
        ! values "0;10"
    S.path ! id_ "hamx" ! d (textValue hPath)  ! stroke "#eee"
    withDims rect ! id_ "hamunder"
    withDims rect ! id_ "hamover"
