{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Css.Blog where

import qualified Data.Text as T
import Clay as C hiding (id, sticky)
import qualified Clay.Stylesheet as CS
import qualified Clay.Media as CM
import qualified Clay.Background as BG
import Clay.Gradient
import Clay.Property (unPrefixed, unValue)

blogStyle :: Css
blogStyle = do
  pre <> code <> ".sourceCode" ? backgroundColor "#222"
  (main_ <> ".multi-listing") |> (pre <> ".sourceCode") ? do
    sym padding $ px 5

  ".multi-listing" # C.not ".expanded" ? do
    transition "opacity" (sec 0.3) linear (sec 0)
    cursor pointer
    ":hover" & opacity 0.5
    (star # firstChild <> star # nthChild "3") <? do
      star <? display none
      "::before" & content (stringContent "...")

  main_ ? maxWidth (px 800)
