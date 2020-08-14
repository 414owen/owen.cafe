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

  ".multi-listing" # C.not ".before" |> star # firstChild ? display none
  ".multi-listing" # C.not ".after" |> star # nthChild "3" ? display none

  main_ ? maxWidth (px 800)
