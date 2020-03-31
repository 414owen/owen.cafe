{-# LANGUAGE OverloadedStrings #-}

module Nav (cafeNav) where

import qualified Data.Text as T
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A

cafeNav :: Html
cafeNav = nav ! class_ "links" $ do
  a ! href "/" $ "Home"
  a ! href "/music" $ "Music"

