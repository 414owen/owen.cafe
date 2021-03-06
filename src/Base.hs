{-# LANGUAGE OverloadedStrings #-}

module Base (baseTemplate) where

import qualified Data.Text as T
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A

import Hamburger
import RouteTree

baseTemplate :: Html -> Html -> Html -> Html -> Servable
baseTemplate extraHead title cafeNav rest = pure $ renderHtml $
  docTypeHtml ! xmlns "http://www.w3.org/1999/xhtml" ! lang "en" $ do
    H.head $ do
      meta ! charset "utf-8"
      H.title (title <> " - owen.cafe")
      meta ! name "theme-color" ! content "#000"
      meta ! name "description" ! content "Owen Shepherd's portfolio page and personal website"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1"
      link ! type_ "text/css" ! rel "stylesheet" ! href "/css/default.css"
      link ! rel "apple-touch-icon" ! href "/favicon.png"
      extraHead
    body $ do
      input ! A.id "hb" ! class_ "hidden" ! type_ "checkbox"
      H.label ! for "hb" $ hamburger
      H.div ! class_ "sidebar mob" $ cafeNav
      H.div ! class_ "sidebar desk" $ do
        cafeNav
        H.div ! class_ "vsep" $ mempty
      H.div ! A.id "content" $ main rest
      H.script "document.getElementById(\"hb\").checked = false;"
