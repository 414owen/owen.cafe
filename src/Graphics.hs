{-# LANGUAGE OverloadedStrings #-}

module Graphics (graphicsRoute) where

import qualified Data.Text as T
import qualified Text.Blaze.Internal as BI
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A

import Css.Frames
import RouteTree

frame :: Html -> Html
frame = iframe ! A.style "border: none"

vid :: T.Text -> Html
vid url =
  H.div ! class_ "aspect a16-9" $
    frame ! src (textValue $ "https://www.youtube-nocookie.com/embed/7hOF8Ei-ys4")
          $ mempty

allowfullscreen :: Attribute
allowfullscreen = BI.customAttribute "allowfullscreen" ""

pen :: T.Text -> Html
pen  url =
  H.div ! class_ "aspect a10-8" $
    frame ! allowfullscreen
          ! src (textValue $ "https://codepen.io/shephero/embed/preview/"
                 <> url
                 <> "?theme-id=dark&default-tab=result") $ mempty

graphicsPage :: H.Html
graphicsPage = H.div ! class_ "frames" $ do
  h3 "Tree Declarator"
  pen "zYOZoWN"
  h3 "Warp Speed"
  pen "QWLpGBW"
  h3 "Gravitational Fireflies"
  pen "jrqrWW"
  h3 "Declarative Spirangle"
  pen "rNByWvM"
  h3 "Fractal Trees"
  vid "rte"

graphicsRoute :: CafeRoute
graphicsRoute = CafeRoute ["graphics"] "graphics" [frameStyleRoute] graphicsPage
