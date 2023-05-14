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

heading :: Html -> Html
heading = h2

vid :: T.Text -> T.Text -> Html
vid name url = do
  heading $ text name
  H.div ! class_ "aspect a16-9" $
    frame ! A.title (textValue name)
          ! src (textValue $ "https://www.youtube-nocookie.com/embed/7hOF8Ei-ys4")
          $ mempty

allowfullscreen :: Attribute
allowfullscreen = BI.customAttribute "allowfullscreen" ""

pen :: T.Text -> T.Text -> Html
pen name url = do
  heading $ text name
  H.div ! class_ "aspect a10-8" $
    frame ! A.title (textValue name)
          ! src (textValue $ "https://codepen.io/shephero/embed/preview/"
                 <> url
                 <> "?theme-id=dark&default-tab=result")
          ! allowfullscreen
          $  mempty

graphicsPage :: H.Html
graphicsPage = H.div ! class_ "frames" $ do
  pen "Tree Declarator" "zYOZoWN"
  pen "Warp Speed" "QWLpGBW"
  pen "Gravitational Fireflies" "jrqrWW"
  pen "Declarative Spirangle" "rNByWvM"
  vid "Fractal Trees" "rte"

graphicsRoute :: CafeRoute
graphicsRoute = CafeRoute ["graphics"] "graphics" [frameStyleRoute] graphicsPage
