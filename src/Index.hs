{-# LANGUAGE OverloadedStrings #-}

module Index (indexRoute) where

import qualified Clay as C
import Control.Category
import Data.List.NonEmpty
import qualified Data.Text as T
import System.FilePath
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A

import Base
import qualified Css.Index as S
import ClickSwitch
import RouteTree
import Util

techs :: ClickSwitch
techs = ClickSwitch "tech" (C.opacity 1.0) $ toClickSwitchOption <$>
    ( "haskell-anim-d.svg"
    , "haskell"
    , do
        "Haskell is an advanced, purely functional programming language."
        H.div ! class_ "links" $ do
          l ! href "https://www.haskell.org/" $ "homepage"
          l ! href "https://github.com/414owen?tab=repositories&q=&type=source&language=haskell" $ "my haskell projects"
    )
    :| [ ( "phage-anim-d.svg"
         , "phage"
         , do
             "Phage is my own homegrown lispy programming language."
             H.div ! class_ "links" $ do
               l ! href "https://github.com/414owen/phage" $ "homepage"
         )
       , ( "svg-d.svg"
         , "svg"
         , do
             "I create animated SVG vector graphics."
             H.div ! class_ "links" $ do
               l ! href "https://codepen.io/shephero" $ "my codepen account"
         )
       , ( "nix-d.svg"
         , "nix"
         , do
             "I use Nix, a purely functional package manager, and a powerful language for expressing environments and builds"
             H.div ! class_ "links" $ do
               l ! href "https://nixos.org/" $ "homepage"
         )
       , ( "shell-anim-d.svg"
         , "shell"
         , do
             "I abuse shell scripts, for example "
             l ! href (textValue "https://github.com/414owen/turtle-svg/tree/master/scripts") $ "creating, rasterizing and stiching together"
             " thousands of svgs into a video."
         )
       , ( "lambda-d.svg"
         , "lambda calculus"
         , do
             "I think good code needs a solid mathematical background."
             H.div ! class_ "links" $ do
               l ! href "https://lambda.how/" $ "my lambda repl"
         )
       , ( "data-anim-d.svg"
         , "data wrangling"
         , do
             "I wrangle data with the best of them; streaming, indexed, (un)structured, realtime."
         )
       , ( "rust-anim-d.svg"
         , "rust"
         , do
             "Rust is fast, safe and expressive. What more could you want?"
         )
       -- , ( "minizinc-d.svg"
       --   , "constraint optimisation"
       --   , do
       --       "I write constraint satisfaction and optimisation programs."
       --   )
       ]

toClickSwitchOption :: (T.Text, T.Text, Html) -> ClickSwitchOption
toClickSwitchOption (imgSrc, desc, longhand) =
  ClickSwitchOption techName switch elem
    where
      techName = T.takeWhile (/= '-') imgSrc
      image = img ! src (stringValue $ "img" </> T.unpack imgSrc) ! alt (textValue techName)
      switch = do
        image
        H.div $ text desc
      elem = H.div $ do
        image
        H.div longhand

displays, switches :: Html
techsCss :: C.Css
(switches, displays, techsCss) = renderClickSwitch techs

index :: Html
index = do
  section $ do
    h2 "I am"
    "Owen Shepherd"
  section ! A.id "techs-container" $ do
    displays
    switches
    h2 "I like"
  H.script $ text clickSwitchScript

indexStyle :: C.Css
indexStyle = do
  techsCss
  S.indexStyle

indexStyleRoute :: CssRoute
indexStyleRoute = CssRoute ["root"] indexStyle

indexRoute :: CafeRoute
indexRoute = CafeRoute [] "home" [indexStyleRoute] index
