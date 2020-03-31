{-# LANGUAGE OverloadedStrings #-}

module Index (index, indexStyle) where

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

techs :: ClickSwitch
techs = ClickSwitch "tech" (C.opacity 1.0) $ toClickSwitchOption <$>
    ("haskell-anim-d.svg", "haskell")
    :| [("phage-anim-d.svg", "phage")
       , ("svg-d.svg", "svg")
       , ("nix-d.svg", "nix")
       , ("shell-anim-d.svg", "shell")
       , ("lambda-d.svg", "lambda calculus")
       , ("data-anim-d.svg", "data wrangling")
       , ("minizinc-d.svg", "constraint optimisation")
       ]

toClickSwitchOption :: (T.Text, T.Text) -> ClickSwitchOption
toClickSwitchOption (i, d) = let techName = T.takeWhile (/= '-') i in
  ClickSwitchOption techName (toTechImage techName i d) $ H.span $ text techName

toTechImage :: T.Text -> T.Text -> T.Text -> Html
toTechImage name imgsrc desc
  = img ! src (stringValue $ "img" </> T.unpack imgsrc) ! alt (textValue name)
  >> text desc

displays, switches :: Html
techsCss :: C.Css
(switches, displays, techsCss) = renderClickSwitch techs

extraHead :: Html
extraHead = link ! rel "stylesheet" ! href "./css/index.css"

index :: Servable
index = baseTemplate "hi" extraHead $ do
  p "Hi, I'm Owen Shepherd."
  p "I like"
  H.div ! A.style "display: flex; flex-direction: column-reverse" $ do
    displays
    switches

indexStyle :: C.Css
indexStyle = do
  techsCss
  S.indexStyle
