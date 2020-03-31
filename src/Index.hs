{-# LANGUAGE OverloadedStrings #-}

module Index (index) where

import Control.Category
import qualified Data.Text as T
import System.FilePath
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A

import Base
import ClickSwitch
import RouteTree

      -- H.label ! for "lphage" $ mempty
      -- H.label ! for "lnone" $ mempty

techs :: ClickSwitch
techs = toClickSwitchOption <$>
  [ ("haskell-anim-d.svg", "haskell")
  , ("phage-anim-d.svg", "phage")
  , ("svg-d.svg", "svg")
  , ("nix-d.svg", "nix")
  , ("shell-anim-d.svg", "shell")
  , ("lambda-d.svg", "lambda calculus")
  , ("data-anim-d.svg", "data wrangling")
  , ("minizinc-d.svg", "constraint optimisation")
  ]

toClickSwitchOption :: (T.Text, T.Text) -> ClickSwitchOption
toClickSwitchOption (i, d) = let techName = T.takeWhile (/= '-') i in
  ClickSwitchOption techName (toTechImage techName i) $ text "hi"

toTechImage :: T.Text -> T.Text -> Html
toTechImage name imgsrc
  = img ! src (stringValue $ "img" </> T.unpack imgsrc) ! alt (textValue name)

displays, switches :: Html
(switches, displays) = renderClickSwitch "tech" techs

extraHead :: Html
extraHead = do
  link ! rel "stylesheet" ! href "./css/index.css"

index :: Servable
index = baseTemplate "hi" $ do
  p "Hi, I'm Owen Shepherd."
  displays
  p "I like"
  switches
