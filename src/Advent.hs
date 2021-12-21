{-# LANGUAGE OverloadedStrings #-}

module Advent (adventRoute) where

import qualified Data.Text as T
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A

import RouteTree
import qualified Icons as I

adventPage :: Html
adventPage = do
  H.iframe ! A.src "https://owen.cafe/advent/2021" $ mempty
  H.style $ "iframe,main{height:100%;}iframe{width:100%;border:none}"

adventRoute :: CafeRoute
adventRoute = CafeRoute ["advent"] "advent" [] adventPage
