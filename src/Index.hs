{-# LANGUAGE OverloadedStrings #-}

module Index (index) where

import Text.Blaze.Html
import Text.Blaze.XHtml5 as H
import Text.Blaze.Html5.Attributes as A

import Base
import RouteTree
  
index :: Servable
index = baseTemplate "buss" $ do
  h2 "Bottom Up Static Sites"
  h3 "For people who"
  ul $ do
    li "Want the full abstractive power of Haskell"
    li "Like having a little server that reloads"
    li "Prefer functions to templating systems"
    li "Use nix (preferably)"
  img ! alt "haskell" ! src "img/haskell.svg"
  img ! alt "nix" ! src "img/nix.svg"
