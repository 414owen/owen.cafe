{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Debug.Trace
import qualified Data.ByteString.Lazy as LB
import qualified Clay as C
import Data.Function
import qualified Data.Text.Lazy as T
import Data.Text.Encoding
import System.Directory
import System.FilePath

import Css.Default (defaultStyle)
import Index (index)
import RouteTree

cssRoute :: (String, C.Css) -> RouteTree
cssRoute (s, c)
  = C.render c
  & T.toStrict
  & encodeUtf8
  & LB.fromStrict
  & pure
  & File s

imageRoute :: FilePath -> RouteTree
imageRoute fname = File fname $ LB.readFile $ "img" </> fname

routes :: IO [RouteTree]
routes = do
  images <- listDirectory "./img"
  pure
    [ File "index.xhtml" index
    , Dir "css" $ cssRoute <$>
      [ ("default.css", defaultStyle)
      , ("index.css", defaultStyle)
      ]
    , Dir "img" $ imageRoute <$> images
    , Dir "test"
      [ File "index.xhtml" $ pure "DootDoot"
      ]
    ]
