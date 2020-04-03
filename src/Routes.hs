{-# LANGUAGE OverloadedStrings #-}

module Routes where

import qualified Clay as C
import Control.Category
import Data.List
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Encoding
import Debug.Trace
import System.Directory
import System.FilePath
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A

import Css.Default (defaultStyle)
import Index (indexRoute)
import Base (baseTemplate)
import Projects
import RouteTree

cafeRoutes :: [CafeRoute]
cafeRoutes =
  [ indexRoute
  , projectRoute
  ]

imageRoute :: FilePath -> (FilePath, RouteTree)
imageRoute fname = (fname, File $ LB.readFile $ "img" </> fname)

cssPath :: [FilePath] -> FilePath
cssPath l = intercalate [pathSeparator] l

servableCss :: C.Css -> Servable
servableCss
  = C.render
  >>> LT.toStrict
  >>> encodeUtf8
  >>> LB.fromStrict
  >>> pure

cssRoute :: CssRoute -> RouteTree
cssRoute (CssRoute [x] style) = Dir $ M.singleton (x <> ".css") $ File (servableCss style)

cssHead :: CssRoute -> Html
cssHead (CssRoute path _) = H.link
  ! A.rel "stylesheet"
  ! A.type_ "text/css"
  ! A.href (stringValue (foldl' (</>) "" path <> ".css"))

renderRoute :: [T.Text] -> T.Text -> [CssRoute] -> Html -> RouteTree
renderRoute path title styles page = case path of
  (x:xs) -> Dir $ M.singleton (T.unpack x) (renderRoute xs title styles page)
  [] -> File $ baseTemplate (mapM_ cssHead styles) (H.text title) page

cafeRouteTree :: CafeRoute -> RouteTree
cafeRouteTree (CafeRoute path title styles page) =
  renderRoute (path <> ["index.xhtml"]) title styles page <>
  mconcat (cssRoute <$> styles)

routes :: IO RouteTree
routes = do
  images <- listDirectory "./img"
  pure $ (Dir $ M.fromList
    [ ( "css"
      , Dir $ M.singleton "default.css" $ File (servableCss defaultStyle)
      )
    , ( "img"
      , Dir $ M.fromList $ imageRoute <$> images
      )
    ]) <> mconcat [(cafeRouteTree indexRoute)]
