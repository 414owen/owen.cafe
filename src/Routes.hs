{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Debug.Trace
import qualified Clay as C
import Control.Category ((>>>))
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
import Text.Blaze.XHtml5.Attributes as A hiding (id)

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

mkLink :: CafeRoute -> [T.Text] -> Html
mkLink (CafeRoute path name _ _) current =
  let p = "/" <> T.intercalate "/" path in
    H.a ! A.href (textValue p) ! mempty $ text name

mkNavLinks :: [CafeRoute] -> [T.Text] -> Html
mkNavLinks [] _ = mempty
mkNavLinks (c@(CafeRoute path _ _ _) : rest) current =
  let a = mkLink c current
  in  (if current == path then a ! class_ "current" else a) >> mkNavLinks rest current

mkNav :: [T.Text] -> Html
mkNav = (H.nav ! A.class_ "links") . mkNavLinks cafeRoutes

imageRoute :: FilePath -> (FilePath, RouteTree)
imageRoute fname = (fname, File $ LB.readFile $ "img" </> fname)

cssPath :: [FilePath] -> FilePath
cssPath l = intercalate "/" l

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

renderRoute :: CafeRoute -> [T.Text] -> RouteTree
renderRoute c@(CafeRoute path title styles page) pathSegs = case pathSegs of
  (x:xs) -> Dir $ M.singleton (T.unpack x) (renderRoute c xs)
  [] -> File $ baseTemplate (mapM_ cssHead styles) (H.text title) (mkNav path) page

cafeRouteTree :: CafeRoute -> RouteTree
cafeRouteTree r@(CafeRoute path title styles page) =
  renderRoute (CafeRoute path title styles page) (path <> ["index.xhtml"]) <>
  mconcat (cssRoute <$> styles)

routes :: IO RouteTree
routes = do
  images <- listDirectory "./img"
  pure $ traceShowId (Dir $ M.fromList
    [ ( "css"
      , Dir $ M.singleton "default.css" $ File (servableCss defaultStyle)
      )
    , ( "img"
      , Dir $ M.fromList $ imageRoute <$> images
      )
    ]) <> mconcat (cafeRouteTree <$> cafeRoutes)
