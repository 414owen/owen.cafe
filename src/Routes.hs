{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Debug.Trace
import qualified Clay as C
import Control.Category ((>>>))
import Control.Monad.Reader
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
import Contact (contactRoute)
import Graphics (graphicsRoute)
import Blog (mkBlogRoute, mkBlogs, blogCss, blogJs)
import Projects
import RouteTree
import Util

type RouteM = Reader [CafeRoute]

mkCafeRoutes :: CafeRoute -> [CafeRoute]
mkCafeRoutes blogRoute =
  [ indexRoute
  , blogRoute
  , projectRoute
  , graphicsRoute
  , ExternalRoute "github" "https://github.com/414owen/"
  , contactRoute
  ]

toCurrent :: T.Text -> T.Text
toCurrent t = "&#8594;&#160;&#160;" <> t

mkLink :: CafeRoute -> [T.Text] -> Html
mkLink (ExternalRoute name url) _ =
  l ! href (textValue url) $ text name
mkLink (CafeRoute path name _ _) current =
  let p = "/" <> T.intercalate "/" path in
    H.a ! href (textValue p) $ preEscapedText $
      if current == path then toCurrent name else name

mkNavLinks :: [CafeRoute] -> [T.Text] -> Html
mkNavLinks [] _ = mempty
mkNavLinks (c@(CafeRoute path _ _ _) : rest) current =
  let a = mkLink c current
  in  (if current == path then a ! class_ "current" else a) >> mkNavLinks rest current
mkNavLinks (c : rest) current = mkLink c current >> mkNavLinks rest current

mkNav :: [T.Text] -> RouteM Html
mkNav current = do
  cafeRoutes <- ask
  pure $ H.nav ! A.class_ "links" $ mkNavLinks cafeRoutes current

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
  ! A.href (stringValue (foldl' (</>) "/" path <> ".css"))

renderRoute :: CafeRoute -> [T.Text] -> RouteM RouteTree
renderRoute c@(CafeRoute path title styles page) pathSegs = do
  nav <- mkNav path
  case pathSegs of
    (x:xs) -> do
      sub <- renderRoute c xs
      pure $ Dir $ M.singleton (T.unpack x) sub
    [] -> pure $ File $ baseTemplate (mapM_ cssHead styles) (H.text title) nav page

cafeRouteTree :: CafeRoute -> RouteM RouteTree
cafeRouteTree r@(CafeRoute path title styles page) = do
  current <- renderRoute (CafeRoute path title styles page) (path <> ["index.xhtml"])
  pure $ current <> mconcat (cssRoute <$> styles)
cafeRouteTree _ = pure $ Dir mempty

routes :: IO RouteTree
routes = do
  blogRoute <- mkBlogRoute
  let cafeRoutes = mkCafeRoutes blogRoute
  blogs <- mkBlogs (runReader (mkNav ["blog"]) cafeRoutes)
  images <- listDirectory "./img"
  let routeTree = runReader (sequence $ cafeRouteTree <$> cafeRoutes) cafeRoutes
  pure $ traceShowId (Dir $ M.fromList
    [ ( "css"
      , Dir $ M.fromList [ ("default.css", File $ servableCss defaultStyle)
                         , ("blog.css", File $ pure $ blogCss) ]
      )
    , ( "img"
      , Dir $ M.fromList $ imageRoute <$> images
      )
    , ( "blog"
      , Dir $ M.fromList blogs
      )
    , ( "js"
      , Dir $ M.fromList [ ("blog.js", File $ blogJs)
                         ]
      )
    ]) <> mconcat routeTree
