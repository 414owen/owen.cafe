{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LB
import Data.Semigroup ((<>))
import Data.String (IsString(..))
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.Mime
import Options.Applicative
import System.FilePath
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8
import Web.Scotty

import Index
import Routes
import RouteTree

type Route = (FilePath, MimeType, Servable)

data Args = Args { dev :: Bool }

args :: Parser Args
args = Args <$> switch ( long "dev" <> short 'd' <> help "Run the live reload server")

optParser :: ParserInfo Args
optParser = info (args <**> helper)
  (fullDesc <> progDesc "Static site generator")

serveServable :: Route -> ScottyM ()
serveServable s@(path, mime, getter) = get (fromString $ "/" <> path) $ do
  res <- liftIO $ getter
  setHeader "Content-Type" $ LT.fromStrict $ decodeUtf8 mime
  raw $ res

createRoutes :: [Route] -> ScottyM ()
createRoutes routes = mapM_ serveServable routes

serve :: [Route] -> IO ()
serve = scotty 8000 . createRoutes

flattenRoute :: RouteTree -> [Route]
flattenRoute (File n s) = let m = defaultMimeLookup (T.pack n) in
  [(n, m, s)] <> if n == "index.html" then [("", m, s)] else []
flattenRoute (Dir n s) = addPath n <$> flattenRoutes s

flattenRoutes :: [RouteTree] -> [Route]
flattenRoutes = concatMap flattenRoute

addPath :: String -> Route -> Route
addPath n (n', m, s) = (n </> n', m, s)

run :: [RouteTree] -> IO ()
run routes = do
  let flattenedRoutes = flattenRoutes routes
  args <- execParser optParser
  case dev args of
    True -> serve flattenedRoutes
    False -> putStrLn "not implemented"

main :: IO ()
main = routes >>= run
