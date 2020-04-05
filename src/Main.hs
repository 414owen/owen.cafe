{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Category ((>>>))
import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LB
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Semigroup ((<>))
import Data.String (IsString(..))
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.Mime
import Options.Applicative
import System.FilePath
import System.Directory
import Web.Scotty

import Index
import Routes
import RouteTree

type Route = (FilePath, MimeType, Servable)

newtype Args = Args { dev :: Bool }

args :: Parser Args
args = Args <$> switch ( long "dev" <> short 'd' <> help "Run the live reload server")

optParser :: ParserInfo Args
optParser = info (args <**> helper)
  (fullDesc <> progDesc "Static site generator")

serveServable :: Route -> ScottyM ()
serveServable s@(path, mime, getter) = get (fromString $ "/" <> path) $ do
  res <- liftIO getter
  setHeader "Content-Type" $ LT.fromStrict $ decodeUtf8 mime
  raw res

createRoutes :: [Route] -> ScottyM ()
createRoutes = mapM_ serveServable

serve :: [Route] -> IO ()
serve = scotty 8000 . createRoutes

flattenRoute :: RouteTree -> [Route]
flattenRoute (Dir subs) =
  M.toList subs >>= \case
    (n, File s) ->
      let m = defaultMimeLookup (T.pack n) in
        [(n, m, s)] <> [("", m, s) | "index" `isPrefixOf` n]
    (n, flattenRoute -> els) -> els <&> \(n', m, s) -> (n </> n', m, s)

addPath :: String -> Route -> Route
addPath n (n', m, s) = (n </> n', m, s)

writeRoute :: FilePath -> Route -> IO ()
writeRoute p (f, _, c) = do
  contents <- c
  LB.writeFile (p </> f) contents

gen :: FilePath -> [Route] -> IO ()
gen prefix routes = mapM_ (writeRoute prefix) routes

dirs :: RouteTree -> [FilePath]
dirs (Dir (M.toList -> subs)) = subs >>= \case
  (p, File _) -> [p]
  (p, a) -> (p </>) <$> dirs a

createDirs :: FilePath -> RouteTree -> IO ()
createDirs prefix tree =
  let ds = (prefix </>) <$> nub (dirs tree) in
  mapM_ (createDirectoryIfMissing True) ds

run :: RouteTree -> IO ()
run routes = do
  let flattenedRoutes = flattenRoute routes
  dir <- getCurrentDirectory <&> takeDirectory
  args <- execParser optParser
  fn <- if dev args
    then serve flattenedRoutes
    else do
      createDirs dir routes
      gen dir $ flattenedRoutes <&> \(f, m, s) -> ("result" </> f, m, s)
  pure ()

main :: IO ()
main = routes >>= run
