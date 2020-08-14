{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Blog (mkBlogRoute, mkBlogs, blogCss) where

import Control.Arrow
import Control.Monad.Reader
import Control.Category ((>>>))
import qualified Data.ByteString.Lazy as LB
import Data.Char
import Data.FileEmbed
import Data.Functor
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as T
import System.Directory
import Skylighting.Styles
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A
import Text.Pandoc
import Text.Pandoc.Highlighting
import qualified Clay as C
import Debug.Trace

import Base
import RouteTree
import qualified Icons as I
import Css.Blog

data ListingType = Haskell

instance Show ListingType where
  show Haskell = "haskell"

getListingType :: FilePath -> ListingType
getListingType fp | ".hs" `isSuffixOf` fp = Haskell

postBasePath :: String -> String
postBasePath = ("blog/" <>)

postPath :: String -> String
postPath = postBasePath >>> (<> "/post.md")

listingPath :: String -> String -> String
listingPath post listing = postBasePath post <> "/listings/" <> listing

wrapListing :: String -> Text -> Text
wrapListing lang listing = mconcat
  [ "```"
  , T.pack lang
  , "\n"
  , listing
  , "```"
  ]

splitListing :: ListingType -> Text -> [Text]
splitListing Haskell = T.splitOn "\n-----\n"

dropLeadingLines :: T.Text -> T.Text
dropLeadingLines = T.lines >>> dropWhile (T.all isSpace) >>> T.unlines

processListing :: ListingType -> Text -> IO Text
processListing listingType listing = case splitListing listingType listing of
  [a] -> pure $ wrapListing (show listingType) a
  as | length as `elem` [2, 3] -> do
    processedListings <- traverse mdToBlaze $ fmap (wrapListing $ show listingType) (dropLeadingLines <$> as)
    pure $ T.unlines $ "<div class=\"multi-listing\">" : processedListings <> ["</div>"]

processMd :: String -> Text -> IO Text
processMd post (T.lines -> ls)  = forM ls processLine <&> T.unlines
  where
    processLine :: Text -> IO Text
    processLine (T.splitOn " " -> ["<!--", "listing", (T.unpack -> listingName), "-->"]) = do
      let listingType = getListingType listingName
      listing <- T.readFile $ listingPath post listingName
      processListing listingType listing
    processLine l = pure l

mdToBlaze :: Text -> IO Text
mdToBlaze txt = runIOorExplode $
  readMarkdown def{ readerExtensions = githubMarkdownExtensions } txt
  >>= writeHtml5String def{ writerReferenceLinks = True, writerHighlightStyle = Just highlightStyle }

removeExtension :: String -> String
removeExtension = takeWhile (/= '.')

splitDate :: String -> (String, String)
splitDate [] = ([], [])
splitDate (x:xs) | isDigit x || x == '-' = first (x:) $ splitDate xs
                 | otherwise = ([], x : xs)

formatTitle' :: String -> String
formatTitle' [] = []
formatTitle' ('-' : x : xs) = ' ' : toUpper x : formatTitle' xs
formatTitle' (x : xs) = x : formatTitle' xs

formatTitle :: String -> String
formatTitle s = tail $ formatTitle' $ '-' : removeExtension s

parsePath :: FilePath -> (String, String)
parsePath path = let (date, rest) = splitDate path
                 in  (take 8 date, formatTitle rest)

toBlogLink :: FilePath -> Html
toBlogLink path@(parsePath -> (date, title))
  = li $ a ! A.href ("/blog/" <> stringValue (removeExtension path)) $ toHtml title

blogPage :: [FilePath] -> Html
blogPage paths = ul $ mapM_ toBlogLink paths

isBlogEntry :: FilePath -> IO Bool
isBlogEntry path = doesPathExist $ postPath path

posts :: IO [FilePath]
posts = do
  paths <- getDirectoryContents "blog"
  filterM isBlogEntry paths

highlightLink :: Html
highlightLink = link ! type_ "text/css" ! rel "stylesheet" ! href "/css/highlight.css"

renderBlog :: FilePath -> Html -> Servable
renderBlog path nav = do
  contents <- T.readFile (postPath path) >>= processMd path
  blaze <- mdToBlaze contents
  let (date, title) = parsePath path
  baseTemplate
    highlightLink
    (toHtml $ toLower <$> title)
    nav
    (preEscapedToHtml blaze)

mkBlogEntry :: Html -> FilePath -> (FilePath, RouteTree)
mkBlogEntry nav path = (removeExtension path, Dir $ M.fromList [("index.xhtml", File $ renderBlog path nav)])

highlightStyle :: Style
Right highlightStyle = parseTheme $ LB.fromStrict $(embedFile "highlight.json")

blogCss :: LB.ByteString
blogCss = LB.fromStrict $ encodeUtf8 $ (T.pack $ styleToCss highlightStyle)
  <> (LT.toStrict $ C.render blogStyle)

mkBlogRoute :: IO CafeRoute
mkBlogRoute = do
  fs <- posts
  pure $ CafeRoute ["blog"] "blog" [] $ blogPage fs

mkBlogs :: Html -> IO [(FilePath, RouteTree)]
mkBlogs nav = do
  fs <- posts
  let blogDir = mkBlogEntry nav <$> fs
  pure blogDir