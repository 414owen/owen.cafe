{-# LANGUAGE OverloadedStrings #-}

module Advent where

import Control.Arrow
import Data.Functor
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html.Renderer.Text as R
import Data.Text.Encoding as E
import Data.ByteString.Lazy as LB
import Base (baseTemplate)

import RouteTree
import qualified Icons as I

adventPage :: Int -> Html
adventPage year = do
  H.h1 $ H.text $ "Advent of Code " <> T.pack (show year) <> " Benchmarks"
  H.div ! A.class_ "no-print top-overview" $ do
    H.span $ do
      "sort by: "
      H.select ! A.id "sort-overview" ! A.class_ "select" $ do
        H.option ! A.value "report-index" $ "index"
        H.option ! A.value "lex" $ "lexical"
        H.option ! A.value "colex" $ "colexical"
        H.option ! A.value "duration" $ "time ascending"
        H.option ! A.value "rev-duration" $ "time descending"
    H.div ! A.class_ "overview-info" $ do
      H.a ! A.id "legend-toggle" ! A.class_ "chevron button" $ mempty
  H.section $ do
    H.div ! A.id "overview-chart" $ mempty
    H.div ! A.id "reports" $ mempty
    H.script ! A.type_ "application/javascript" ! A.src "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.4/Chart.min.js" $ mempty
    H.script ! A.type_ "application/javascript" ! A.src "criterion.js" $ mempty

extraHead :: Html
extraHead = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/css/criterion.css"

adventYears :: Html -> RouteTree
adventYears nav = Dir $ M.fromList $ years <&> \year ->
  ( show year
  , Dir $ M.fromList
     [ ("index.xhtml"
       , File $ baseTemplate extraHead (H.text $ T.pack $ "Advent of Code" <> show year) nav (adventPage year)
       )
     ]
  )
  -- (show &&& File . pure . LB.fromStrict . E.encodeUtf8 . LT.toStrict . R.renderHtml . adventPage) <$> years

years :: [Int]
years = [ 2021 ]

root :: Html
root = H.ul $ do
  flip traverse years (\year -> H.li $ H.a ! A.href (preEscapedStringValue ("/advent/" <> show year))
                          $ H.text $ T.pack (show year))
  pure ()

adventRoute :: CafeRoute
adventRoute = CafeRoute ["advent"] "advent of code" [] root
