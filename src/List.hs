{-# LANGUAGE OverloadedStrings #-}

module List
  ( List
  , ListItem(..)
  , renderListPage
  , Icon(..)
  ) where

import Data.Maybe
import qualified Data.Text as T
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A hiding (name)

import Base
import Css.List
import RouteTree

data Icon = Img T.Text | Obj T.Text

data ListItem = ListItem
  { name :: T.Text
  , iconName :: Html
  , description :: T.Text
  , url :: T.Text
  }

type List = [ListItem]

sep :: Html
sep = H.div ! A.class_ "hsep" $ ""

srcMod :: T.Text -> AttributeValue
srcMod src = textValue $ "/img/" <> src <> ".svg"

renderListItem :: Bool -> ListItem -> Html
renderListItem isLast (ListItem name imgsrc desc url) = H.li $ do
  let link = H.a ! href (textValue url)
  let Just stripped = T.stripPrefix "https://" url
  sep
  H.div ! A.class_ "media" $ do
    link $ imgsrc
    H.div $ do
      link $ do
        if "github" `T.isPrefixOf` stripped
          then mempty
          else H.span ! class_ "url"
            $ text $ fromMaybe stripped (T.stripSuffix "/" stripped)
        H.div $ text name
      H.div $ text desc
  if isLast then sep else mempty

renderList :: List -> Html
renderList [] = mempty
renderList [x] = renderListItem True x
renderList (x:xs) = renderListItem False x >> renderList xs

renderListPage :: List -> Html
renderListPage l = H.ul ! A.class_ "list" $ renderList l
