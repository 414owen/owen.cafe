{-# LANGUAGE OverloadedStrings #-}

module List
  ( List
  , ListItem(..)
  , renderListPage
  ) where

import qualified Data.Text as T
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A hiding (name)

import Base
import Css.List
import RouteTree

data ListItem = ListItem
  { name :: T.Text
  , iconName :: T.Text
  , description :: T.Text
  , url :: T.Text
  }

type List = [ListItem]

sep :: Html
sep = H.div ! A.class_ "hsep" $ ""

renderListItem :: Bool -> ListItem -> Html
renderListItem isLast (ListItem name imgsrc desc url) = H.li $ do
  let link = H.a ! href (textValue url)
  sep
  H.div ! A.class_ "media" $ do
    link $ img ! src (textValue $ "/img/" <> imgsrc <> ".svg")
               ! alt (textValue $ name <> " icon")
    H.div $ do
      link $ H.div $ text name
      H.div $ text desc
  if isLast then sep else mempty

renderList :: List -> Html
renderList [] = mempty
renderList [x] = renderListItem True x
renderList (x:xs) = renderListItem False x >> renderList xs

renderListPage :: List -> Html
renderListPage l = H.ul ! A.class_ "list" $ renderList l
