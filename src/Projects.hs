{-# LANGUAGE OverloadedStrings #-}

module Projects (projectRoute) where

import qualified Data.Text as T
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A hiding (name)

import Base
import Css.List
import List
import RouteTree

github :: T.Text -> T.Text
github = ("https://github.com/414owen/" <>)

projects :: List
projects =
  [ ListItem
      "Phage"
      "phage-anim-d"
      ("An extremely dynamic, functional, homoiconic, interpreted "
      <> "programming language I made to save keystrokes in competitions")
      (github "phage")
  , ListItem
      "Lambda Repl"
      "lambda-anim-d"
      "TODO"
      "https://owen.cafe/try-lambda/"
  ]
projectRoute :: CafeRoute
projectRoute = CafeRoute ["projects"] "projects" [listStyleRoute] (renderListPage projects)
