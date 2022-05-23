{-# LANGUAGE OverloadedStrings #-}

module Projects (projectRoute) where

import qualified Data.Text as T
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A hiding (name)

import Base
import Css.List
import qualified Icons as I
import List
import RouteTree

github :: T.Text -> T.Text
github = ("https://github.com/414owen/" <>)

projects :: List
projects =
  [ ListItem
      "Phage"
      I.phageAnim
      ("A dynamic, functional, homoiconic, interpreted "
      <> "programming language.")
      (github "phage")
  , ListItem
      "simfin"
      (img ! src "/img/simfin.png")
      "SimFin API wrapper for Haskell"
      (github "simfin")
  , ListItem
      "Lambda Repl"
      I.lambdaRepl
      "An online lambda calculus REPL, which shows evaluation step-by-step."
      "https://lambda.how/"
  , ListItem
      "Select Dot Pink"
      I.pinkAnim
      "A game of CSS selectors."
      "https://select.pink/"
  , ListItem
      "Brownian Music"
      I.brownianMusic
      "A music recommendation interface and physics sandbox."
      "https://brownian.app"
  , ListItem
      "Brainfuck Interpreter"
      I.brainfuckAnim
      "A tiny, clean Brainfuck interpreter in Haskell."
      (github "brainfuck-hs")
  , ListItem
      "Turtle-SVG"
      I.turtle
      "An interpreter for a minimal turtle-graphics instruction set that outputs scalable vector graphics."
      (github "turtle-svg")
  , ListItem
      "Hexplode"
      I.hexplodeAnim
      "A clone of J. Ansell's 1982 BBC Micro game."
      "https://hexplo.de"
  ]

projectRoute :: CafeRoute
projectRoute = CafeRoute ["projects"] "projects" [listStyleRoute] (renderListPage projects)
