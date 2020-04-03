{-# LANGUAGE OverloadedStrings #-}

module Projects (projectRoute) where

import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A

import Base
import RouteTree

projects :: Servable
projects = baseTemplate "☕ owen.cafe" mempty mempty

projectRoute :: CafeRoute
projectRoute = CafeRoute ["projects"] mempty mempty mempty
