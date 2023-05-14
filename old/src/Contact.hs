{-# LANGUAGE OverloadedStrings #-}

module Contact (contactRoute) where

import qualified Data.Text as T
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A

import RouteTree
import qualified Icons as I

contactPage :: Html
contactPage = do
  H.div ! A.style "max-width: 300px" $ do
    H.div $ do
      "developers can find my email on "
      H.a ! href "https://github.com/414owen/" ! target "_blank"
        $ "GitHub"
    br
    H.div $ do
      "recruiters can find me on "
      H.a ! href "https://www.linkedin.com/in/owen-shepherd-50418b110" ! target "_blank"
        $ "LinkedIn"

contactRoute :: CafeRoute
contactRoute = CafeRoute ["contact"] "contact" [] contactPage
