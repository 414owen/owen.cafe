{-# LANGUAGE OverloadedStrings #-}

module ClickSwitch where

import Control.Category
import qualified Clay as C
import Data.List.NonEmpty
import Data.Function
import Data.Semigroup
import qualified Data.Text as T
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A hiding (name)
import qualified Text.Blaze.XHtml5.Attributes as A

data ClickSwitch = ClickSwitch T.Text C.Css (NonEmpty ClickSwitchOption)

data ClickSwitchOption
  = ClickSwitchOption
  { name :: T.Text
  , switch :: Html
  , elem :: Html
  }

prefixOpt :: T.Text -> ClickSwitchOption -> ClickSwitchOption
prefixOpt prefix c = c{ name = prefix <> name c }

radioInput :: Html
radioInput = H.input ! type_ "radio" ! A.class_ "hidden"

-- switches, elems, css
renderClickSwitch :: ClickSwitch -> (Html, Html, C.Css)
renderClickSwitch (ClickSwitch prefix extraCss options)
  = let p = prefix <> "-"
        ps = prefix <> "s"
        psVal = textValue ps
        prefixedOpts = prefixOpt p <$> options
        noneAttr = textValue $ p <> "none"
        noneLabel = H.label ! for noneAttr ! checked "checked" $ mempty
    in  ( renderSwitch noneLabel <$> prefixedOpts & sequence_
          & H.div ! A.id psVal
        , do
            radioInput ! A.id noneAttr ! A.name psVal
            renderElem psVal <$> prefixedOpts & sequence_
        , renderCss extraCss ps $ name <$> prefixedOpts
        )

renderCss :: C.Css -> T.Text -> NonEmpty T.Text -> C.Css
renderCss extraCss plural names
  = sequence_
    [ sconcat (extraCssSelector plural <$> names) C.? extraCss
    , sconcat ((hideLabel plural <$> names) <> (hideElem <$> names)) C.? C.display C.none
    ]

extraCssSelector :: T.Text -> T.Text -> C.Selector
extraCssSelector plural name
  = C.star C.# C.byId name C.# ":checked"
  C.|~ C.star C.# C.byId plural
  C.|> C.star C.# C.byClass name

hideElem :: T.Text -> C.Selector
hideElem name = C.star C.# C.byId name C.# C.not ":checked"
  C.|+ C.star

hideLabel :: T.Text -> T.Text -> C.Selector
hideLabel plural name = C.star C.# C.byId name C.# ":checked"
  C.|~ C.star C.# C.byId plural
  C.|> C.star C.# C.byClass name
  C.|> C.star C.# ("for" C.@= name)

renderElem :: AttributeValue -> ClickSwitchOption -> Html
renderElem switchName (ClickSwitchOption name _ elem) = do
  radioInput ! A.id (textValue name) ! A.name switchName
  elem

renderSwitch :: Html -> ClickSwitchOption -> Html
renderSwitch noneLabel (ClickSwitchOption name switch _)
  = H.div ! class_ (textValue $ name <> " clickswitch") $ do
      switch
      noneLabel
      H.label ! for (textValue name) $ mempty
