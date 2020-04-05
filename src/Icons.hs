{-# LANGUAGE OverloadedStrings #-}

module Icons where
  
import qualified Data.Text as T
import Text.Blaze.XHtml5 as H
import Text.Blaze.XHtml5.Attributes as A hiding (name)

altVal :: T.Text -> AttributeValue
altVal name = textValue $ name <> " icon"

srcVal :: T.Text -> AttributeValue
srcVal imgsrc = textValue $ "/img/" <> imgsrc <> "-d.svg"

imgIcon :: T.Text -> T.Text -> Html
imgIcon name imgsrc =
  H.img ! src (srcVal imgsrc)
        ! alt (altVal name)

objIcon :: T.Text -> T.Text -> Html
objIcon name imgsrc =
  object ! type_ "image/svg+xml"
         ! data_ (srcVal imgsrc)
         ! alt (altVal name)
         $ mempty

animTup :: T.Text -> T.Text -> (Html, Html)
animTup name iconname = (imgIcon name iconname, imgIcon name (iconname <> "-anim"))

phage, phageAnim :: Html
(phage, phageAnim) = animTup "phage" "phage"

lambdaRepl :: Html
lambdaRepl = imgIcon "lambda repl" "lambda"

brownianMusic :: Html
brownianMusic = objIcon "brownian music" "brownian"

brainfuckAnim :: Html
brainfuckAnim = imgIcon "brainfuck interpreter" "brainfuck-anim"

haskell :: Html
haskell = imgIcon "haskell" "haskell"

turtle :: Html
turtle = imgIcon "turtle-svg" "turtle"

hexplodeAnim :: Html
hexplodeAnim = imgIcon "hexplode" "hexplode-anim"

pinkAnim :: Html
pinkAnim = imgIcon "select.pink" "pink-anim"
