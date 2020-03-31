{-# LANGUAGE OverloadedStrings #-}

module Css.Index (indexStyle) where

import qualified Data.Text as T
import Clay as C

indexStyle :: Css
indexStyle = do
  "#techs" |> star ? do
    position relative
    transition color (sec 0.3)
    label ? do
      position absolute
      width (pct 100)
      height (pct 100)
      cursor pointer


{-
#langs > * {
  position: relative;
  transition: color 0.3s;
}

#langs label {
  position: absolute;
  width: 100%; height: 100%;
  cursor: pointer;
}

#lhaskell:checked    ~ #langs > .lhaskell,
#lphage:checked      ~ #langs > .lphage,
#lnix:checked        ~ #langs > .lnix,
#lsvg:checked        ~ #langs > .lsvg,
#lshell:checked      ~ #langs > .lshell,
#llambda:checked     ~ #langs > .llambda,
#ldata:checked       ~ #langs > .ldata,
#lconstraint:checked ~ #langs > .lconstraint {
  color: #f77;
}

#lhaskell:not(:checked)    ~ #langs > .lhaskell > [for="lhaskell"] + label,
#lphage:not(:checked)      ~ #langs > .lphage > [for="lphage"] + label,
#lnix:not(:checked)        ~ #langs > .lnix > [for="lnix"] + label,
#lsvg:not(:checked)        ~ #langs > .lsvg > [for="lsvg"] + label,
#lshell:not(:checked)      ~ #langs > .lshell > [for="lshell"] + label,
#llambda:not(:checked)     ~ #langs > .llambda > [for="llambda"] + label,
#ldata:not(:checked)       ~ #langs > .ldata > [for="ldata"] + label,
#lconstraint:not(:checked) ~ #langs > .lconstraint > [for="lconstraint"] + label,

#lhaskell:not(:checked)    ~ #ldesc > #shaskell,
#lphage:not(:checked)      ~ #ldesc > #sphage,
#lnix:not(:checked)        ~ #ldesc > #snix,
#lsvg:not(:checked)        ~ #ldesc > #ssvg,
#lshell:not(:checked)      ~ #ldesc > #sshell,
#llambda:not(:checked)     ~ #ldesc > #slambda,
#ldata:not(:checked)       ~ #ldesc > #sdata,
#lconstraint:not(:checked) ~ #ldesc > #sconstraint {
  display: none;
}

#langs {
  display: flex;
  flex-wrap: wrap;
  align-items: start;
}

#langs img {
  height: 50px;
  padding-bottom: 10px;
}

#langs > * {
  margin: 20px;
  min-width: 50px;
  display: flex;
  flex-direction: column;
  text-align: center;
}

#ldesc img {
  height: 200px;
}
-}
