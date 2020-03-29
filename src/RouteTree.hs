module RouteTree where

import qualified Data.ByteString.Lazy as LB
import Data.List
import Text.Blaze.Html

type Servable = IO LB.ByteString

data RouteTree = Dir FilePath [RouteTree] | File FilePath Servable

instance Show RouteTree where
  show (Dir path subs)
    = intercalate "\n" $ ((path <> "/") <>) . show <$> subs
  show (File path _) = path
