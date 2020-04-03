module RouteTree where
  
import Clay (Css)
import Data.Map (Map)
import Data.Function
import Data.Functor
import qualified Data.Map.Merge.Lazy as M
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import Data.List
import System.FilePath
import Text.Blaze.XHtml5 as H

type Servable = IO LB.ByteString

data RouteTree
  = Dir (Map FilePath RouteTree)
  | File Servable

merge :: RouteTree -> RouteTree -> RouteTree
merge (Dir suba) (Dir subb) = Dir $
  M.merge M.preserveMissing
          M.preserveMissing
          (M.zipWithMaybeMatched $ \k v1 v2 -> Just $ merge v1 v2)
          suba subb
merge (Dir a) _ = error $ "merge failed: " <> unwords (M.keys a)
merge _ (Dir a) = error $ "merge failed: " <> unwords (M.keys a)

instance Semigroup RouteTree where
  (<>) = merge

instance Monoid RouteTree where
  mempty = Dir mempty

show' :: RouteTree -> [String]
show' (File _) = [""]
show' (Dir subs) = M.toList subs >>= \(k, s) -> (s & show') <&> (k </>)

instance Show RouteTree where
  show = intercalate "\n" . show'

data CssRoute = CssRoute [FilePath] Css

data CafeRoute = CafeRoute
  [T.Text]   -- path
  T.Text     -- title
  [CssRoute] -- extra css
  H.Html     -- page
