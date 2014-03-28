module Octopus.Primitive where


--import Data.List
--import Data.Ratio
import Data.Symbol
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
--import Data.IORef
--import Data.Array.IO
--import System.IO

import Data.Foldable
import Data.Traversable hiding (mapM)
import Control.Applicative
import Control.Monad


import Octopus.Data
import Octopus.Basis

resolveSymbol :: Symbol -> Val -> Maybe Val
resolveSymbol sy (Ob ob) = Map.lookup sy ob
resolveSymbol _ _ = Nothing

extendEnvironment :: Val -> Val -> Val -> Maybe Val
extendEnvironment x v env = case env of
    Ob env -> Ob . flip Map.union env <$> go x v
    _ -> Ob <$> go x v
    where
    go :: Val -> Val -> Maybe (Map Symbol Val)
    go (Sy x) v = pure $ Map.fromList [(x, v)]
    go (Sq ps) (Sq xs) | Seq.length ps == Seq.length xs = do
        --FIXME disallow double-binding
        Map.unions <$> mapM (uncurry go) (zip (toList ps) (toList xs))
    go (Sq ps) _ = Nothing
