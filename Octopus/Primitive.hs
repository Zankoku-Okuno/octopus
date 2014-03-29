module Octopus.Primitive where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Foldable hiding (concat)
import Data.Traversable hiding (mapM)

import Octopus.Data
import Octopus.Basis


resolveSymbol :: Symbol -> Val -> Maybe Val
resolveSymbol sy (Ob ob) = Map.lookup sy ob
resolveSymbol _ _ = Nothing

{-| Create a new environment by matching the second value
    to atoms in the first, even if those atoms are nested
    under sequences or objects. Symbols match any value and
    perform binding.
-}
match :: Val -> Val -> Maybe Val
match var val = mkObj <$> go var val
    where
    go :: Val -> Val -> Maybe [(Symbol, Val)]
    go (Sy x) v = Just [(x, v)]
    go (Sq ps) (Sq xs) | Seq.length ps == Seq.length xs = do
        --FIXME disallow double-binding
        concat <$> mapM (uncurry go) (zip (toList ps) (toList xs))
    go (Sq ps) _ = Nothing

{-| @extend a b@ extends and overwrites bindings in @b@ with bindings in @a@. -}
extend :: Val -> Val -> Val
extend (Ob env') (Ob env) = Ob $ Map.union env' env
extend _ (Ob env) = (Ob env)
extend (Ob env') _ = Ob env'
extend _ _ = mkObj []




