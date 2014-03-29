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
extend (Ob ob') (Ob ob) = Ob $ Map.union ob' ob
extend _ (Ob ob) = (Ob ob)
extend (Ob ob') _ = Ob ob'
extend _ _ = mkObj []

delete :: Val -> Val -> Val
delete (Ob ob) (Sy sy) = Ob $ Map.delete sy ob
delete x _ = x

keys :: Val -> Val
keys (Ob ob) = mkSeq $ Sy <$> Map.keys ob
keys _ = mkSeq []

get :: Val -> Val -> Maybe Val
get (Ob ob) (Sy sy) = Map.lookup sy ob
get _ _ = Nothing



