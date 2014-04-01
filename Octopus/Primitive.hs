module Octopus.Primitive where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Octopus.Data
import Octopus.Shortcut
import Octopus.Basis

{-| The point of closures is to protect from premature evaluation. Since closures 
    are just an object responding to a protocol, we need to protect some fields
    from evaluation. This function takes an object's internals and splits the
    mapping inot the protected and evaulated parst, respectively.
-}
splitFields :: Map Symbol Val -> ([(Symbol, Val)], [(Symbol, Val)])
splitFields = go ([], []) . Map.toList
    where
    go (protect, eval) [] = (protect, eval)
    go (protect, eval) (x:xs) | fst x `elem` protectedFields = go (x:protect, eval) xs
                              | otherwise = go (protect, x:eval) xs
    protectedFields = [closureVar, closureBody, closureEnv]

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
    go (Ob ps) _ | length (Map.keys ps) == 0 = Just []
    go pat val = error $ "unimplemented pattern-matching:\n" ++ show pat ++ "\n" ++ show val

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



