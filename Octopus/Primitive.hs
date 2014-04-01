module Octopus.Primitive (
      resolveSymbol

    , match

    --, eq, neq

    --, add, sub, mul, div --TODO more arithmetic
    --, numer, denom, round, ceil, floor
    --, lt, gt, lte, gte

    --, read, write, flush, close

    --, newTag

    --, len, cat, cut

    , get, keys, extend, delete

    --, new, deref, assign

    --, newArr, getIx, setIx

    --TODO os interface (perhaps in a separate file)
    ) where

import Import hiding (delete)
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Octopus.Data
import Octopus.Shortcut
import Octopus.Basis

------ Internals ------
{-| Lookup symbol in a value, if the binding exists. -}
resolveSymbol :: Symbol -> Val -> Maybe Val
resolveSymbol sy (Ob ob) = Map.lookup sy ob
resolveSymbol _ _ = Nothing


------ Non-data Primitives ------
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


------ Xons ------
{-| @get x f@ retrieves field @f@ from @x@, if the field exists. -}
get :: Val -> Val -> Maybe Val
get (Ob ob) (Sy sy) = Map.lookup sy ob
get _ _ = Nothing

{-| Get a list of the fields in a value. -}
keys :: Val -> Val
keys (Ob ob) = mkSeq $ Sy <$> Map.keys ob
keys _ = mkSeq []

{-| @extend a b@ extends and overwrites bindings in @b@ with bindings in @a@. -}
extend :: Val -> Val -> Val
extend (Ob ob') (Ob ob) = Ob $ Map.union ob' ob
extend _ (Ob ob) = (Ob ob)
extend (Ob ob') _ = Ob ob'
extend _ _ = mkObj []

{-| @delete x f@ removes field @f@ from @x@, if the field exists.
    If it does not exist, then there is no change.
-}
delete :: Val -> Val -> Val
delete (Ob ob) (Sy sy) = Ob $ Map.delete sy ob
delete x _ = x



