module Octopus.Primitive (
      resolveSymbol

    , match
    , ifz

    , eq, neq, lt, gt, lte, gte

    , add, sub, mul, div--, quo, rem, quorem
    
    , numer, denom, numParts
    
    --TODO float arithmetic (exp, log, ln, trig)

    --, read, write, flush, close

    --, newTag

    , len, cat, cut

    , get, keys, extend, delete

    --, new, deref, assign

    --, newArr, getIx, setIx

    --TODO os interface (perhaps in a separate file)
    ) where

import Prelude hiding (div, rem)
import Import hiding (delete)
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as BS

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
match var val = mkOb <$> go var val
    where
    go :: Val -> Val -> Maybe [(Symbol, Val)]
    go (Sy x) v = Just [(x, v)]
    go (Sq ps) (Sq xs) | Seq.length ps == Seq.length xs = do
        --FIXME disallow double-binding
        concat <$> mapM (uncurry go) (zip (toList ps) (toList xs))
    go (Sq ps) _ = Nothing
    go (Ob ps) _ | length (Map.keys ps) == 0 = Just []
    go pat val = error $ "unimplemented pattern-matching:\n" ++ show pat ++ "\n" ++ show val

ifz :: Val -> Val -> Val -> Val
ifz (Nm x) | x == 0 = const
           | otherwise = ignore
    where
    ignore x y = y


------ Relational ------
eq :: Val -> Val -> Maybe Val
eq (Nm a) (Nm b) = Just . mkInt $ if a == b then 1 else 0
eq _ _ = error "TODO eq"

neq :: Val -> Val -> Maybe Val
neq a b = Just $ case fromJust $ eq a b of
    (Nm x) | x == 0 -> mkInt 1
           | x == 1 -> mkInt 0

lt :: Val -> Val -> Maybe Val
lt (Nm a) (Nm b) = Just . mkInt $ if a < b then 1 else 0

lte :: Val -> Val -> Maybe Val
lte (Nm a) (Nm b) = Just . mkInt $ if a <= b then 1 else 0

gt :: Val -> Val -> Maybe Val
gt (Nm a) (Nm b) = Just . mkInt $ if a > b then 1 else 0

gte :: Val -> Val -> Maybe Val
gte (Nm a) (Nm b) = Just . mkInt $ if a >= b then 1 else 0


------ Arithmetic ------
add :: Val -> Val -> Maybe Val
add (Nm a) (Nm b) = Just . Nm $ a + b
add _ _ = Nothing

sub :: Val -> Val -> Maybe Val
sub (Nm a) (Nm b) = Just . Nm $ a - b
sub _ _ = Nothing

mul :: Val -> Val -> Maybe Val
mul (Nm a) (Nm b) = Just . Nm $ a * b
mul _ _ = Nothing

div :: Val -> Val -> Maybe Val
div (Nm a) (Nm b) = Just . Nm $ a / b
div _ _ = Nothing

--quo :: Val -> Val -> Maybe Val
--quo = error "TODO"

--rem :: Val -> Val -> Maybe Val
--rem = error "TODO"

--quorem :: Val -> Val -> Maybe Val
--quorem = error "TODO"


------ Rationals ------
numer :: Val -> Maybe Val
numer (Nm n) = Just . mkInt $ numerator n
numer _ = Nothing

denom :: Val -> Maybe Val
denom (Nm n) = Just . mkInt $ denominator n
denom _ = Nothing

trunc :: Val -> Maybe Val
trunc (Nm n) = Just . mkInt $ truncate n
trunc _ = Nothing

numParts :: Val -> Maybe Val
numParts (Nm n) = let (whole, frac) = properFraction n
                    in Just $ mkSq [mkInt whole, Nm frac]
numParts _ = Nothing


------ Floats ------


------ Sequence/Text/Bytes ------
len :: Val -> Maybe Val
len (Sq xs) = Just . mkInt $ Seq.length xs
len (Tx xs) = Just . mkInt $ T.length xs
len (By xs) = Just . mkInt $ BS.length xs
len _ = Nothing

cat :: Val -> Val -> Maybe Val
cat (Sq xs) (Sq ys) = Just . Sq $ xs <> ys
cat (Tx xs) (Tx ys) = Just . Tx $ xs <> ys
cat (By xs) (By ys) = Just . By $ xs <> ys
cat _ _ = Nothing

cut :: Val -> Val -> Maybe Val --FIXME I guess I really need to return (Either Val Val), where the left is an exception to raise
cut x (Nm q) = 
    case x of
        Sq xs -> do
            i <- n
            when (i >= Seq.length xs) Nothing
            let (as, bs) = Seq.splitAt i xs
            Just $ mkSq [Sq as, Sq bs]
        Tx xs -> do
            i <- n
            when (i >= T.length xs) Nothing
            let (as, bs) = T.splitAt i xs
            Just $ mkSq [Tx as, Tx bs]
        By xs -> do
            i <- n
            when (i >= BS.length xs) Nothing
            let (as, bs) = BS.splitAt i xs
            Just $ mkSq [By as, By bs]
    where
    n = if denominator q == 1 then Just (fromIntegral $ numerator q) else Nothing


------ Xons ------
{-| @get x f@ retrieves field @f@ from @x@, if the field exists. -}
get :: Val -> Val -> Maybe Val
get (Ob ob) (Sy sy) = Map.lookup sy ob
get _ _ = Nothing

{-| Get a list of the fields in a value. -}
keys :: Val -> Maybe Val
keys (Ob ob) = Just . mkSq $ Sy <$> Map.keys ob
keys _ = Just $ mkSq []

{-| @extend a b@ extends and overwrites bindings in @b@ with bindings in @a@. -}
extend :: Val -> Val -> Val
extend (Ob ob') (Ob ob) = Ob $ Map.union ob' ob
extend _ (Ob ob) = (Ob ob)
extend (Ob ob') _ = Ob ob'
extend _ _ = mkOb []

{-| @delete x f@ removes field @f@ from @x@, if the field exists.
    If it does not exist, then there is no change.
-}
delete :: Val -> Val -> Maybe Val
delete (Ob ob) (Sy sy) = Just . Ob $ Map.delete sy ob
delete x _ = Just x



