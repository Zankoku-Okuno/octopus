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
match :: Val -> Val -> Fallible Val
match var val = mkOb <$> go var val
    where
    go :: Val -> Val -> Fallible [(Symbol, Val)]
    go (Sy x) v = Right [(x, v)]
    go (Sq ps) (Sq xs) | Seq.length ps == Seq.length xs = do
        --FIXME disallow double-binding
        concat <$> mapM (uncurry go) (zip (toList ps) (toList xs))
    go (Sq ps) _ = Left (error "TODO")
    go (Ob ps) _ | length (Map.keys ps) == 0 = Right []
    go pat val = error $ "unimplemented pattern-matching:\n" ++ show pat ++ "\n" ++ show val

ifz :: Val -> Val -> Val -> Val
ifz (Nm x) | x == 0 = const
           | otherwise = ignore
ifz _ = ignore
ignore x y = y


------ Relational ------
eq :: Val -> Val -> Fallible Val
eq (Nm a) (Nm b) = Right . mkInt $ if a == b then 1 else 0
eq _ _ = error "TODO eq"

neq :: Val -> Val -> Fallible Val
neq (Nm a) (Nm b) = Right . mkInt $ if a == b then 1 else 0
neq _ _ = error "TODO neq"

lt :: Val -> Val -> Fallible Val
lt (Nm a) (Nm b) = Right . mkInt $ if a < b then 1 else 0

lte :: Val -> Val -> Fallible Val
lte (Nm a) (Nm b) = Right . mkInt $ if a <= b then 1 else 0

gt :: Val -> Val -> Fallible Val
gt (Nm a) (Nm b) = Right . mkInt $ if a > b then 1 else 0

gte :: Val -> Val -> Fallible Val
gte (Nm a) (Nm b) = Right . mkInt $ if a >= b then 1 else 0


------ Arithmetic ------
add :: Val -> Val -> Fallible Val
add (Nm a) (Nm b) = Right . Nm $ a + b
add _ _ = Left (error "TODO")

sub :: Val -> Val -> Fallible Val
sub (Nm a) (Nm b) = Right . Nm $ a - b
sub _ _ = Left (error "TODO")

mul :: Val -> Val -> Fallible Val
mul (Nm a) (Nm b) = Right . Nm $ a * b
mul _ _ = Left (error "TODO")

div :: Val -> Val -> Fallible Val
div (Nm a) (Nm b) = Right . Nm $ a / b
div _ _ = Left (error "TODO")

--quo :: Val -> Val -> Maybe Val
--quo = error "TODO"

--rem :: Val -> Val -> Maybe Val
--rem = error "TODO"

--quorem :: Val -> Val -> Maybe Val
--quorem = error "TODO"


------ Rationals ------
numer :: Val -> Fallible Val
numer (Nm n) = Right . mkInt $ numerator n
numer _ = Left (error "TODO")

denom :: Val -> Fallible Val
denom (Nm n) = Right . mkInt $ denominator n
denom _ = Left (error "TODO")

trunc :: Val -> Fallible Val
trunc (Nm n) = Right . mkInt $ truncate n
trunc _ = Left (error "TODO")

numParts :: Val -> Fallible Val
numParts (Nm n) = let (whole, frac) = properFraction n
                    in Right $ mkSq [mkInt whole, Nm frac]
numParts _ = Left (error "TODO")


------ Floats ------


------ Sequence/Text/Bytes ------
len :: Val -> Fallible Val
len (Sq xs) = Right . mkInt $ Seq.length xs
len (Tx xs) = Right . mkInt $ T.length xs
len (By xs) = Right . mkInt $ BS.length xs
len _ = Left (error "TODO")

cat :: Val -> Val -> Fallible Val
cat (Sq xs) (Sq ys) = Right . Sq $ xs <> ys
cat (Tx xs) (Tx ys) = Right . Tx $ xs <> ys
cat (By xs) (By ys) = Right . By $ xs <> ys
cat _ _ = Left (error "TODO")

cut :: Val -> Val -> Fallible Val --FIXME I guess I really need to return (Either Val Val), where the left is an exception to raise
cut x (Nm q) = 
    case x of
        Sq xs -> do
            i <- n
            when (i >= Seq.length xs) $ Left (error "TODO")
            let (as, bs) = Seq.splitAt i xs
            Right $ mkSq [Sq as, Sq bs]
        Tx xs -> do
            i <- n
            when (i >= T.length xs) $ Left (error "TODO")
            let (as, bs) = T.splitAt i xs
            Right $ mkSq [Tx as, Tx bs]
        By xs -> do
            i <- n
            when (i >= BS.length xs) $ Left (error "TODO")
            let (as, bs) = BS.splitAt i xs
            Right $ mkSq [By as, By bs]
    where
    n = if denominator q == 1 then Right (fromIntegral $ numerator q) else Left (error "TODO")


------ Xons ------
{-| @get x f@ retrieves field @f@ from @x@, if the field exists. -}
get :: Val -> Val -> Fallible Val
get (Ob ob) (Sy sy) = maybe (Left (error "TODO")) Right $ Map.lookup sy ob
get _ _ = Left (error "TODO")

{-| Get a list of the fields in a value. -}
keys :: Val -> Fallible Val
keys (Ob ob) = Right . mkSq $ Sy <$> Map.keys ob
keys _ = Right $ mkSq []

{-| @extend a b@ extends and overwrites bindings in @b@ with bindings in @a@. -}
extend :: Val -> Val -> Val
extend (Ob ob') (Ob ob) = Ob $ Map.union ob' ob
extend _ (Ob ob) = (Ob ob)
extend (Ob ob') _ = Ob ob'
extend _ _ = mkOb []

{-| @delete x f@ removes field @f@ from @x@, if the field exists.
    If it does not exist, then there is no change.
-}
delete :: Val -> Val -> Fallible Val
delete (Ob ob) (Sy sy) = Right . Ob $ Map.delete sy ob
delete x _ = Right x



