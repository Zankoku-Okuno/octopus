{-# LANGUAGE OverloadedStrings #-}
module Language.Octopus.Primitive (
      resolveSymbol

    , match
    , ifz

    , eq, neq, lt, gt, lte, gte

    , add, sub, mul, div--, quo, rem, quorem
    
    , numer, denom, numParts
    
    --TODO float arithmetic (exp, log, ln, trig)

    , openFp, readFp, writeFp, flushFp, closeFp

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
import System.IO.Error

import Language.Octopus.Data
import Language.Octopus.Data.Shortcut
import Language.Octopus.Basis


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
--FIXME disallow double-binding
match var val = mkOb <$> go var val
    where
    go :: Val -> Val -> Fallible [(Symbol, Val)]
    go (Sy x) v = Right [(x, v)]
    go (Sq ps) (Sq xs) | Seq.length ps == Seq.length xs =
                            concat <$> mapM (uncurry go) (zip (toList ps) (toList xs))
                       | otherwise = Left $ mkMatchFail (Sq ps) (Sq xs)
    go (Sq ps) v = Left $ mkMatchFail (Sq ps) v
    go (Ob ps) _ | length (Map.keys ps) == 0 = Right []
    go (Ob ps) (Ob vs) = goObj (Map.toList ps) vs
        where
        goObj :: [(Symbol, Val)] -> Map Symbol Val -> Fallible [(Symbol, Val)]
        goObj [] _ = Right []
        goObj ((k, v):ks) vs = case Map.lookup k vs of
            Just v' -> (++) <$> go v v' <*> goObj ks vs
            Nothing -> Left $ mkMatchFail (Ob ps) (Ob vs)
    go (Ob ps) v = Left $ mkMatchFail (Ob ps) v
    go pat val = error $ "unimplemented pattern-matching:\n" ++ show pat ++ "\n" ++ show val

ifz :: Val -> Val -> Val -> Val
ifz (Nm x) | x == 0 = const
           | otherwise = ignore
--TODO float test vs. zero
ifz _ = ignore
ignore x y = y


------ Relational ------
eq :: Val -> Val -> Fallible Val
eq a b = Right . mkInt $ if a == b then 1 else 0

neq :: Val -> Val -> Fallible Val
neq a b = Right . mkInt $ if a /= b then 1 else 0

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
add a b = Left $ mkTypeError (Pr Add) "(Nm, Nm)" (mkSq [a, b])

sub :: Val -> Val -> Fallible Val
sub (Nm a) (Nm b) = Right . Nm $ a - b
sub a b = Left $ mkTypeError (Pr Sub) "(Nm, Nm)" (mkSq [a, b])

mul :: Val -> Val -> Fallible Val
mul (Nm a) (Nm b) = Right . Nm $ a * b
mul a b = Left $ mkTypeError (Pr Mul) "(Nm, Nm)" (mkSq [a, b])

div :: Val -> Val -> Fallible Val
div (Nm a) (Nm b) | b == 0 = Left (getTag exnDivZero, exnDivZero)
                  | otherwise = Right . Nm $ a / b
div a b = Left $ mkTypeError (Pr Div) "(Nm, Nm)" (mkSq [a, b])

--quo :: Val -> Val -> Maybe Val
--quo = error "TODO"

--rem :: Val -> Val -> Maybe Val
--rem = error "TODO"

--quorem :: Val -> Val -> Maybe Val
--quorem = error "TODO"


------ Rationals ------
numer :: Val -> Fallible Val
numer (Nm n) = Right . mkInt $ numerator n
numer x = Left $ mkTypeError (Pr Numer) "Nm" x

denom :: Val -> Fallible Val
denom (Nm n) = Right . mkInt $ denominator n
denom x = Left $ mkTypeError (Pr Denom) "Nm" x

numParts :: Val -> Fallible Val
numParts (Nm n) = let (whole, frac) = properFraction n
                    in Right $ mkSq [mkInt whole, Nm frac]
--TODO get (whole, mantissa)
numParts x = Left $ mkTypeError (Pr NumParts) "Nm | Fl" x


------ Floats ------


------ Handles ------
openFp :: Val -> IO (Fallible Val)
openFp (Sq args) = case toList args of
    [Tx path, Sy m] -> do
        fp_m <- tryIOError $ openBinaryFile (unpack path) (mode m)
        case fp_m of
            Right fp -> return . Right $ Fp fp
            Left err -> return . Left $ (getTag exnIOError, mkSq [exnIOError, mkTx $ show err])
    _ -> return . Left $ mkTypeError (Pr OpenFp) "(Tx, { mode ← mode ∈ [`r, `w, `rw, `a] })" (Sq args)
    where
    mode m = case unintern m of
        "r" -> ReadMode
        "w" -> WriteMode
        "a" -> AppendMode
        "rw" -> ReadWriteMode
        _ -> error "TODO Type error"
openFp args = return . Left $ mkTypeError (Pr OpenFp) "(Tx, { mode ← mode ∈ [`r, `w, `rw, `a] })" args

readFp :: Val -> IO (Fallible Val)
readFp (Fp fp) = do
    c_m <- tryIOError $ hGetChar fp
    return $ case c_m of
        Right c -> Right $ mkInt (ord c)
        Left err -> Left $ (getTag exnIOError, mkSq [exnIOError, mkTx $ show err])
readFp x = return . Left $ mkTypeError (Pr OpenFp) "Fp" x

writeFp :: Val -> IO (Fallible Val)
writeFp (Sq args) = case toList args of
    [Fp fp, x] -> case ensureByte x of
        Right byte -> do
            success <- tryIOError $ hPutChar fp byte
            return $ case success of
                Right () -> Right $ mkOb []
                Left err -> Left $ (getTag exnIOError, mkSq [exnIOError, mkTx $ show err])
        Left err -> return . Left $ mkTypeError (Pr WriteFp) "(Fp, Byte)" (Sq args)
    _ -> return . Left $ mkTypeError (Pr WriteFp) "(Fp, Byte)" (Sq args)
    where
    ensureByte (Nm q) | denominator q == 1 =
        let n = numerator q
        in if 0 <= n && n < 256
            then Right (chr $ fromIntegral n)
            else Left $ mkTypeError (Pr WriteFp) "(Fp, Byte)" (Sq args)
    ensureByte _ = Left $ mkTypeError (Pr WriteFp) "(Fp, Byte)" (Sq args)
writeFp args = return . Left $ mkTypeError (Pr WriteFp) "(Fp, Byte)" args

flushFp :: Val -> IO (Fallible Val)
flushFp (Fp fp) = do
    success <- tryIOError $ hFlush fp
    return $ case success of
        Right () -> Right $ mkOb []
        Left err -> Left $ (getTag exnIOError, mkSq [exnIOError, mkTx $ show err])
flushFp x = return . Left $ mkTypeError (Pr FlushFp) "Fp" x

closeFp :: Val -> IO (Fallible Val)
closeFp (Fp fp) = do
    hClose fp
    return . Right $ mkOb []
closeFp x = return . Left $ mkTypeError (Pr CloseFp) "Fp" x

------ Sequence/Text/Bytes ------
len :: Val -> Fallible Val
len (Sq xs) = Right . mkInt $ Seq.length xs
len (Tx xs) = Right . mkInt $ T.length xs
len (By xs) = Right . mkInt $ BS.length xs
len x = Left $ mkTypeError (Pr Len) "Sq * | Tx | By" x

cat :: Val -> Val -> Fallible Val
cat (Sq xs) (Sq ys) = Right . Sq $ xs <> ys
cat (Tx xs) (Tx ys) = Right . Tx $ xs <> ys
cat (By xs) (By ys) = Right . By $ xs <> ys
cat x y = Left $ mkTypeError (Pr Cat) "∀ f :: Sq * | Tx | By. (f, f)" (mkSq [x, y])

cut :: Val -> Val -> Fallible Val --FIXME I guess I really need to return (Either Val Val), where the left is an exception to raise
cut x (Nm q) = 
    case x of
        Sq xs -> do
            i <- n
            when (i >= Seq.length xs) $ ixErr
            let (as, bs) = Seq.splitAt i xs
            Right $ mkSq [Sq as, Sq bs]
        Tx xs -> do
            i <- n
            when (i >= T.length xs) $ ixErr
            let (as, bs) = T.splitAt i xs
            Right $ mkSq [Tx as, Tx bs]
        By xs -> do
            i <- n
            when (i >= BS.length xs) $ ixErr
            let (as, bs) = BS.splitAt i xs
            Right $ mkSq [By as, By bs]
        _ -> Left $ mkTypeError (Pr Cut) "(Sq * | Tx | By, Nat)" (mkSq [x, Nm q])
    where
    n = if denominator q == 1
        then Right (fromIntegral $ numerator q)
        else Left $ mkTypeError (Pr Cut) "(Sq * | Tx | By, Nat)" (mkSq [x, Nm q])
    ixErr = Left (getTag exnIndexError, mkSq [exnIndexError, Nm q, x])
cut xs n = Left $ mkTypeError (Pr Cut) "(Sq * | Tx | By, Nat)" (mkSq [xs, n])


------ Xons ------
{-| @get x f@ retrieves field @f@ from @x@, if the field exists. -}
get :: Val -> Val -> Fallible Val
get (Ob ob) (Sy sy) = maybe (Left (getTag exnAttrError, mkSq [exnAttrError, Sy sy, Ob ob])) Right $ Map.lookup sy ob
get ob sy = Left $ mkTypeError (Pr Get) "(Ob, Sy)" (mkSq [ob, sy])

{-| Get a list of the fields in a value. -}
keys :: Val -> Fallible Val
keys (Ob ob) = Right . mkSq $ Sy <$> Map.keys ob
keys _ = Right $ mkSq []

{-| @extend a b@ extends and overwrites bindings in @b@ with bindings in @a@. -}
extend :: Val -> Val -> Val
extend (Ob ob') (Ob ob) = Ob $ Map.union ob' ob
extend _ (Ob ob) = Ob ob
extend (Ob ob') _ = Ob ob'
extend _ _ = mkOb []

{-| @delete x f@ removes field @f@ from @x@, if the field exists.
    If it does not exist, then there is no change.
-}
delete :: Val -> Val -> Fallible Val
delete (Ob ob) (Sy sy) = Right . Ob $ Map.delete sy ob
delete x _ = Right x




