{-# LANGUAGE OverloadedStrings #-}
{-| This module contains values that help create and manipulate Octopus
    values that are used by the interpreter, but not strictly primitive,
    that is, they could just as easily be defined in user-space, as long
    as the machine could get at them.
-}
module Language.Octopus.Basis where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import System.IO

import Language.Octopus.Data
import Language.Octopus.Data.Shortcut


fpStdin = Fp stdin
fpStdout = Fp stdout
fpStderr = Fp stderr

tyNm           = Tg ( 0, "Nm")
tyBy           = Tg ( 1, "By")
tyTx           = Tg ( 2, "Tx")
tyFp           = Tg ( 3, "Fp")
tySy           = Tg ( 4, "Sy")
tyTg           = Tg ( 5, "Tg")
tySq           = Tg ( 6, "Sq")
tyXn           = Tg ( 7, "Xn")
tyFn           = Tg ( 8, "* -> *")
tyCe           = Tg ( 9, "Ce")
tyAr           = Tg (10, "Ar")

exnTypeError   = Tg (20, "TypeError")
exnMatchFail   = Tg (21, "MatchFailure")
exnScopeError  = Tg (22, "ScopeError")
exnAttrError   = Tg (23, "AttributeError")
exnIndexError  = Tg (24, "IndexError")
exnDivZero     = Tg (25, "DivideByZero")
exnIOError     = Tg (26, "IOError")
exnSyntaxError = Tg (27, "SyntaxError")
exnImportError = Tg (28, "ImportError")

startTag :: Word
startTag       =     50



mkTypeError :: Val -> Text -> Val -> (Word, Val)
mkTypeError f ty val = (getTag exnTypeError, mkSq [exnTypeError, f, Tx ty, val])
mkMatchFail :: Val -> Val -> (Word, Val)
mkMatchFail p v = (getTag exnMatchFail, mkSq [exnMatchFail, p, v])


mkVau :: String -> String -> Val -> Val
mkVau e arg body = mkCall (Pr Vau) (mkSq [mkSq [Sy $ intern e, Sy $ intern arg], body])

{-| Construct a combination: an object with a
    @__car__@ slot and a @__cdr__@ slot.
-}
mkCall :: Val -- ^ Combiner (@__car__@)
           -> Val -- ^ Argument (@__cdr__@)
           -> Val
mkCall f x = mkXn [(callOpr, f), (callArg, x)]

{-| Extract a (combiner, argument) pair from a
    combination-responsive object.
-}
ensureCombination :: Val -> Maybe (Val, Val)
ensureCombination (Xn xn) = (,) <$> Map.lookup callOpr xn
                                <*> Map.lookup callArg xn
ensureCombination _ = Nothing

{-| Combiner slot name in a combination (aka. application) -}
callOpr :: Symbol
callOpr = intern "__car__"
{-| Argument slot name in a combination (aka. application) -}
callArg :: Symbol
callArg = intern "__cdr__"


{-| Construct a closure: an object with @__var__@,
    @__ast__@ and @__env__@ slots.
-}
mkClosure :: Val -- ^ Body (@__ast__@)
          -> Val -- ^ Static environment (@__env__@)
          -> Val -- ^ Parameter (@__arg__@)
          -> Val
mkClosure var ast env = Cl var ast env

{-| Extract a (body, environment, parameter) triple from
    a closure-responsive object.
-}
ensureClosure :: Val -> Maybe (Val, Val, Val)
ensureClosure (Cl var ast env) = Just (var, ast, env)
ensureClosure _ = Nothing


{-| Construct a thunk: a (environment, body) pair. -}
mkThunk :: Val -- ^ Environment (second element)
        -> Val -- ^ Body (first element)
        -> Val
mkThunk env ast = Sq $ Seq.fromList [env, ast]

{-| Extract a (environment, body) pair from a thunk-responsive pair. -}
ensureThunk :: Val -> Maybe (Val, Val)
ensureThunk (Sq xs) = case toList xs of
    [env, ast] -> Just (env, ast)
    _ -> Nothing
ensureThunk _ = Nothing










