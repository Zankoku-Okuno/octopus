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

exnTypeError   = Tg 0 "TypeError"
exnMatchFail   = Tg 1 "MatchFailure"
exnScopeError  = Tg 2 "ScopeError"
exnAttrError   = Tg 3 "AttributeError"
exnIndexError  = Tg 4 "IndexError"
exnDivZero     = Tg 5 "DivideByZero"
exnIOError     = Tg 6 "IOError"
exnSyntaxError = Tg 7 "SyntaxError"
exnImportError = Tg 8 "ImportError"
startTag :: Word
startTag       =    9



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
mkCall f x = mkOb [(callOpr, f), (callArg, x)]

{-| Extract a (combiner, argument) pair from a
    combination-responsive object.
-}
ensureCombination :: Val -> Maybe (Val, Val)
ensureCombination (Ob ob) = (,) <$> Map.lookup callOpr ob
                                <*> Map.lookup callArg ob
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










