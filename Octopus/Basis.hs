{-| This module contains values that help create and manipulate Octopus
    values that are used by the interpreter, but not strictly primitive,
    that is, they could just as easily be defined in user-space, as long
    as the machine could get at them.
-}
module Octopus.Basis (
    -- * Basic Protocols
    -- ** Combination
      mkCall
    , callOpr
    , callArg
    , ensureCombination
    -- ** Closure
    , mkClosure
    , ensureClosure
    -- ** Suspension
    , mkThunk
    , ensureThunk
    ) where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Octopus.Data
import Octopus.Shortcut


{-| Construct a combination: an object with a
    @__car__@ slot and a @__cdr__@ slot.
-}
mkCall :: Val -- ^ Combiner (@__car__@)
           -> Val -- ^ Argument (@__cdr__@)
           -> Val
mkCall f x = mkObj [(callOpr, f), (callArg, x)]

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










