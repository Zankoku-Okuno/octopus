{-| This module contains values that help create and manipulate Octopus
    values that are used by the interpreter, but not strictly primitive,
    that is, they could just as easily be defined in user-space, as long
    as the machine could get at them.
-}
module Octopus.Basis (
      mkSym
    , mkSeq
    , mkObj
    , fromEnv
    -- * Basic Protocols
    -- ** Combination
    , mkCombination
    , combineF
    , combineX
    , ensureCombination
    -- ** Closure
    , closureBody
    , closureEnv
    , closureArg
    , ensureClosure
    -- ** Suspension
    , mkThunk
    , ensureThunk
    ) where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Foldable
import Data.Traversable hiding (mapM)

import Octopus.Data

mkSym :: String -> Val
mkSym = Sy . intern

mkSeq :: [Val] -> Val
mkSeq = Sq . Seq.fromList

mkObj :: [(Symbol, Val)] -> Val
mkObj = Ob . Map.fromList

fromEnv :: Val -> Map Symbol Val
fromEnv (Ob ob) = ob
fromEnv _ = Map.empty


{-| Construct a combination: an object with a
    @__car__@ slot and a @__cdr__@ slot.
-}
mkCombination :: Val -- ^ Combiner (@__car__@)
           -> Val -- ^ Argument (@__cdr__@)
           -> Val
mkCombination f x = Ob $ Map.fromList [(combineF, f), (combineX, x)]

{-| Extract a (combiner, argument) pair from a
    combination-responsive object.
-}
ensureCombination :: Val -> Maybe (Val, Val)
ensureCombination (Ob ob) = (,) <$> Map.lookup combineF ob
                                <*> Map.lookup combineX ob
ensureCombination _ = Nothing

{-| Combiner slot name in a combination (aka. application) -}
combineF :: Symbol
combineF = intern "__car__"
{-| Argument slot name in a combination (aka. application) -}
combineX :: Symbol
combineX = intern "__cdr__"


{-| Construct a closure: an object with @__ast__@,
    @__env__@ and @__arg__@ slots.
-}
mkClosure :: Val -- ^ Body (@__ast__@)
          -> Val -- ^ Static environment (@__env__@)
          -> Val -- ^ Parameter (@__arg__@)
          -> Val
mkClosure ast env arg = Ob $ Map.fromList
    [ (closureBody, ast), (closureEnv, env), (closureArg, arg) ]

{-| Body slot name in a closure (aka. application) -}
closureBody :: Symbol
closureBody = intern "__ast__"
{-| Static environment slot name in a closure (aka. application) -}
closureEnv :: Symbol
closureEnv = intern "__env__"
{-| Parameter slot name in a closure (aka. application) -}
closureArg :: Symbol
closureArg = intern "__arg__"

{-| Extract a (body, environment, parameter) triple from
    a closure-responsive object.
-}
ensureClosure :: Val -> Maybe (Val, Val, Val)
ensureClosure (Ob ob) = (,,) <$> Map.lookup closureBody ob
                             <*> Map.lookup closureEnv ob
                             <*> Map.lookup closureArg ob
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










