module Octopus where

import Debug.Trace

import Data.List
import Data.Ratio
import Data.Symbol
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Data.Array.IO
import System.IO

import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad
import Control.Monad.State

import Octopus.Builtin

--TODO
{-  Calling vau means wrapping the passed ast into a new obj under __ast__ and placing the caller's env under __caller__.
    In fact, __ast__ + __caller__ may as well be the suspension protocol.
-}

--FIXME DELME
blahData = Ob $ Map.fromList
        [ (intern "__car__", Pr Eval)
        , (intern "__cdr__", Sq $ Seq.fromList
            [ Sq $ Seq.fromList [Sy (intern "inc"), Nm (2%1), Ab 0 $ Nm (1%3)]
            , Ob $ Map.fromList [(intern "inc", Nm (4%1))]
            ])
        ]


data Context = Op Val -- ^ Hole must be a combiner, val is the uneval'd argument
             | Ap Val -- ^ Hole is the argument, apply held value to it
             | Es [Val] [Val] -- ^ Left-to-right sequence evaluation
             | Eo Symbol [(Symbol, Val)] [(Symbol, Val)] -- ^ Object evaluation
    deriving (Eq, Show)
data Control = NormK [Context]
             | HndlK Val
             | EnvRK Val
    deriving (Eq, Show)
          --TODO onEnter/onSuccess/onFail
push :: Context -> Machine ()
push k = do
    (NormK ks):kss <- gets control
    modify $ \s -> s {control = (NormK (k:ks)):kss}
pop :: Machine ()
pop = do
    stack <- gets control
    case stack of
        (NormK []):kss -> modify $ \s -> s {control = kss}
        (NormK (_:ks)):kss -> modify $ \s -> s {control = (NormK ks):kss}
        (EnvRK env):kss -> modify $ \s -> s {environ = env, control = kss}
replace :: Context -> Machine ()
replace k = do
    stack <- gets control
    case stack of
        (NormK []):kss -> modify $ \s -> s {control = (NormK [k]):kss}
        (NormK (_:ks)):kss -> modify $ \s -> s {control = (NormK (k:ks)):kss}
swapEnv :: Val -> Machine ()
swapEnv env' = do
    --FIXME tail-recurse: destroy empty continuation with environment directly underneath
    env <- gets environ
    kss <- gets control
    modify $ \s -> s {environ = env', control = (NormK []):(EnvRK env):kss}


data Val = Nm Rational -- ^ Rational number
         | Pr Primitive -- ^ Primitive operations
         | Fp Handle -- ^ Input/output handle
         | Pt Int -- ^ Control prompt
         | Ks [Control] -- ^ Control stack 
         | Ab Int Val -- ^ Abstract data
         | Ce (IORef Val) -- ^ Reference cell
         | Ar (IOArray Int Val) -- ^ Mutable array
         | Sq (Seq Val) -- ^ Sequence, aka. list
         | Ob (Map Symbol Val) -- ^ Symbol-value map, aka. object
         | Sy Symbol -- ^ Symbol, aka. identifier
         --TODO concurrency
    deriving (Eq)
data Primitive = Vau | Eval | MkEnv
    deriving (Eq, Show)

eval :: Val -> Machine Val
eval x@(Nm _) = done x
eval x@(Ab _ _) = done x
eval x@(Ce _) = done x
eval x@(Ar _) = done x
eval x@(Fp _) = done x
eval x@(Pr _) = done x
eval (Sy x) = gets environ >>= resolve x >>= \val_m -> case val_m of
    Just val -> done val
    Nothing -> error $ "TODO unbound symbol: " ++ show x
eval sq@(Sq xs) = case toList xs of
    [] -> done sq
    (x:xs) -> push (Es [] xs) >> eval x
eval ob@(Ob m) = case ensureCombination ob of
    Just (f, x) -> push (Op x) >> eval f
    Nothing -> case Map.toList m of
        [] -> done (Ob Map.empty)
        ((k,v):xs) -> push (Eo k [] xs) >> eval v

done :: Val -> Machine Val
done x = do
    k <- gets control
    case k of
        [] -> return x
        (EnvRK _):_ -> pop >> done x
        (NormK []):_ -> pop >> done x
        (NormK (Es xs []:_)):_ -> pop >> done (Sq . Seq.fromList $ reverse (x:xs))
        (NormK (Es xs (x':xs'):_)):_ -> replace (Es (x:xs) xs') >> eval x'
        (NormK (Eo k xs []:_)):_ -> pop >> done (Ob . Map.fromList $ (k,x):xs)
        (NormK (Eo k xs ((k',x'):xs'):_)):_ -> replace (Eo k' ((k,x):xs) xs') >> eval x'
        (NormK (Op arg:ks)):kss -> pop >> combine x arg
        (NormK (Ap f:ks)):kss -> pop >> apply f x

resolve :: Symbol -> Val -> Machine (Maybe Val)
resolve sy (Ob env) = pure $ Map.lookup sy env
resolve sy _ = error "TODO resolve"

bind :: Val -> Val -> Val -> Machine (Maybe Val)
bind (Ob env) (Sy sy) x = pure . Just . Ob $ Map.insert sy x env
bind _ _ _ = pure Nothing

combine :: Val -> Val -> Machine Val
combine f@(Pr Eval) x = case x of
    Sq xs -> case toList xs of
        [ast, env] -> swapEnv env >> eval ast
        _ -> error "TODO raise wrong number of args to eval"
    _ -> error "TODO raise invalid args to eval"
combine (Pr _) x = error "TODO combine primitives"
combine f x = case ensureClosure f of
    Nothing -> error "TODO raise not combiner"
    Just (ast, env, arg) -> do
        env' <- bind env arg x
        error "TODO combine"

apply :: Val -> Val -> Machine Val
apply f x = error "TODO apply"



ensureCombination :: Val -> Maybe (Val, Val)
ensureCombination (Ob ob) = (,) <$> Map.lookup combineF ob
                                <*> Map.lookup combineX ob
ensureCombination _ = Nothing

ensureClosure :: Val -> Maybe (Val, Val, Val)
ensureClosure (Ob ob) = (,,) <$> Map.lookup closureBody ob
                          <*> Map.lookup closureEnv ob
                          <*> Map.lookup closureArg ob
ensureClosure _ = Nothing

ensureThunk :: Val -> Maybe (Val, Val)
ensureThunk (Ob ob) = (,) <$> Map.lookup thunkBody ob
                          <*> Map.lookup thunkEnv ob
ensureThunk _ = Nothing






data MState = MState { environ :: Val, control :: [Control] }
type Machine = StateT MState IO

runMachine :: Val -> IO Val
runMachine code = evalStateT (eval code) MState { environ = Ob Map.empty
                                                , control = [NormK []]
                                                }




instance Show Val where
    show (Nm nm) | denominator nm == 1 = show $ numerator nm
                 | otherwise           = show (numerator nm) ++ "/" ++ show (denominator nm)
    show (Sy sy) = show sy
    show (Ab tag x) = "<box " ++ show tag ++ ": " ++ show x ++ ">"
    show (Ce x) = "<reference cell>" --TODO show contents
    show (Ar xs) = "<mutable array>" --TODO show contents
    show (Fp _) = "<handle>" --TODO at least show some metadata
    show (Sq xs) = "[" ++ intercalate ", " (show <$> toList xs) ++ "]"
    show (Ob m) = "{" ++ intercalate " " (showPair <$> Map.toList m) ++ "}"
        where
        showPair (k,v) = ":" ++ show k ++ " " ++ show v
    show (Pr f) = "<primitive: " ++ show f ++ ">"
