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

import Octopus.Data
import Octopus.Primitive
import Octopus.Basis

--TODO
--figure out how to sequence stuff
--possibly, simply add primitives for destructuring sequences?
--Syntax:
--  object = braces $ many (symbol <*> expr)
--  sequence = brackets $ many1 expr `sepBy` comma
--  combine = foldr mkCombiner <$> parens (many expr) --more-or-less
--  block = ? $ expr `endBy` newline


--FIXME DELME
--(**eval** [{:inc 4},[inc, 2, <box 0: 1/3>]])
blahData = Ob $ Map.fromList
        [ (combineF, Pr Eval)
        , (combineX, Sq $ Seq.fromList
            [ Ob $ Map.fromList [(intern "inc", Nm (4%1))]
            , Sq $ Seq.fromList [Sy (intern "inc"), Nm (2%1), Ab 0 $ Nm (1%3)]
            ])
        ]
--((**vau** [x,x]) four)
noevalData = mkCombination
    (mkCombination
        (Pr Vau)
        (mkSeq [Sy $ intern "x", Sy $ intern "x"])
    )
    (Sy $ intern "four")
--((**vau** [x,(**eval** x)]) four)
callData = mkCombination
    (mkCombination
        (Pr Vau)
        (mkSeq [Sy $ intern "x", mkCombination (Pr Eval) (Sy $ intern "x")])
    )
    (Sy $ intern "four")
--((**vau** [[e,ast],e]) {})
getenvProg = mkCombination
    (mkCombination
        (Pr Vau)
        (mkSeq [mkSeq [Sy $ intern "e", Sy $ intern "ast"], Sy $ intern "e"])
    )
    (Ob Map.empty)
--((**vau** [[e,ast],ast]) dne)
quoteProg = mkCombination
    (mkCombination
        (Pr Vau)
        (mkSeq [ mkSeq [Sy (intern "e"), Sy (intern "ast")], Sy (intern "ast") ])
    )
    (Sy $ intern "dne")
--((**vau** [[e,ast], (**eval** [(**mkenv** [{:five 5},e]),ast])]) five)
fiveProg = mkCombination
    (mkCombination
        (Pr Vau) (mkSeq [mkSeq [Sy (intern "e"), Sy (intern "ast")],
            mkCombination (Pr Eval)
                (mkSeq [ mkCombination (Pr MkEnv)
                            (mkSeq [ Ob $ Map.fromList [(intern "five", Nm (5%1))]
                                   , Sy $ intern "e"
                                   ])
                       , Sy (intern "ast")
                       ])
        ])
    )
    (Sy $ intern "five")
startData = Ob $ Map.fromList
    [ (intern "four", Nm (4%1))
    ]



runMachine :: Val -> IO Val
runMachine code = evalStateT (eval code) MState { environ = startData --Ob Map.empty
                                                , control = [NormK []]
                                                }





eval :: Val -> Machine Val
eval x@(Nm _) = done x
eval x@(Ab _ _) = done x
eval x@(Ce _) = done x
eval x@(Ar _) = done x
eval x@(Fp _) = done x
eval x@(Pr _) = done x
eval (Sy x) = gets environ >>= \env -> case resolveSymbol x env of
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

combine :: Val -> Val -> Machine Val
combine (Pr Vau) x = case x of
    Sq xs -> case toList xs of
        [arg, ast] -> do
            env <- gets environ
            done $ Ob $ Map.fromList [ (closureBody, ast)
                                     , (closureArg, arg)
                                     , (closureEnv, env)
                                     ]
        _ -> error "raise wrong number of args to vau"
    _ -> error "raise invalid args to vau"
combine (Pr Eval) x = push (Ap (Pr Eval)) >> eval x
combine (Pr MkEnv) x = push (Ap (Pr MkEnv)) >> eval x
combine f x = case ensureClosure f of --all of these are operatives
    Just (ast, env, arg) -> do
        caller <- gets environ
        let x' = (Sq $ Seq.fromList [caller, x])
        case extendEnvironment arg x' env of
            Just env' -> swapEnv env' >> eval ast
            Nothing -> error "raise match failure"
    _ -> error "raise not a combiner"

apply :: Val -> Val -> Machine Val
apply (Pr Eval) x = case ensureThunk x of
    Just (env, ast) -> swapEnv env >> eval ast
    _ -> error $ "raise invalid args to eval: " ++ show x
apply (Pr MkEnv) x = case x of
    Sq xs -> do
        done $ Ob $ Map.unions $ map fromEnv (toList xs)
    _ -> error "raise invalid args to mkEnv"
apply f x = error "TODO apply"

done :: Val -> Machine Val
done x = do
    k <- gets control
    case k of
        [] -> return x
        (NormK []):_ -> pop >> done x
        (NormK (Re _:_)):_ -> pop >> done x
        (NormK (Es xs []:_)):_ -> pop >> done (Sq . Seq.fromList $ reverse (x:xs))
        (NormK (Es xs (x':xs'):_)):_ -> replace (Es (x:xs) xs') >> eval x'
        (NormK (Eo k xs []:_)):_ -> pop >> done (Ob . Map.fromList $ (k,x):xs)
        (NormK (Eo k xs ((k',x'):xs'):_)):_ -> replace (Eo k' ((k,x):xs) xs') >> eval x'
        (NormK (Op arg:ks)):kss -> pop >> combine x arg
        (NormK (Ap f:ks)):kss -> pop >> apply f x








