module Octopus where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Foldable (toList)
import Data.Traversable

import Control.Monad.State

import Octopus.Data
import qualified Octopus.Primitive as Oct
import Octopus.Basis

--TODO
--figure out how to sequence stuff
--  the trick is to pass an environment from one argument to another
--Syntax:
--  object = braces $ many (symbol <*> expr)
--  sequence = brackets $ many1 expr `sepBy` comma
--  combine = foldr mkCombiner <$> parens (many expr) --more-or-less
--  block = ? $ expr `endBy` newline





mkVau e arg body = mkCombination (Pr Vau) (mkSeq [mkSeq [Sy $ intern e, Sy $ intern arg], body])
startData = mkObj [
      (intern "four", Nm (4%1)) --FIXME DELME
    , (intern "vau", vauDef)
    , (intern "force", Pr Eval)
    , (intern "extends", Pr Extends)
    , (intern "delete", delDef)
    , (intern "keys", Pr Keys)
    , (intern "__get__", getDef)
    ]
    where
    --(__vau__ [[_, var], (__vau__ [[e, body],
    --    { __var__: var
    --    , __ast__: body
    --    , __env__: e
    --    }])])
    vauDef = mkObj
        [ (closureArg, mkSeq [mkSym "_", mkSym "var"]) --FIXME use empty object instead of underscore
        , (closureBody, mkVau "e" "body"
            (mkObj
                [ (closureArg, mkSym "var")
                , (closureBody, mkSym "body")
                , (closureEnv, mkSym "e")
                ]))
        , (closureEnv, mkObj [])
        ]
    --(__vau__ [[_,x], __vau__ [ob, __delete__ [__force__ ob, x]]])
    delDef = mkObj
        [ (closureArg, mkSym "ob")
        , (closureBody, mkCombination (Pr Vau) (mkSeq [mkSeq [mkSym "_", mkSym "x"], --FIXME use empty object instead of underscore
            mkCombination (Pr Delete) (mkSeq [mkCombination (Pr Eval) (mkSym "ob"), mkSym "x"])]))
        , (closureEnv, mkObj [])
        ]
    --(__vau__ [[_, x], __vau__ [ob, __force__ [__force__ ob, x]]])
    --evals to
    --{
    --    __var__: [_, x],
    --    __ast__: __vau__ [ob,
    --                  __force__ [__force__ ob, x]],
    --    __env__: topLevel
    --}
    getDef = mkObj
        [ (closureArg, mkSeq [mkSym "_", mkSym "x"]) --FIXME use empty object instead of underscore
        , (closureBody, mkCombination (Pr Vau) (mkSeq [mkSym "ob",
            mkCombination (Pr Eval) (mkSeq [mkCombination (Pr Eval) (mkSym "ob"), mkSym "x"])]))
        , (closureEnv, mkObj [])
        ]


eval :: Val -> Val -> IO Val
eval env code = evalStateT (reduce code) MState { environ = env --Ob Map.empty
                                                , control = [NormK []]
                                                }





reduce :: Val -> Machine Val
reduce x@(Nm _) = done x
--reduce x@(Ab _ _) = done x
reduce x@(Ce _) = done x
reduce x@(Ar _) = done x
reduce x@(Fp _) = done x
reduce x@(Pr _) = done x
reduce (Sy x) = gets environ >>= \env -> case Oct.resolveSymbol x env of
    Just val -> done val
    Nothing -> do
        env <- gets environ
        error $ "TODO unbound symbol: " ++ show x ++ "\n" ++ show env
reduce sq@(Sq xs) = case toList xs of
    [] -> done sq
    (x:xs) -> push (Es [] xs) >> reduce x
reduce ob@(Ob m) = case ensureCombination ob of
    Just (f, x) -> push (Op x) >> reduce f
    Nothing -> case Map.toList m of
        [] -> done (Ob Map.empty)
        ((k,v):xs) -> push (Eo k [] xs) >> reduce v

combine :: Val -> Val -> Machine Val
combine (Pr Vau) x = case x of
    Sq xs -> case toList xs of
        [arg, ast] -> do
            env <- gets environ
            done $ Ob $ Map.fromList [ (closureBody, ast)
                                     , (closureArg, arg)
                                     , (closureEnv, env)
                                     ]
        _ -> error "raise wrong number of args to primitive vau"
    _ -> error "raise invalid args to primitive vau"
combine f@(Pr _) x = push (Ap f) >> reduce x
combine f x = case ensureClosure f of --all of these are operatives
    Just (ast, env, arg) -> do
        caller <- gets environ
        let x' = (Sq $ Seq.fromList [caller, x])
        case Oct.match arg x' of
            Nothing -> error "raise match failure"
            Just env' -> swapEnv (env' `Oct.extend` env) >> reduce ast
    _ -> error $ "raise not a combiner:\n" ++ show f

apply :: Val -> Val -> Machine Val
apply (Pr Eval) x = case ensureThunk x of
    Just (env, ast) -> swapEnv env >> reduce ast
    _ -> error $ "raise invalid args to primitive eval: " ++ show x
apply (Pr Extends) x = case x of
    Sq xs -> case toList xs of
        [] -> done $ mkObj []
        xs -> done $ foldr1 Oct.extend xs
    _ -> error "raise invalid args to primitive extends"
apply (Pr Delete) x = case x of
    Sq xs -> case toList xs of
        [ob,sy] -> done $ Oct.delete ob sy
        _ -> error "raise wrong number of args to primitive delete"
    _ -> error "raise invalid args to primitive delete"
apply (Pr Keys) x = done $ Oct.keys x
apply (Pr Get) x = case x of
    Sq xs -> case toList xs of
        [ob,sy] -> case Oct.get ob sy of
            Just val -> done val
            Nothing -> error $ "raise key error: " ++ show sy ++ "\n" ++ show ob
        _ -> error "raise wrong number of args to primitive get"
    _ -> error "raise invalid args to primitive get"
apply f x = error "TODO apply"

done :: Val -> Machine Val
done x = do
    k <- gets control
    case k of
        [] -> return x
        (NormK []):_ -> pop >> done x
        (NormK (Re _:_)):_ -> pop >> done x
        (NormK (Es xs []:_)):_ -> pop >> done (Sq . Seq.fromList $ reverse (x:xs))
        (NormK (Es xs (x':xs'):_)):_ -> replace (Es (x:xs) xs') >> reduce x'
        (NormK (Eo k xs []:_)):_ -> pop >> done (Ob . Map.fromList $ (k,x):xs)
        (NormK (Eo k xs ((k',x'):xs'):_)):_ -> replace (Eo k' ((k,x):xs) xs') >> reduce x'
        (NormK (Op arg:ks)):kss -> pop >> combine x arg
        (NormK (Ap f:ks)):kss -> pop >> apply f x








