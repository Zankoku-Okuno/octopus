module Octopus where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Control.Monad.State

import Octopus.Data
import qualified Octopus.Primitive as Oct
import Octopus.Shortcut
import Octopus.Basis




mkVau e arg body = mkCall (Pr Vau) (mkSeq [mkSeq [Sy $ intern e, Sy $ intern arg], body])
startData = mkObj [
      (intern "__vau__", Pr Vau)
    , (intern "__match__", Pr Match)
    , (intern "__eval__", Pr Eval)
    , (intern "__extends__", Pr Extends)
    , (intern "__del__", Pr Delete)
    , (intern "keys", Pr Keys)
    , (intern "vau", vauDef)
    , (intern "__get__", getDef)
    , (intern "__let__", letDef)
    , (intern "__lambda__", lambdaDef)
    , (intern "__quote__", quoteDef)
    ]
    where
    vauDef = Cl
        (mkSeq [mkObj [], mkSym "x"])
        (mkCall (Pr Vau) (mkSeq [mkSeq [mkSym "static", mkSym "body"],
            mkCall (Pr Vau) (mkSeq [mkSym "arg",
                mkCall (Pr Eval) (mkSeq [
                    mkCall (Pr Extends) (mkSeq [
                        mkCall (Pr Match) (mkSeq [mkSym "x", mkSym "arg"]),
                        mkSym "static"]),
                    mkSym "body"])])]))
        (mkObj [])
    delDef = Cl 
        (mkSym "ob")
        (mkCall (Pr Vau) (mkSeq [mkSeq [mkObj [], mkSym "x"],
            mkCall (Pr Delete) (mkSeq [mkCall (Pr Eval) (mkSym "ob"), mkSym "x"])]))
        (mkObj [])
    getDef = Cl
        (mkSeq [mkObj [], mkSym "x"])
        (mkCall (Pr Vau) (mkSeq [mkSym "ob",
            mkCall (Pr Eval) (mkSeq [mkCall (Pr Eval) (mkSym "ob"), mkSym "x"])]))
        (mkObj [])
    letDef = Cl
        (mkSeq [mkObj [], mkSym "x"])
        (mkCall (Pr Vau) (mkSeq [mkSym "val",
            mkCall (Pr Vau) (mkSeq [mkSeq [mkSym "e", mkSym "body"],
                mkCall (Pr Eval) (mkSeq
                    [ mkCall (Pr Extends) (mkSeq 
                        [ mkCall (Pr Match) (mkSeq [mkSym "x", mkCall (Pr Eval) (mkSym "val")])
                        , mkSym "e"])
                    , mkSym "body"])])]))
        (mkObj [])
    lambdaDef = Cl
        (mkSeq [mkObj [], mkSym "var"])
        (mkCall (Pr Vau) (mkSeq [mkSeq [mkSym "static", mkSym "ast"],
            mkCall (Pr Vau) (mkSeq [mkSym "arg",
                mkCall (Pr Eval) (mkSeq
                    [ mkCall (Pr Extends) (mkSeq
                        [ mkCall (Pr Match) (mkSeq [mkSym "var", mkCall (Pr Eval) (mkSym "arg")])
                        , mkSym "static"])
                    , mkSym "ast"])])]))
        (mkObj [])
    quoteDef = Cl (mkSeq [mkObj [], mkSym "ast"]) (mkSym "ast") (mkObj [])




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
reduce x@(Cl _ _ _) = done x
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
                [] -> done (mkObj [])
                ((k,v):xs) -> push (Eo k [] xs) >> reduce v

combine :: Val -> Val -> Machine Val
combine (Pr Vau) x = case x of
    Sq xs -> case toList xs of
        [var, ast] -> do
            env <- gets environ
            done $ Cl var ast env
        _ -> error "raise wrong number of args to primitive vau"
    _ -> error "raise invalid args to primitive vau"
combine f@(Pr _) x = push (Ap f) >> reduce x
combine f x = case ensureClosure f of --all of these are operatives
    Just (var, ast, env) -> do
        caller <- gets environ
        let x' = mkSeq [caller, x]
        case Oct.match var x' of
            Nothing -> error "raise match failure"
            Just env' -> swapEnv (env' `Oct.extend` env) >> reduce ast
    _ -> error $ "raise not a combiner:\n" ++ show f

apply :: Val -> Val -> Machine Val
apply (Pr Eval) x = case ensureThunk x of
    Just (env, ast) -> swapEnv env >> reduce ast
    _ -> error $ "raise invalid args to primitive eval: " ++ show x
apply (Pr Match) x = case x of
    Sq xs -> case toList xs of
        [pat,arg] -> case Oct.match pat arg of
            Just env -> done env
            Nothing -> error "raise match failure"
        xs -> error "raise wrong number of args to primitive match"
    _ -> error "raise invalid args to primitive extends"
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
        (NormK []):_                        -> pop >> done x
        (NormK (Re _:_)):_                  -> pop >> done x
        (NormK (Es xs []:_)):_              -> pop >> done (Sq . Seq.fromList $ reverse (x:xs))
        (NormK (Es xs (x':xs'):_)):_        -> replace (Es (x:xs) xs') >> reduce x'
        (NormK (Eo k xs []:_)):_            -> pop >> done (Ob . Map.fromList $ (k,x):xs)
        (NormK (Eo k xs ((k',x'):xs'):_)):_ -> replace (Eo k' ((k,x):xs) xs') >> reduce x'
        (NormK (Op arg:ks)):kss             -> pop >> combine x arg
        (NormK (Ap f:ks)):kss               -> pop >> apply f x








