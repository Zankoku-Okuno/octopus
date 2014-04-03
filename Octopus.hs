module Octopus where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader

import Octopus.Data
import qualified Octopus.Primitive as Oct
import Octopus.Basis
import Octopus.Shortcut


eval :: ImportsCache -> Val -> Val -> IO Val
eval cache env code = evalStateT (runReaderT (reduce code) cache) startState
    where
    startState = MState { environ = env --Ob Map.empty
                        , control = [NormK []]
                        }


reduce :: Val -> Machine Val
reduce x@(Nm _) = done x
reduce x@(By _) = done x
reduce x@(Tx _) = done x
reduce x@(Fp _) = done x
reduce x@(Tg _) = done x
reduce x@(Ab _ _) = done x
reduce x@(Cl _ _ _) = done x
reduce x@(Ce _) = done x
reduce x@(Ar _) = done x
reduce x@(Pr _) = done x
reduce sq@(Sq xs) = case toList xs of
    [] -> done sq
    (x:xs) -> push (Es [] xs) >> reduce x
reduce ob@(Ob m) = case ensureCombination ob of
    Just (f, x) -> push (Op x) >> reduce f
    Nothing -> case Map.toList m of
                [] -> done (mkOb [])
                ((k,v):xs) -> push (Eo k [] xs) >> reduce v
reduce (Sy x) = gets environ >>= \env -> case Oct.resolveSymbol x env of
    Just val -> done val
    Nothing -> do
        env <- gets environ
        error $ "TODO unbound symbol: " ++ show x ++ "\n" ++ show env

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
        let x' = mkSq [caller, x]
        case Oct.match var x' of
            Right env' -> swapEnv (env' `Oct.extend` env) >> reduce ast
            Left err -> error "raise match failure"
    _ -> error $ "raise not a combiner:\n" ++ show f

apply :: Val -> Val -> Machine Val
apply (Pr Eval) x = case ensureThunk x of
    Just (env, ast) -> swapEnv env >> reduce ast
    _ -> error $ "raise invalid args to primitive eval: " ++ show x
apply (Pr Ifz) x = case x of
    Sq xs -> case toList xs of
        [p, c, a] -> done $ Oct.ifz p c a
        _ -> error "raise wrong number of args to primitive Ifz"
    _ -> error "raise invalid args to primitive Ifz"
apply (Pr Extends) x = case x of
    Sq xs -> case toList xs of
        [] -> done $ mkOb []
        xs -> done $ foldr1 Oct.extend xs
    _ -> error "raise invalid args to primitive Extends"
apply (Pr pr) x =
    case lookup pr table of
        Just f -> case f pr x of
            Right val -> done val
            Left err -> error $ "raise some error in primitive " ++ show pr
        Nothing -> error $ "unknown primitive " ++ show pr
    where
    table = 
        [ (Match, binary Oct.match)
        
        , (Delete, binary Oct.delete)
        , (Keys, unary Oct.keys)
        , (Get, binary Oct.get)
        
        , (Eq, binary Oct.eq)
        , (Neq, binary Oct.neq)
        , (Lt, binary Oct.lt)
        , (Lte, binary Oct.lte)
        , (Gt, binary Oct.gt)
        , (Gt, binary Oct.gt)
    
        , (Add, binary Oct.add)
        , (Sub, binary Oct.sub)
        , (Mul, binary Oct.mul)
        , (Div, binary Oct.div)
    
        , (Len, unary Oct.len)
        , (Cat, binary Oct.cat)
        , (Cut, binary Oct.cut)
    
        , (Numer, unary Oct.numer)
        , (Denom, unary Oct.denom)
        , (NumParts, unary Oct.numParts)
        ]
    binary :: (Val -> Val -> Falible Val) -> Primitive -> Val -> Falible Val
    binary op pr x = case x of
        Sq xs -> case toList xs of
            [a, b] -> op a b
            _ -> error $ "raise wrong number of args to primitive " ++ show pr
        _ -> error $ "raise invalid args to primitive " ++ show pr
    unary :: (Val -> Falible Val) -> Primitive -> Val -> Falible Val
    unary op pr x = op x
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




