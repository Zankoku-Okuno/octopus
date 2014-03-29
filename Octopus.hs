module Octopus where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Foldable
import Data.Traversable

import Control.Monad.State

import Octopus.Data
import Octopus.Primitive
import Octopus.Basis

--TODO
--figure out how to sequence stuff
--  the trick is to pass an environment from one argument to another
--Syntax:
--  object = braces $ many (symbol <*> expr)
--  sequence = brackets $ many1 expr `sepBy` comma
--  combine = foldr mkCombiner <$> parens (many expr) --more-or-less
--  block = ? $ expr `endBy` newline


--FIXME DELME all these test cases

--(((vau x) x) four)
noevalData = mkCombination (mkCombination (mkCombination (mkSym "vau") (mkSym "x")) (mkSym "x")) (mkSym "four")
--(((vau x) (force x)) four)
callData = mkCombination (mkCombination (mkCombination (mkSym "vau") (mkSym "x")) (mkCombination (mkSym "force") (mkSym "x"))) (mkSym "four")
--(((vau [e,ast]) e) dne)
getenvProg = mkCombination (mkCombination (mkCombination (mkSym "vau") (mkSeq [mkSym "e", mkSym "ast"])) (mkSym "e")) (mkSym "dne")
--(((vau [e,ast]) ast) dne)
quoteProg = mkCombination (mkCombination (mkCombination (mkSym "vau") (mkSeq [mkSym "e", mkSym "ast"])) (mkSym "ast")) (mkSym "dne")
--(((vau [e,ast]) (force [(**mkenv** [{:five 5},e]),ast])) five)
fiveProg = mkCombination
    (mkCombination
        (mkCombination (mkSym "vau") (mkSeq [mkSym "e", mkSym "ast"]))
        (mkCombination
            (mkSym "force") (mkSeq
                [ mkCombination (Pr MkEnv) (mkSeq [mkObj [(intern "five", Nm (5%1))], mkSym "e"])
                , mkSym "ast"
                ])))
    (mkSym "five")



mkVau e arg body = mkCombination (Pr Vau) (mkSeq [mkSeq [Sy $ intern e, Sy $ intern arg], body])
startData = Ob $ Map.fromList [
      (intern "four", Nm (4%1))
    , (intern "vau", vauDef)
    , (intern "force", Pr Eval)
    ]
    where
    --(**vau** [[_,var], (**vau** [[e.body],
    --    { :__var__ var
    --      :__ast__ body
    --      :__env__ e
    --    }])])
    vauDef = mkObj
        [ (closureArg, mkSeq [mkSym "_", mkSym "var"])
        , (closureBody, mkVau "e" "body"
            (mkObj
                [ (closureArg, mkSym "var")
                , (closureBody, mkSym "body")
                , (closureEnv, mkSym "e")
                ]))
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
reduce (Sy x) = gets environ >>= \env -> case resolveSymbol x env of
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
        _ -> error "raise wrong number of args to vau"
    _ -> error "raise invalid args to vau"
combine (Pr Eval) x = push (Ap (Pr Eval)) >> reduce x
combine (Pr MkEnv) x = push (Ap (Pr MkEnv)) >> reduce x
combine f x = case ensureClosure f of --all of these are operatives
    Just (ast, env, arg) -> do
        caller <- gets environ
        let x' = (Sq $ Seq.fromList [caller, x])
        case match arg x' of
            Nothing -> error "raise match failure"
            Just env' -> swapEnv (env' `extend` env) >> reduce ast
    _ -> error $ "raise not a combiner:\n" ++ show f

apply :: Val -> Val -> Machine Val
apply (Pr Eval) x = case ensureThunk x of
    Just (env, ast) -> swapEnv env >> reduce ast
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
        (NormK (Es xs (x':xs'):_)):_ -> replace (Es (x:xs) xs') >> reduce x'
        (NormK (Eo k xs []:_)):_ -> pop >> done (Ob . Map.fromList $ (k,x):xs)
        (NormK (Eo k xs ((k',x'):xs'):_)):_ -> replace (Eo k' ((k,x):xs) xs') >> reduce x'
        (NormK (Op arg:ks)):kss -> pop >> combine x arg
        (NormK (Ap f:ks)):kss -> pop >> apply f x








