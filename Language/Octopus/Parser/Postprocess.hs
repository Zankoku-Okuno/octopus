module Language.Octopus.Parser.Postprocess where

import Language.Desugar

import Language.Octopus.Data
import Language.Octopus.Data.Shortcut
import Language.Octopus.Basis
import Language.Octopus.Parser.Import
import Language.Octopus.Parser.Syntax


------ Desugar ------
desugar :: Syx -> Val
desugar (Lit x) = x
desugar (Call [x]) = desugar x
desugar (Call xs) = loop . (desugar <$>) $ revTripBy isInfix (id, rewrite) xs
    where
    rewrite [] inf rest = error "TODO syntax error: infix :/. needs a subject"
    rewrite subject (Infix inf) rest = inf : Call subject : rest
    loop [e] = e
    loop [f, x] = mkCall f x
    loop es = mkCall (loop $ init es) (last es)
    isInfix (Infix _) = True
    isInfix _ = False
desugar (SqSyx xs) = mkSq $ desugar <$> xs
desugar (XnExpr xs) = mkXn $ desugarField <$> xs
desugar (Do xs) = loop xs
    where
    loop [Defn ds x e]      = mkCall (mkDefn $ desugarDefine ds x e) (mkXn [])
    loop [Expr e]           = desugar e
    loop (Defn ds x e:rest) = mkCall (mkDefn $ desugarDefine ds x e) (loop rest)
    loop (LRec x e:rest)    = mkCall (mkDefn $ desugarLetrec x e) (loop rest)
    loop (Open e:rest)      = mkCall (mkOpen $ desugar e) (loop rest)
    loop (Expr e:rest)      = mkCall (mkExpr $ desugar e) (loop rest)
desugar x = error $ "INTERNAL ERROR Octopus.Parser.desugar: " ++ show x

desugarField :: (Symbol, Syx) -> (Symbol, Val)
desugarField (k, e) = (k, desugar e)

desugarDefine :: [Syx] -> Syx -> Syx -> (Val, Val)
desugarDefine ds x e = (desugar x, foldr mkCall (desugar e) (map desugar ds))

desugarLetrec :: Syx -> Syx -> (Val, Val)
desugarLetrec x e = let f = desugar x
                    in (f, mkCall (mkSy "__Y__") (mkCall (mkCall (mkSy "__lambda__") f) (desugar e)))


------ Helpers ------
mkDefn (x, val) = mkCall (mkCall (mkSy "__let__") x) val
mkOpen env = mkCall (mkSy "__open__") env
mkExpr e = mkCall (mkCall (mkSy "__let__") (mkXn [])) e

