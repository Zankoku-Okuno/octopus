module Octopus.Libraries where

import Import
import Octopus.Data
import Octopus.Basis


startData = mkOb [
    --- Non-data ---
      (intern "__vau__",      Pr Vau)
    , (intern "__match__",    Pr Match)
    , (intern "__eval__",     Pr Eval)
    , (intern "__ifz!__",     Pr Ifz)
    , (intern "__import__",   Pr Imp)
    --- Relationals ---
    , (intern "__eq__",       Pr Eq)
    , (intern "__neq__",      Pr Neq)
    , (intern "__lt__",       Pr Lt)
    , (intern "__lte__",      Pr Lte)
    , (intern "__gt__",       Pr Gt)
    , (intern "__gte__",      Pr Gt)
    --- Arithmetic ---
    , (intern "__add__",      Pr Add)
    , (intern "__mul__",      Pr Mul)
    , (intern "__sub__",      Pr Sub)
    , (intern "__div__",      Pr Div)
    --- Numbers ---
    , (intern "__numer__",    Pr Numer)
    , (intern "__denom__",    Pr Denom)
    , (intern "__numparts__", Pr NumParts)
    --- Lists ---
    , (intern "__len__",      Pr Len)
    , (intern "__cat__",      Pr Cat)
    , (intern "__cut__",      Pr Cut)
    --- Xonses ---
    , (intern "__extends__",  Pr Extends)
    , (intern "__del__",      Pr Delete)
    , (intern "__keys__",     Pr Keys)
    --TODO __lookup__
    --- Niceties ---
    , (intern "vau",          vauDef)
    , (intern "__get__",      getDef)
    , (intern "__let__",      letDef)
    , (intern "__lambda__",   lambdaDef)
    , (intern "__quote__",    quoteDef)
    ]

vauDef = Cl
    (mkSq [mkOb [], mkSy "x"])
    (mkCall (Pr Vau) (mkSq [mkSq [mkSy "static", mkSy "body"],
        mkCall (Pr Vau) (mkSq [mkSy "arg",
            mkCall (Pr Eval) (mkSq [
                mkCall (Pr Extends) (mkSq [
                    mkCall (Pr Match) (mkSq [mkSy "x", mkSy "arg"]),
                    mkSy "static"]),
                mkSy "body"])])]))
    (mkOb [])


delDef = Cl 
    (mkSy "ob")
    (mkCall (Pr Vau) (mkSq [mkSq [mkOb [], mkSy "x"],
        mkCall (Pr Delete) (mkSq [mkCall (Pr Eval) (mkSy "ob"), mkSy "x"])]))
    (mkOb [])

getDef = Cl
    (mkSq [mkOb [], mkSy "x"])
    (mkCall (Pr Vau) (mkSq [mkSy "ob",
        mkCall (Pr Eval) (mkSq [mkCall (Pr Eval) (mkSy "ob"), mkSy "x"])]))
    (mkOb [])


letDef = Cl
    (mkSq [mkOb [], mkSy "x"])
    (mkCall (Pr Vau) (mkSq [mkSy "val",
        mkCall (Pr Vau) (mkSq [mkSq [mkSy "e", mkSy "body"],
            mkCall (Pr Eval) (mkSq
                [ mkCall (Pr Extends) (mkSq 
                    [ mkCall (Pr Match) (mkSq [mkSy "x", mkCall (Pr Eval) (mkSy "val")])
                    , mkSy "e"])
                , mkSy "body"])])]))
    (mkOb [])

lambdaDef = Cl
    (mkSq [mkOb [], mkSy "var"])
    (mkCall (Pr Vau) (mkSq [mkSq [mkSy "static", mkSy "ast"],
        mkCall (Pr Vau) (mkSq [mkSy "arg",
            mkCall (Pr Eval) (mkSq
                [ mkCall (Pr Extends) (mkSq
                    [ mkCall (Pr Match) (mkSq [mkSy "var", mkCall (Pr Eval) (mkSy "arg")])
                    , mkSy "static"])
                , mkSy "ast"])])]))
    (mkOb [])
quoteDef = Cl (mkSq [mkOb [], mkSy "ast"]) (mkSy "ast") (mkOb [])