module Octopus.Libraries where

import Import
import Octopus.Data
import Octopus.Shortcut
import Octopus.Basis


startData = mkOb [
    --- Niceties ---
      (intern "vau",          vauDef)
    , (intern "__get__",      getDef)
    , (intern "__let__",      letDef)
    , (intern "__lambda__",   lambdaDef)
    , (intern "__quote__",    quoteDef)
    --- Exceptions ---
    , (intern "TypeError",    exnTypeError)
    , (intern "MatchFailure", exnMatchFail)
    , (intern "DivZero",      exnDivZero)
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