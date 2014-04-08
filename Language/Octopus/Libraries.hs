module Language.Octopus.Libraries where

import Import
import Language.Octopus.Data
import Language.Octopus.Data.Shortcut
import Language.Octopus.Basis

initialEnv = mkXn [
      (intern "__let__", letDef)
    , (intern "__open__", openDef)
    --- Exceptions --- TODO give builtin literals
    , (intern "TypeError",    exnTypeError)
    , (intern "MatchFailure", exnMatchFail)
    , (intern "DivZero",      exnDivZero)
    ]

letDef = Cl
    (mkSq [mkXn [], mkSy "x"])
    (mkCall (Pr Vau) (mkSq [mkSy "val",
        mkCall (Pr Vau) (mkSq [mkSq [mkSy "e", mkSy "body"],
            mkCall (Pr Eval) (mkSq
                [ mkCall (Pr Extends) (mkSq 
                    [ mkCall (Pr Match) (mkSq [mkSy "x", mkCall (Pr Eval) (mkSy "val")])
                    , mkSy "e"])
                , mkSy "body"])])]))
    (mkXn [])
openDef = Cl
    (mkSy "env")
    (mkCall (Pr Vau) (mkSq [mkSq [mkSy "static", mkSy "body"],
        mkCall (Pr Eval) (mkSq 
            [ mkCall (Pr Extends) (mkSq
                [ mkCall (Pr Eval) (mkSy "env")
                , mkSy "static"])
            , mkSy "body"])]))
    (mkXn [])
    

