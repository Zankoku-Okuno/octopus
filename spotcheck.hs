import System.IO
import System.Environment
import System.Exit
import Control.Concurrent.MVar

import Import
import Language.Octopus
import Language.Octopus.Parser
import Language.Octopus.Libraries
import Language.Octopus.Data
import Language.Octopus.Data.Shortcut
import Language.Octopus.Basis

import qualified Language.Octopus.Parser.Syntax as Syx

import qualified Data.Map as Map

main :: IO ()
main = do
    --parse_e <- parseOctopusFile "y.oct" <$> readFile "y.oct"
    --case parse_e of
    --    Left err -> print err
    --    Right (dirs, val) -> do
    --        print val
    --        --print =<< eval startData val
    testParse "\n  #qega\n b\"af \n 3637\""
    testParse "(a b\n   c d\n)"
    testParse "(a b\n   c d)"
    testParse "[1, 2 + 3\n,\n  f x]"
    testParse "{a: 1, b: 2,\n c: (3\n      4)\n}"
    testParse "do 3\n   f: lambda\n      4"
    testParse "do open #<import> module\n   letrec f: f 3"
    --testParse "do: 3 4;"
    testParse "3 .+ 4"
    testParse "x :y"
    testParse "x :( y: ++)"
    testParse "`q"
    exitSuccess

    test "\"hi\""
    test "r\"b\"\"\\foo\\n\""
    test "#<<END\na\\s;dg\nasdg\nEND>>"
    test "b\" 0abb 55 #should be 'U'\n\""

    test "3 #{block #{com}#ment #3 {} }#"
    test "do four: 4 ((vau x x) four);"
    test "do four: 4 ((vau x (#<eval> x)) four);"
    test "((vau [e, ast] e) dne)"
    test "((vau [e, ast] ast) dne)"
    test "(#<extends> [{a: 3}, {a: 2}, {a: 1}])"
    test "((vau [e, ast] (#<eval> [#<extends> [{five: 5}, e], ast])) five)"
    test "do four: 4 (vau [{}, xn] (#<keys> xn) {foo: 3, bar: four});"
    test "do four: 4 ((vau [e, xn] (@x xn)) {x: four});"
    test "do four: 4 ((vau [e, xn] (#<eval> [e, @x xn])) {x: four});"
    test $ "do four: 4 (" ++ lambda ++ " xn (@x xn) {x: four});"
    test $ "(" ++ lambda ++ " eight eight 8)"
    test $ "("++letin++" eight 8 eight)"
    
    test $ "("++lambda++" let 2 "++letin++")"
    test $ "("++lambda++" let (let x 2 x) "++letin++")"
    test $ "("++lambda++" lambda 2 "++lambda++")"
    test $ "("++lambda++" lambda (lambda x x 2) "++lambda++")"
    test "(__let__ let __let__ (let x 3 x))"
    test "(__let__ let __let__ (let x let (x y 3 y)))"
    test "(__let__ let __let__ (let x let (x x 3 x)))"
    test "(__lambda__ x x 7)"
    test "(__lambda__ \955 (\955 x 7 \955) __lambda__)"
    test "(__let__ \955 __lambda__ (\955 x (\955 x x) 2 7))"
    test "((__lambda__ __\955__ ((__\955__ \955 ((\955 x x) 7)) __\955__)) __lambda__)"

    --TODO MAYBE this stuff could be the real definitions of let/lambda, but I've already got them built-in to the start environment (__let__ for good reason)
    --test $ "("++lambda++" __\955__ (__\955__ \955 2) "++lambda++")"
    --test $ "("++lambda++" __\955__ (__\955__ \955 2 __\955__) "++lambda++")"
    --test $ "("++lambda++" __let__ (__let__ let __let__ 2) "++letin++")"
    test $ "("++letin++" let "++letin++" (let x 2 x))"
    test "[__lambda__ x x 3, __lambda__ y y 4]" --[3, 4]

    test "do \955: __lambda__\n   x: 1 \n   (\955 x x 3)\n   y: 2\n   [x, y];" --[1, 2]
    test "(#<del> [{x: 1, y: 2}, `y])"
    test "do \955: __lambda__\n\
         \   delete: (\955 xn (vau [{}, field] (#<del> [xn, field])))\n\
         \   (delete {x: 1, y: 2} x);"
    test "do x: do \955: __lambda__\n\
         \         (\955 y y 3);\n\
         \      x;"
    test "do defx: (__let__ x)\n\
         \   (defx 5 x);"
    test "do defx5: (__let__ x 5)\n\
         \   (defx5 x);"
    test "do [a, b, c]: [`there, `Bob, `hi] [c, a, b];"

    test "do x: (#<add> [1, 2])\n   y: (#<sub> [1, 2])\n   z: (#<mul> [2, 3])\n   w: (#<div> [2, 3])\n   [x, y, z, w];"
    test "do [a, b]: (#<cut> [[1, 2, 3, 4, 5], 3])\n   [a, b, #<len> a];"
    test "do [a, b]: (#<cut> [\"hello\", 3])\n   [a, b, #<len> a];"
    test "do y: (#<ifz!> [0, `y, `n])\n   n: (#<ifz!> [1, `y, `n])\n   [y, n];"

    test "do {\955: \955}: (#<import> \"/basis\") (\955 x x 6);"
    test "do {\955: \955}: (#<import> \"examples/basis\") (\955 x x 6);"

    test "do t: (#<mkTag> \"phoey\")\n   (#<handle> [t, 3, `5]);"
    test "do id: (__lambda__ x x)\n   t: (#<mkTag> \"phoey\")\n   (#<handle> [t, id,\n      `(#<add> [3, (#<raise> [t, 7])])]);"
    test "do \955: __lambda__\n\
         \   id: (\955 x x)\n\
         \   +: (\955 x (\955 y (#<add> [x, y])))\n\
         \   ex: (#<mkTag> \"exn\")\n\
         \   handle: (\955 tag (\955 handler (vau thunk\n\
         \              (#<handle> [tag, handler, `(#<eval> thunk)]))))\n\
         \   [ handle ex (+ 3) (+ 1 6)\n\
         \   , handle ex (+ 3) (+ 1 (#<raise> [ex, 10]))];"
    test "(#<handle> [#<TypeError>, (__lambda__ x x), `(#<add> 3 4)])"
    test "(#<handle> [#<TypeError>, (__lambda__ x x), `(#<add> [3, 4])])"
    test "(#<handle> [#<TypeError>, (__lambda__ x x), `(#<add> [3, 5, 4])])"
    test "(#<handle> [#<DivZero>, (vau {} 1), `do (#<div> [3, 0]) (#<raise> []);])"

    test "do \955: __lambda__\n\
         \   __modify__: (vau [{}, field] (\955 f (\955 x\n\
         \                  (#<extends> [#<match> [field, (f (__get__ field x))], x]))))\n\
         \   K: (\955 x (vau {} x))\n\
         \   (@(x: K 3) {x: 1, y: 2});"
    test "do \955: __lambda__\n\
         \   __modify__: (vau [{}, field] (\955 f (\955 x\n\
         \                  (#<extends> [#<match> [field, (f (__get__ field x))], x]))))\n\
         \   K: (\955 x (vau {} x))\n\
         \   [{x: 1, y: 2} :x, {x: 1, y: 2} :(x: K 6)];"
    test "do \955: __lambda__\n\
         \   +: (\955 x (\955 y (#<add> [x, y])))\n\
         \   ++: (+ 1)\n\
         \   [3 .+ 4, 8 .++, ++ 8];"
    test "[#<get> [{x: 3}, `x], {x: 3} :x]"

    test "do (#<writeByte> [#<stdout>, 67]) (#<writeByte> [#<stdout>, 10]);"
    test "do fp: (#<openFile> [\"spotcheck.hs\", `rw])\n\
         \   c: (#<readByte> fp)\n\
         \   (#<close> fp)\n\
         \   (#<handle> [#<IOError>, (vau {} c), `(#<readByte> fp)]);"

    test "[#<typeof> 3, #<typeof> \"blah\", #<typeof> {}, #<typeof> (vau {} {})]"
    test "do [t, c, d]: (#<mkAbstype> \"Foo\")\n\
         \   foo: (c 431)\n\
         \   [#<typeof> foo, d foo];"
    test "do 3: 3;"
    test "['a', 'λ', ''']"
    
    test "do x: (#<new> 4)\n\
         \   (#<assign> [x, (#<add> [#<deref> x, 5])])\n\
         \   (#<deref> x);"
    test "do ar1: (#<newArr> [[1, 2, 3]])\n\
         \   ar2: (#<newArr> [10, 5])\n\
         \   (#<assignIx> [ar1, 2, 5])\n\
         \   (#<assignIx> [ar2, 9, 3])\n\
         \   [ #<bounds> ar1, #<index> [ar1, 1], #<index> [ar1, 2]\n\
         \   , #<bounds> ar2, #<index> [ar2, 0], #<index> [ar2, 9]];"



lambda = "(vau [{}, var] (vau [static, ast] (vau arg (#<eval> [#<extends> [#<match> [var, #<eval> arg], static], ast ]))))"
letin  = "(vau [{}, x] (vau val (vau [e, body] (#<eval> [#<extends> [#<match> [x, #<eval> val], e], body]))))"



--TODO
--dot-infixing as normal
--colon-infixing also desugars into a getter

testParse input = do
    putStrLn input
    case Syx.parseOctopusExpr "" input of
        Right val -> print val
        Left err -> print err >> exitFailure
test input = do
    putStr $ input ++ " ===> "
    cache <- newMVar Map.empty
    case parseOctopusExpr "repl" input of
        Right val -> print =<< eval (MConfig { libdir = "lib/", importsCache = cache, thisFile = Nothing }) startData val
        Left err -> print err >> exitFailure


startData = mkXn [
    --- Syntax ---
      (intern "__get__",      getDef)
    , (intern "__let__",      letDef)
    , (intern "__quote__",    quoteDef)
    --- Niceties --- TODO translate into Octopus libraries
    , (intern "vau",          vauDef)
    , (intern "__lambda__",   lambdaDef)
    ]

vauDef = Cl
    (mkSq [mkXn [], mkSy "x"])
    (mkCall (Pr Vau) (mkSq [mkSq [mkSy "static", mkSy "body"],
        mkCall (Pr Vau) (mkSq [mkSy "arg",
            mkCall (Pr Eval) (mkSq [
                mkCall (Pr Extends) (mkSq [
                    mkCall (Pr Match) (mkSq [mkSy "x", mkSy "arg"]),
                    mkSy "static"]),
                mkSy "body"])])]))
    (mkXn [])

getDef = Cl
    (mkSq [mkXn [], mkSy "x"])
    (mkCall (Pr Vau) (mkSq [mkSy "xn", 
        mkCall (Pr Get) (mkSq [mkCall (Pr Eval) (mkSy "xn"), mkSy "x"])]))
    (mkXn [])

lambdaDef = Cl
    (mkSq [mkXn [], mkSy "var"])
    (mkCall (Pr Vau) (mkSq [mkSq [mkSy "static", mkSy "ast"],
        mkCall (Pr Vau) (mkSq [mkSy "arg",
            mkCall (Pr Eval) (mkSq
                [ mkCall (Pr Extends) (mkSq
                    [ mkCall (Pr Match) (mkSq [mkSy "var", mkCall (Pr Eval) (mkSy "arg")])
                    , mkSy "static"])
                , mkSy "ast"])])]))
    (mkXn [])

quoteDef = Cl (mkSq [mkXn [], mkSy "ast"]) (mkSy "ast") (mkXn [])
