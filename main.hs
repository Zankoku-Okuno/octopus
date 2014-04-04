import System.IO
import System.Environment
import System.Exit
import Control.Applicative
import Control.Concurrent.MVar

import Octopus
import Octopus.Parser
import Octopus.Libraries

import qualified Data.Map as Map

main :: IO ()
main = do
    --parse_e <- parseOctopusFile "foo.oct" <$> readFile "test/foo.oct"
    --case parse_e of
    --    Left err -> print err
    --    Right val -> do
    --        print val
    --        print =<< eval startData val
    --exitSuccess

    test "#<<END\na\\s;dg\nasdg\nEND>>"
    test "do four: 4 ((vau x x) four);"
    test "do four: 4 ((vau x (#<eval> x)) four);"
    test "((vau [e, ast] e) dne)"
    test "((vau [e, ast] ast) dne)"
    test "(#<extends> [{a: 3}, {a: 2}, {a: 1}])"
    test "((vau [e, ast] (#<eval> [#<extends> [{five: 5}, e], ast])) five)"
    test "do four: 4 (vau [{}, ob] (#<keys> ob) {foo: 3, bar: four});"
    test "do four: 4 ((vau [e, ob] (:x ob)) {x: four});"
    test "do four: 4 ((vau [e, ob] (#<eval> [e, :x ob])) {x: four});"
    test $ "do four: 4 (" ++ lambda ++ " ob (:x ob) {x: four});"
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
         \   delete: (\955 ob (vau [{}, field] (#<del> [ob, field])))\n\
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

    test "do {\955: \955}: (#<import> \"./test/foo.oct\") (\955 x x 6);"

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
    test "(#<handle> [TypeError, (__lambda__ x x), `(#<add> 3 4)])"
    test "(#<handle> [TypeError, (__lambda__ x x), `(#<add> [3, 4])])"
    test "(#<handle> [TypeError, (__lambda__ x x), `(#<add> [3, 5, 4])])"
    test "(#<handle> [DivZero, (vau {} 1), `do (#<div> [3, 0]) (#<raise> []);])"



lambda = "(vau [{}, var] (vau [static, ast] (vau arg (#<eval> [#<extends> [#<match> [var, #<eval> arg], static], ast ]))))"
letin  = "(vau [{}, x] (vau val (vau [e, body] (#<eval> [#<extends> [#<match> [x, #<eval> val], e], body]))))"



--TODO
--dot-infixing as normal
--colon-infixing also desugars into a getter

testParse input = case parseOctopusExpr "" input of
    Right val -> print val
    Left err -> print err >> exitFailure
test input = do
    putStr $ input ++ " ===> "
    cache <- newMVar Map.empty
    case parseOctopusExpr "repl" input of
        Right val -> print =<< eval cache startData val
        Left err -> print err