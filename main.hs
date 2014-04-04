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

    test "do four: 4 ((vau x x) four);"
    test "do four: 4 ((vau x (__eval__ x)) four);"
    test "((vau [e, ast] e) dne)"
    test "((vau [e, ast] ast) dne)"
    test "(__extends__ [{a: 3}, {a: 2}, {a: 1}])"
    test "((vau [e, ast] (__eval__ [__extends__ [{five: 5}, e], ast])) five)"
    test "do four: 4 (vau [{}, ob] (__keys__ ob) {foo: 3, bar: four});"
    test "do four: 4 ((vau [e, ob] (:x ob)) {x: four});"
    test "do four: 4 ((vau [e, ob] (__eval__ [e, :x ob])) {x: four});"
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
    test "(__del__ [{x: 1, y: 2}, `y])"
    test "do \955: __lambda__\n\
         \   delete: (\955 ob (vau [{}, field] (__del__ [ob, field])))\n\
         \   (delete {x: 1, y: 2} x);"
    test "do x: do \955: __lambda__\n\
         \         (\955 y y 3);\n\
         \      x;"
    test "do defx: (__let__ x)\n\
         \   (defx 5 x);"
    test "do defx5: (__let__ x 5)\n\
         \   (defx5 x);"
    test "do [a, b, c]: [`there, `Bob, `hi] [c, a, b];"

    test "do x: (__add__ [1, 2])\n   y: (__sub__ [1, 2])\n   z: (__mul__ [2, 3])\n   w: (__div__ [2, 3])\n   [x, y, z, w];"
    test "do [a, b]: (__cut__ [[1, 2, 3, 4, 5], 3])\n   [a, b, __len__ a];"
    test "do [a, b]: (__cut__ [\"hello\", 3])\n   [a, b, __len__ a];"
    test "do y: (__ifz!__ [0, `y, `n])\n   n: (__ifz!__ [1, `y, `n])\n   [y, n];"

    test "do {\955: \955}: (__import__ \"./test/foo.oct\") (\955 x x 6);"

    test "do t: (__mkTag__ \"phoey\")\n   (__handle__ [t, 3, `5]);"
    test "do id: (__lambda__ x x)\n   t: (__mkTag__ \"phoey\")\n   (__handle__ [t, id,\n      `(__add__ [3, (__raise__ [t, 7])])]);"
    test "do \955: __lambda__\n\
         \   id: (\955 x x)\n\
         \   +: (\955 x (\955 y (__add__ [x, y])))\n\
         \   ex: (__mkTag__ \"exn\")\n\
         \   handle: (\955 tag (\955 handler (vau thunk\n\
         \              (__handle__ [tag, handler, `(__eval__ thunk)]))))\n\
         \   [ handle ex (+ 3) (+ 1 6)\n\
         \   , handle ex (+ 3) (+ 1 (__raise__ [ex, 10]))];"
    test "(__handle__ [TypeError, (__lambda__ x x), `(__add__ 3 4)])"
    test "(__handle__ [TypeError, (__lambda__ x x), `(__add__ [3, 4])])"
    test "(__handle__ [TypeError, (__lambda__ x x), `(__add__ [3, 5, 4])])"
    test "(__handle__ [DivZero, (vau {} 1), `do (__div__ [3, 0]) (__raise__ []);])"

    test "(__import__ \"dne.oct\")"


lambda = "(vau [{}, var] (vau [static, ast] (vau arg (__eval__ [__extends__ [__match__ [var, __eval__ arg], static], ast ]))))"
letin  = "(vau [{}, x] (vau val (vau [e, body] (__eval__ [__extends__ [__match__ [x, __eval__ val], e], body]))))"



--TODO
--dot-infixing as normal
--colon-infixing also desugars into a getter

testParse input = case parseOctopusExpr "" input of
    Right val -> print val
    Left err -> print err
test input = do
    putStr $ input ++ " ===> "
    cache <- newMVar Map.empty
    case parseOctopusExpr "repl" input of
        Right val -> print =<< eval cache startData val
        Left err -> print err