import System.IO
import System.Environment
import System.Exit

import Octopus
import Octopus.Parser

main :: IO ()
main = do
    --testParse "30/4"
    --testParse "symbol"
    --testParse "[ 1 , #\n2, 3  ]#"
    --testParse "{four: 4, five: neg 5}"
    --testParse "(vau x (force x))"
    --testParse "\"hello world\995\""
    test "((vau x x) four)"
    test "((vau x (force x)) four)"
    test "((vau [e, ast] e) dne)"
    test "((vau [e, ast] ast) dne)"
    test "(__extends__ [{a: 3}, {a: 2}, {a: 1}])"
    test "((vau [e, ast] (force [__extends__ [{five: 5}, e], ast])) five)"
    test "(vau [{}, ob] (keys ob) {foo: 3, bar: four})"
    test "(vau [{}, ob] (delete ob x) {x: 2, y: 3})"
    test "((vau [e, ob] (:x ob)) {x: four})"
    test "((vau [e, ob] (force [e, :x ob])) {x: four})"
    test $ "(" ++ lambda ++ " ob (:x ob) {x: four})"
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

lambda = "(vau [{}, var] (vau [static, ast] (vau arg (force [__extends__ [__match__ [var, force arg], static], ast ]))))"
letin  = "(vau [{}, x] (vau val (vau [e, body] (force [__extends__ [__match__ [x, force val], e], body]))))"



--TODO
--dot-infixing as normal
--colon-infixing also desugars into a getter

testParse input = case parseOctopus "" input of
    Right val -> print val
    Left err -> print err
test input = do
    putStr $ input ++ " ===> "
    case parseOctopus "repl" input of
        Right val -> print =<< eval startData val
        Left err -> print err