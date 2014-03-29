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
    test "((vau [e, ast] (force [extends [{five: 5}, e], ast])) five)"
    test "(vau [_, ob] (keys ob) {foo: 3, bar: four})"
    test "(vau [_, ob] (delete ob x) {x: 2, y: 3})"
    test "((vau [e, ob] (:x ob)) {x: four})"
    --TODO figure out lambda so I can do "((lambda x (:x x)) {x: four})" and get the number 4 back


testParse input = case parseOctopus "" input of
    Right val -> print val
    Left err -> print err
test input = case parseOctopus "repl" input of
    Right val -> print =<< eval startData val
    Left err -> print err