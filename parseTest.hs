import System.IO
import System.Environment
import System.Exit
import Control.Concurrent.MVar

import Import
import Language.Octopus
import Language.Octopus.Parser

import qualified Language.Octopus.Parser.Postprocess as Syx

import qualified Data.Map as Map

main :: IO ()
main = do
    test "a :: foo\nexport a;\na;\n3;"
    test "\n  #qega\n b\"af \n 3637\";"
    test "(a b \n   c d\n);"
    test "do a b\n      c d;;"
    test "do a b\n      c d;\n   e f;;"
    test "[1, 2 + 3\n,\n  f x];"
    test "{a: 1, b: 2,\n c: (3\n      4)\n};"
    test "do 3;\n   f: lambda\n      4;;"
    test "do open #<import> module;\n   letrec f: f 3;;"
    test "3 .+ 4;"
    test "x :y;"
    test "x :( y: ++);"
    test "`q;"
    test "export {foo, bar};\nfoo: 3;\nbar: 4;"
    test "a: 3;\nb :: Int (a random number)\nb: 4;"
    test "do ;"
    content <- readFile "lib/foo.oct"
    case parseOctopusFile "" content of
        Right val -> print val
        Left err -> print err >> exitFailure
    exitSuccess


test input = do
    putStrLn input
    case parseOctopusFile "" input of
        Right val -> print val
        Left err -> print err >> exitFailure
