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
    print =<< eval startData noevalData
    print =<< eval startData callData
    print =<< eval startData getenvProg
    print =<< eval startData quoteProg
    print =<< eval startData fiveProg

testParse input = case parseOctopus "" input of
    Right val -> print val
    Left err -> print err