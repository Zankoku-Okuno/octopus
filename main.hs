import System.IO
import System.Environment
import System.Exit

import Octopus

main :: IO ()
main = do
    print =<< runMachine blahData
