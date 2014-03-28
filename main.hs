import System.IO
import System.Environment
import System.Exit

import Octopus

main :: IO ()
main = do
    print =<< runMachine noevalData
    print =<< runMachine callData
    print =<< runMachine getenvProg
    print =<< runMachine quoteProg
    print =<< runMachine fiveProg
