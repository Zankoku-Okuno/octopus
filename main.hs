import Import
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Directory
import System.Exit
import Control.Concurrent.MVar

import qualified Data.Map as Map

import Language.Octopus
import Language.Octopus.Data
import Language.Octopus.Parser
import Language.Octopus.Libraries
import Language.Octopus.Primitive (resolveSymbol)

import Data.Version
import Paths_octopus


main :: IO ()
main = do
    (opts, filepath) <- getOptions
    filepath' <- canonicalizePath filepath
    cache <- newMVar Map.empty
    lib <- getDataFileName "lib/"
    parse_e <- parseOctopusFile filepath <$> readFile filepath
    let startConfig = MConfig { libdir = lib, importsCache = cache, thisFile = Just filepath' }
    case parse_e of
        Left err -> print err
        Right (_, val) -> do
            fileEnv <- eval startConfig initialEnv val
            case resolveSymbol (intern "main") fileEnv of
                Nothing -> return ()
                Just main -> print =<< eval startConfig fileEnv main
    exitSuccess


data Options = Options  { 
                        } deriving (Show)
startOptions = Options  { 
                        }

getOptions :: IO (Options, FilePath)
getOptions = do
    (actions, args, errors) <- getOpt Permute options <$> getArgs
    if errors == []
      then do
        opts <- foldl (>>=) (return startOptions) actions
        case args of
            [] -> putErrLn "No file supplied." >> exitFailure
            [file] -> return (opts, file)
            _ -> putErrLn "Too many files supplied." >> exitFailure
      else mapM putErrLn errors >> exitFailure

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "V" ["version"]
        (NoArg
            (\_ -> do
                putErrLn $ "Octi " ++ showVersion version
                exitSuccess))
        "Print version"
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                putErrLn "Octopus interpreter."
                putErrLn "This program is lisenced under the GPLv3."
                putErrLn (usageInfo (prg ++ " [options] file") options)
                putErrLn "Interpret the given file of Octopus code."
                exitSuccess))
        "Show help"
    , Option "" ["print-libdir"]
        (NoArg
            (\_ -> do
                putErrLn =<< getDataFileName ""
                exitSuccess))
        "Print directory where the system's Octopus library files are searched."
    ]

putErrLn = hPutStrLn stderr