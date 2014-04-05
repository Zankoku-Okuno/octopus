import Import
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import Control.Concurrent.MVar

import qualified Data.Map as Map

import Octopus
import Octopus.Parser
import Octopus.Libraries
import Octopus.Primitive (resolveSymbol)
import Octopus.Shortcut (mkOb)

progVersion = "v0.0.1a"
main :: IO ()
main = do
    (opts, filepath) <- getOptions
    cache <- newMVar Map.empty
    parse_e <- parseOctopusFile filepath <$> readFile filepath
    case parse_e of
        Left err -> print err
        Right (_, val) -> do
            fileEnv <- eval cache initialEnv val
            case resolveSymbol (intern "main") fileEnv of
                Nothing -> return ()
                Just main -> print =<< eval cache fileEnv main
    exitSuccess


data Options = Options  { 
                        } deriving (Show)
startOptions = Options  { 
                        }

getOptions :: IO (Options, FilePath)
getOptions = do
    (actions, args, errors) <- getOpt Permute options <$> getArgs
    if errors == []
      then case args of
        [] -> putErrLn "No file supplied." >> exitFailure
        [file] -> do
            opts <- foldl (>>=) (return startOptions) actions
            return (opts, file)
        _ -> putErrLn "Too many files supplied." >> exitFailure
      else mapM putErrLn errors >> exitFailure

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr progVersion
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
    ]

putErrLn = hPutStrLn stderr