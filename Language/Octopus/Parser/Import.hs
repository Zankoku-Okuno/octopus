module Language.Octopus.Parser.Import (
      module X
    , Parser, startState, Stack
    , topIndent, isImplicit
    , startImplicit, startExplicit
    , endImplicit, endExplicit
    , SourcePos, SourceName, ParseError
    , char, anyChar, oneOf, noneOf, eof
    , try, (<?>), unexpected
    )
    where

import Import as X
import Language.Parse as X hiding ((<|>))
import Text.Parsec hiding ((<|>))


--TODO ReaderT [(String, Val)]
type Parser = Parsec String Stack

startState :: Stack
startState = [Bottom]

type Stack = [StackElem]
data StackElem = Implicit Int
               | Explicit Int
               | Bottom


topIndent :: Parser (Maybe Int)
topIndent = do
    top <- head <$> getState
    return $ case top of
        Implicit n -> Just n
        Explicit n -> Just n
        Bottom -> Nothing

isImplicit :: Parser Bool
isImplicit = do
    top <- head <$> getState
    return $ case top of
        Implicit _ -> True
        _ -> False

startImplicit :: Parser ()
startImplicit = do
    col <- sourceColumn <$> getPosition
    modifyState (Implicit col :)

startExplicit :: Parser ()
startExplicit = do
    col <- sourceColumn <$> getPosition
    modifyState (Explicit col :)

endImplicit :: Int -> Parser ()
endImplicit n = do
    top <- head <$> getState
    case top of
        Implicit n' -> if n < n' then return () else fail ""
        Explicit n' -> fail ""
        Bottom -> fail ""

endExplicit :: Parser ()
endExplicit = do
    top <- head <$> getState
    case top of
        Explicit _ -> modifyState tail
        -- Close Explicits can match open Explicits before dedents match their Implicits
        Implicit _ -> do
            (upper, lower) <- break isExplicit <$> getState
            case lower of
                [] -> fail ""
                (_:lower') -> putState $ upper ++ lower'
        Bottom -> fail ""
    where
    isExplicit (Explicit _) = True
    isExplicit _ = False


