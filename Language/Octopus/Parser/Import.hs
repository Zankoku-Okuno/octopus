module Language.Octopus.Parser.Import (
      module X
    , Parser
    , Stack, startState
    , Config(..), defaultConfig
    , onLayout, layoutOnly, whenLayout
    , initIndent, pushImplicit, pushExplicit
    , popImplicit, popExplicit, emptyIndent
    , peekIndent 
    , indentFromPos
    , SourcePos, SourceName, ParseError
    , char, anyChar, oneOf, noneOf, eof
    , try, (<?>), unexpected
    )
    where

import Import as X
import Language.Parse as X hiding ((<|>), space)
import Text.Parsec hiding ((<|>))
import Control.Monad.Reader as X

type Parser = ParsecT String Stack (Reader Config)

data Config = PC { _isLayout :: Bool, _isTabbed :: Bool }

type Stack = [StackElem]
data StackElem = Implicit Int
               | Explicit Int
               | Bottom Int
getNumber (Implicit n) = n
getNumber (Explicit n) = n
getNumber (Bottom n) = n

defaultConfig :: Config
defaultConfig = PC { _isLayout = True, _isTabbed = False }

startState :: Stack
startState = []


onLayout :: Parser a -> Parser a -> Parser a
onLayout y n = do
    layout <- asks _isLayout
    if layout then y else n

layoutOnly :: Parser a -> Parser a
layoutOnly = flip onLayout parserZero

whenLayout :: Parser () -> Parser ()
whenLayout = flip onLayout (return ())


initIndent :: Int -> Parser ()
initIndent n = layoutOnly $ modifyState (Bottom n :)

pushImplicit :: Int -> Parser ()
pushImplicit n = layoutOnly $ modifyState (Implicit n :)

pushExplicit :: Int -> Parser ()
pushExplicit n = layoutOnly $ modifyState (Explicit n :)

popImplicit :: Parser ()
popImplicit = layoutOnly $ do
    top <- head <$> getState
    case top of
        Implicit _ -> modifyState tail
        Explicit _ -> parserZero
        Bottom _ -> parserZero

popExplicit :: Parser ()
popExplicit = layoutOnly $ do
    top <- head <$> getState
    case top of
        Explicit _ -> modifyState tail
        --_ -> fail ""
        -- Close Explicits can match open Explicits before dedents match their Implicits
        Implicit _ -> do
            (upper, lower) <- break isExplicit <$> getState
            case lower of
                [] -> parserZero
                (_:lower') -> putState $ upper ++ lower'
        Bottom _ -> parserZero
    where
    isExplicit (Explicit _) = True
    isExplicit _ = False

emptyIndent :: Parser ()
emptyIndent = layoutOnly $ do
    top <- head <$> getState
    case top of
        Bottom _ -> return ()
        _ -> parserZero

indentFromPos :: Parser Int
indentFromPos = sub1 . sourceColumn <$> getPosition
    where sub1 x = x - 1
    

peekIndent :: Parser Int
peekIndent = layoutOnly $ getNumber . head <$> getState

