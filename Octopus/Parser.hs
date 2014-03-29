module Octopus.Parser where

import Import

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Text.Parsec ( Parsec, SourceName, ParseError
                   , try, (<?>), parserZero
                   , char, anyChar, eof)
import qualified Text.Parsec as P
import Language.Parse

import Octopus.Data
import Octopus.Basis

type Parser = Parsec String ()

parseOctopus :: SourceName -> String -> Either ParseError Val
parseOctopus sourceName input = P.runParser octopusFile () sourceName input
    where
    octopusFile = expr --STUB

expr :: Parser Val
expr = atom P.<|> composite
    where
    atom = P.choice [symbol, numberLit, textLit, heredoc, accessor] <?> "atom"
    composite = P.choice [combine, sq, ob]

------ Atoms ------
symbol :: Parser Val
symbol = Sy . intern <$> name

numberLit :: Parser Val
numberLit = Nm <$> anyNumber

textLit :: Parser Val
textLit = do
    content <- catMaybes <$> between2 (char '\"') (P.many maybeLiteralChar)
    return $ mkTxt content
mkBytes = By . encodeUtf8 . T.pack
mkTxt = Tx . T.pack

heredoc :: Parser Val
heredoc = do
    string "#<<"
    --grab until end of line, strip whitespace, save as `end`
    --anyChar `manyThru` end
    parserZero

--TODO maybe bytes literals

accessor :: Parser Val
accessor = do
    key <- char ':' *> name
    return $ mkCombination (mkSym "__get__") (mkSym key)


------ Composites ------
combine :: Parser Val
combine = do
    postPadded $ char '('
    e <- bareCombination
    padded $ char ')'
    return e

sq :: Parser Val
sq = do
    postPadded $ char '['
    elems <- bareCombination `P.sepBy` padded comma
    padded $ char ']'
    return $ mkSeq elems

ob :: Parser Val
ob = do
    postPadded $ char '{'
    elems <- pair `P.sepBy` padded comma
    padded $ char '}'
    return $ mkObj elems
    where
    pair = do
        key <- intern <$> padded name
        char ':' <* whitespace
        val <- bareCombination
        return (key, val)


------ Space ------
whitespace :: Parser ()
whitespace = (<?> "space") . P.skipMany1 $ P.choice [spaces1, lineComment, blockComment]

lineComment :: Parser ()
lineComment = void $ do
    try $ char '#' >> P.notFollowedBy (string ">>")
    anyChar `manyThru` (void (char '\n') P.<|> eof)

blockComment :: Parser ()
blockComment = parserZero

padded :: Parser a -> Parser a
padded p = try $ P.optional whitespace >> p
postPadded :: Parser a -> Parser a
postPadded p = p <* P.optional whitespace


------ Helpers ------
name :: Parser String
name = many2
    (blacklistChar (`elem` reservedFirstChar))
    (blacklistChar (`elem` reservedChar))
    where
    reservedChar = "#\\\"\'()[]{}:,." --TODO quasiquote sugar (`,~) and comma requires a space afterwards
    reservedFirstChar = reservedChar ++ "-0123456789"

comma :: Parser ()
comma = char ',' >> whitespace

bareCombination :: Parser Val
bareCombination = loop <$> P.many1 (postPadded expr)
    where
    loop [e] = e
    loop [f, x] = mkCombination f x
    loop es = mkCombination (loop $ init es) (last es)




