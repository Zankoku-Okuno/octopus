module Octopus.Parser where

import Import

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Text.Parsec ( Parsec, SourceName, ParseError
                   , try, (<?>), unexpected, parserZero
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

define :: Parser (String, Val)
define = do
    var <- try (name <* char ':' <* whitespace)
    body <- expr
    return (var, body)

expr :: Parser Val
expr = composite P.<|> atom
    where
    atom = P.choice [symbol, numberLit, textLit, heredoc, accessor] <?> "atom"
    composite = P.choice [block, combine, sq, ob]

statement :: Parser (Either (String, Val) Val)
statement = (Left <$> define) P.<|> (Right <$> expr)


------ Atoms ------
symbol :: Parser Val
symbol = do
    n <- name
    when (n == "do") (unexpected "reserved word (do)") --FIXME report error position before token, not after
    return $ mkSym n

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
    parserZero --TODO

--TODO maybe bytes literals

accessor :: Parser Val
accessor = do
    key <- char ':' *> name
    return $ mkCombination (mkSym "__get__") (mkSym key)


------ Composites ------
block :: Parser Val
block = do
        try $ string "do" >> whitespace
        states <- P.many1 $ postPadded statement
        char ';'
        return $ loop states
    where
    loop [Left s] = mkCombination (mkDefn s) (mkObj [])
    loop [Right e] = e
    loop (Left s:rest) = mkCombination (mkDefn s) (loop rest)
    loop (Right e:rest) = mkCombination (mkExpr e) (loop rest)
    mkDefn (x, val) = mkCombination (mkCombination (mkSym "__let__") (mkSym x)) val
    mkExpr e = mkCombination (mkCombination (mkSym "__let__") (mkObj [])) e

combine :: Parser Val
combine = do
    postPadded $ char '('
    e <- bareCombination
    padded $ char ')'
    return e

--TODO quotation, quasiquotation

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
    reservedChar = "#\\\"\'()[]{}:;.," --TODO quasiquote sugar (`,~) and comma requires a space afterwards
    reservedFirstChar = reservedChar ++ "-0123456789"

comma :: Parser ()
comma = char ',' >> whitespace

bareCombination :: Parser Val
bareCombination = loop <$> P.many1 (postPadded expr)
    where
    loop [e] = e
    loop [f, x] = mkCombination f x
    loop es = mkCombination (loop $ init es) (last es)




