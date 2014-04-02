{-| Parse Octopus source code. The Octopus grammar is:

@
file ::= (\<expr\> | \<defn\>)*
defn ::= _field_ \<expr\>
expr ::= \<atom\> | \<list\> | \<object\>
      | \<combination\> | \<block\> | \<quotation\>
      |  '(' \<expr\> ')'

atom ::= _symbol_ | _number_ | _string_ | _heredoc_ | _accessor_
list ::= '[' (\<expr\>+ (',' \<expr\>+)*)? ']'
object ::= '{' (_field_ \<expr\>+ (',' _field_ \<expr\>+)*)? '}'
combination ::= '(' \<expr\> \<expr\>+ ')'
block ::= 'do' (\<expr\> | \<defn\>)+ ';'
quotation ::= '`' \<expr\>

symbol ::= \/\<name\>\/ - reserved
    reserved = {'do'}
field ::= \/\<name\>:\/
accessor ::= \/:\<name\>\/
number ::= \/[+-]?(0[xX]\<hexnum\>|0[oO]\<octnum\>|0[bB]\<binnum\>|\<decnum\>)\/
    decnum ::= \/\\d+(\\.\\d+\<exponent\>?|\\\/\\d+)?\/
    hexnum ::= \/\\x+(\\.\\x+([hH][+-]?\\x+)?|\\\/\\x+)?\/
    octnum ::= \/[0-7]+(\\.[0-7]+\<exponent\>?|\\\/[0-7]+)?\/
    binnum ::= \/[01]+(\\.[01]+\<exponent\>?|\\\/[01]+)?\/
    exponent ::= \/[eE][+-]?\\d+|[hH][+-]?\\x+\/
string ::= \/\"([^\\\\\"]|\\\\[abefntv\'\"\\\\&]|\\\\\<numescape\>|\\\\\\s*\\n\\s*\\\\)*\"\/
    numescape ::= \/[oO][0-7]{3}|[xX][0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U0[0-9a-fA-F]{5}|U10[0-9a-fA-F]{4}\/
heredoc ::= \/\#\<\<\\s*(?\'END\'\\w+)\\s*\\n.*?\\g{END}\/
name ::= \/[^\#\\\\\"`()[]{}:;.,][^\#\\\\\"`()[]{}:;.,]*\/

linecomment ::= \/#(?!\>\>)\\.*?\\n\/
blockcomment ::= \/\#\\{([^\#}]+|\<blockcomment\>|\#[^{]|\\}[^\#])*\\}\#\/
@
-}
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
import Octopus.Shortcut
import Octopus.Basis

type Parser = Parsec String ()

parseOctopusExpr :: SourceName -> String -> Either ParseError Val
parseOctopusExpr sourceName input = P.runParser (padded expr <* padded eof) () sourceName input

parseOctopusFile :: SourceName -> String -> Either ParseError Val
parseOctopusFile sourceName input = P.runParser octopusFile () sourceName input
    where
    octopusFile = do
        es <- P.many $ padded statement
        padded eof
        return $ loop es
    loop [] = mkCall getenv (mkOb [])
    loop (Left s:rest)  = mkCall (mkDefn s) (loop rest)
    loop (Right e:rest) = mkCall (mkExpr e) (loop rest)
    getenv = (mkCall (Pr Vau) (mkSq [mkSq [mkSy "e", mkOb []], mkSy "e"]))


define :: Parser (Val, Val)
define = do
    var <- try (expr <* char ':' <* whitespace)
    body <- expr
    return (var, body)

expr :: Parser Val
expr = composite P.<|> atom
    where
    atom = P.choice [symbol, numberLit, textLit, heredoc, accessor] <?> "atom"
    composite = P.choice [block, combine, sq, ob, quote]

statement :: Parser (Either (Val, Val) Val)
statement = (Left <$> define) P.<|> (Right <$> expr)


------ Atoms ------
symbol :: Parser Val
symbol = do
    n <- name
    when (n == "do") (unexpected "reserved word (do)") --FIXME report error position before token, not after
    return $ mkSy n

numberLit :: Parser Val
numberLit = Nm <$> anyNumber

--TODO maybe bytes literals

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

accessor :: Parser Val
accessor = do
    key <- char ':' *> name
    return $ mkCall (mkSy "__get__") (mkSy key)


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
    return $ mkSq elems

ob :: Parser Val
ob = do
    postPadded $ char '{'
    elems <- pair `P.sepBy` padded comma
    padded $ char '}'
    return $ mkOb elems
    where
    pair = do
        key <- intern <$> padded name
        char ':' <* whitespace
        val <- bareCombination
        return (key, val)

block :: Parser Val
block = do
        try $ string "do" >> whitespace
        states <- P.many1 $ postPadded statement
        char ';'
        return $ loop states
    where
    loop [Left d] = mkCall (mkDefn d) (mkOb [])
    loop [Right e] = e
    loop (Left d:rest) = mkCall (mkDefn d) (loop rest)
    loop (Right e:rest) = mkCall (mkExpr e) (loop rest)

quote :: Parser Val
quote = do
    char '`'
    e <- expr
    return $ mkCall (mkSy "__quote__") e


------ Space ------
whitespace :: Parser ()
whitespace = (<?> "space") . P.skipMany1 $ P.choice [spaces1, lineComment, blockComment]

lineComment :: Parser ()
lineComment = void $ do
    try $ char '#' >> P.notFollowedBy (string "<<")
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
    reservedChar = "#\\\"`()[]{}:;.,"
    reservedFirstChar = reservedChar ++ "-0123456789"

comma :: Parser ()
comma = char ',' >> whitespace

bareCombination :: Parser Val
bareCombination = loop <$> P.many1 (postPadded expr)
    where
    loop [e] = e
    loop [f, x] = mkCall f x
    loop es = mkCall (loop $ init es) (last es)

mkDefn (x, val) = mkCall (mkCall (mkSy "__let__") x) val
mkExpr e = mkCall (mkCall (mkSy "__let__") (mkOb [])) e



