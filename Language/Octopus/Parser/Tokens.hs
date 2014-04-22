module Language.Octopus.Parser.Tokens where

import qualified Text.Parsec as P

import Language.Octopus.Data
import Language.Octopus.Data.Shortcut
import Language.Octopus.Parser.Import
import Language.Octopus.Parser.Whitespace
import Language.Octopus.Parser.Policy


------ Atoms ------
atom :: Parser Val
atom = P.choice [ numberLit, charLit
                , textLit, bytesLiteral, rawTextLit, heredoc
                , symbol
                , builtin
                ]

symbol :: Parser Val
symbol = try $ do
    n <- name
    when (n `elem` reservedWords)
        (unexpected $ "reserved word (" ++ n ++ ")") --FIXME report error position before token, not after
    return $ mkSy n

numberLit :: Parser Val
numberLit = Nm <$> anyNumber

charLit :: Parser Val
charLit = mkInt . ord <$> between2 (char '\'') (literalChar P.<|> oneOf "\'\"")

bytesLiteral :: Parser Val
bytesLiteral = do
        string "b\""
        P.optional mws
        content <- P.many1 $ byte <* P.optional mws
        string "\""
        return $ mkBy content
    where
    byte = do
        one <- P.hexDigit
        two <- P.hexDigit
        return . fromIntegral $ stringToInteger 16 [one, two]

textLit :: Parser Val
textLit = do
    content <- catMaybes <$> between2 (char '\"') (P.many maybeLiteralChar)
    return $ mkTx content

rawTextLit :: Parser Val
rawTextLit = do
    content <- P.between (string "r\"") (char '"') $
        P.many (noneOf "\"" P.<|> (const '"' <$> string "\"\""))
    return $ mkTx content

heredoc :: Parser Val
heredoc = do
    string "#<<"
    end <- P.many1 P.letter <* newline
    let endParser = newline *> string (end ++ ">>") <* (newline P.<|> eof)
    mkTx <$> anyChar `manyThru` endParser

builtin :: Parser Val
builtin = do
    table <- getBuiltins
    P.choice (map mkPrimParser table)
    where
    mkPrimParser (name, val) = string ("#<" ++ name ++ ">") >> return val


------ Basic Tokens ------
name :: Parser String
name = P.choice [ (:) <$> namehead <*> nametail
                , (:) <$> char '-' <*> P.option [] ((:) <$> (namehead P.<|> char '-') <*> nametail)
                ]
    where
    namehead = blacklistChar (`elem` reservedFirstChar)
    nametail = P.many $ blacklistChar (`elem` reservedChar)


------ Separators ------
openParen :: Parser ()
openParen =  char '(' >> whenLayout (indentFromPos >>= pushExplicit)

openBracket :: Parser ()
openBracket = char '[' >> whenLayout (indentFromPos >>= pushExplicit)

openBrace :: Parser ()
openBrace = char '{' >> whenLayout (indentFromPos >>= pushExplicit)

closeParen :: Parser ()
closeParen =  char ')' >> whenLayout popExplicit

closeBracket :: Parser ()
closeBracket = char ']' >> whenLayout popExplicit

closeBrace :: Parser ()
closeBrace = char '}' >> whenLayout popExplicit

comma :: Parser ()
comma = do
    try $ char ',' >> mws
    whenLayout $ popExplicit >> indentFromPos >>= pushExplicit