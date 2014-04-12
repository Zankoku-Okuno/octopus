module Language.Octopus.Parser.Tokens where

import qualified Text.Parsec as P

import Language.Octopus.Data
import Language.Octopus.Data.Shortcut
import Language.Octopus.Parser.Import
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
        whitespace0
        content <- P.many1 $ byte <* P.optional multiWhitespace
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


------ Whitespace ------
--FIXME REFAC inlineSpace and newline to Language.Parse
inlineSpace = void $ oneOf " \t" --FIXME more possibilities
newline = void $ oneOf "\n" --FIXME any unicode versions

{-| Consume a line comment, but not the newline after. -}
lineComment :: Parser ()
lineComment = void $ do
    try $ char '#' >> P.notFollowedBy (oneOf "<{")
    anyChar `manyTill` (newline P.<|> eof)

blockComment :: Parser ()
blockComment = void $ do P.parserZero

whitespace :: Parser ()
whitespace = do
    P.skipMany1 $ P.choice [
          P.skipMany1 inlineSpace
        , lineComment
        , blockComment
        ]

whitespace0 :: Parser ()
whitespace0 = P.optional whitespace

{-| Consume a blank line
    (starts with newline then whitespace, with nothing else before the next line).
-}
blankLine :: Parser ()
blankLine = try $ do
    newline
    whitespace0
    P.lookAhead (void newline <|> eof)

blankLines :: Parser ()
blankLines = P.skipMany blankLine

multiWhitespace :: Parser ()
multiWhitespace = P.skipMany1 (whitespace P.<|> newline)

multiWhitespace0 :: Parser ()
multiWhitespace0 = P.optional multiWhitespace

buffer :: Parser ()
buffer = whitespace <|> lookAhead newline <|> eof

------ Indentation ------
nextLine :: Parser ()
nextLine = try $ do
    n <- leadingSpaces
    n' <- topIndent >>= return . fromMaybe 0
    if n == n'
        then return ()
        else fail $ if n > n' then "too much indent" else "too little indent"

indent :: Parser ()
indent = try $ do
    n <- leadingSpaces
    n' <- topIndent >>= return . fromMaybe 0
    if n > n'
        then return ()
        else fail $ "not indented far enough (" ++ show n ++ " <= " ++ show n' ++ ")"

dedent :: Parser Int
dedent = try $ do
    n <- lookAhead leadingSpaces <|> (eof >> return 0)
    n' <- topIndent >>= return . fromMaybe 0
    if n < n'
        then return n
        else fail $ "not dedented far enough (" ++ show n ++ " >= " ++ show n' ++ ")"

leadingSpaces :: Parser Int
leadingSpaces = try $ (+1) . length <$> (newline >> P.many (char ' '))


------ Separators ------
open :: Parser ()
open =  try (char '(' >> startExplicit)
    <|> try (indent >> startImplicit)

close :: Parser ()
close =  try (char ')' >> endExplicit)
     <|> try (dedent >>= endImplicit)

openBracket :: Parser ()
openBracket = try $ char '[' >> startExplicit

closeBracket :: Parser ()
closeBracket = try $ char ']' >> endExplicit

openBrace :: Parser ()
openBrace = try $ char '{' >> startExplicit

closeBrace :: Parser ()
closeBrace = try $ char '}' >> endExplicit

comma :: Parser ()
comma = do
    try $ char ',' >> buffer
    endExplicit
    startExplicit