module Language.Octopus.Parser.Syntax where

import qualified Text.Parsec as P

import Language.Octopus.Data
import Language.Octopus.Data.Shortcut
import Language.Octopus.Parser.Import
import Language.Octopus.Parser.Tokens

--FIXME DELME
parseOctopusExpr :: SourceName -> String -> Either ParseError Syx
parseOctopusExpr sourceName input = P.runParser (startFile *> bareCombination <* endFile) startState sourceName input

startFile :: Parser ()
startFile = P.many blankLine >> P.optional newline
endFile :: Parser ()
endFile = P.many blankLine >> whitespace0 >> eof


data Statement a = Defn [a] a a
                 | LRec a a --FIXME multi-letrec
                 | Open a
                 | Eprt a
                 | Expr a
                 | DocS String String
    deriving (Show)

data Syx = Lit Val
         | Anon
         | Call [Syx]
         | SqSyx [Syx]
         | XnExpr [(Symbol, Syx)]
         | Do [Statement Syx]
         | Infix Syx
    deriving (Show)


statement = P.choice
    [ openStmt
    , letrec
    , define
    , Expr <$> bareCombination
    ]

expr :: Parser Syx
expr = P.choice
    [ Lit <$> atom <?> "atom", sq, xn
    , combine, block
    , quote, dottedExpr, accessor, mutator
    ]


------ Expressions ------
sq :: Parser Syx
sq = P.between (openBracket *> whitespace0) (multiWhitespace0 <* closeBracket) $
    SqSyx <$> (multiWhitespace0 *> bareCombination <* multiWhitespace0) `P.sepBy` comma

xn :: Parser Syx
xn = P.between (openBrace *> whitespace0) (multiWhitespace0 <* closeBrace) $
    XnExpr <$> (multiWhitespace0 *> pair <* multiWhitespace0) `P.sepBy` comma
    where
    pair = do
        key <- intern <$> name
        char ':' <* whitespace
        val <- bareCombination
        return (key, val)


combine :: Parser Syx
combine = open *> whitespace0 *> bareCombination <* whitespace0 <* close

block :: Parser Syx
block = do
    try (string "do" >> whitespace)
    startImplicit
    stmts <- (statement <* whitespace0) `P.sepBy1` nextLine
    whitespace0
    dedent >>= endImplicit
    return $ Do stmts

quote :: Parser Syx
quote = do
    e <- char '`' *> expr
    return $ Call [Lit $ mkSy "__quote__", e]

dottedExpr :: Parser Syx
dottedExpr = Infix <$> (char '.' *> expr)

accessor :: Parser Syx
accessor = do
    key <- try $ char ':' *> name
    return . Infix $ Call [Lit $ mkSy "__get__", Lit $ mkSy key]

mutator :: Parser Syx
mutator = do
    string ":(" >> startExplicit
    whitespace0
    key <- name <* char ':'
    whitespace
    e <- bareCombination 
    char ')' >> endExplicit
    return . Infix $ Call [Lit $ mkSy "__modify__", Lit $ mkSy key, e]


------ Statements ------
define :: Parser (Statement Syx)
define = do
    --TODO decorators
    var <- try $ expr <* char ':'
    whitespace
    val <- bareCombination
    return $ Defn [] var val

letrec :: Parser (Statement Syx)
letrec = do
    try $ string "letrec" <* whitespace
    var <- expr <* char ':' <* whitespace
    body <- bareCombination
    return $ LRec var body

openStmt :: Parser (Statement Syx)
openStmt = do
    try $ string "open" <* whitespace
    Open <$> bareCombination

--TODO export statement


------ Helpers ------
bareCombination :: Parser Syx
bareCombination = do
    es <- many2 expr (try $ buffer *> expr)
    return $ case es of { [e] -> e; es -> Call es }
