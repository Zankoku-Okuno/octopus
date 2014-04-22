module Language.Octopus.Parser.Syntax where

import qualified Text.Parsec as P

import Language.Octopus.Data
import Language.Octopus.Data.Shortcut
import Language.Octopus.Parser.Import
import Language.Octopus.Parser.Whitespace
import Language.Octopus.Parser.Tokens



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
         | Do (Maybe Syx) [Statement Syx]
         | Infix Syx
    deriving (Show)



startFile :: Parser ()
startFile = flip onLayout ws0 $ do
    P.many blankline
    P.optional newline
    c <- (\p -> if p then '\t' else ' ') <$> asks _isTabbed
    initIndent =<< length <$> P.many (char c)

endFile :: Parser ()
endFile = ws0 >> P.many blankline >> eof

statement = P.choice
    [ openStmt
    , letrec
    , define
    , docstring
    , Expr <$> bare
    ]

expr :: Parser Syx
expr = P.choice
    [ Lit <$> atom <?> "atom", sq, xn
    , combine, doExpr
    , quote, dottedExpr, accessor, mutator
    ]

block :: Parser (Maybe Syx, [Statement Syx])
block = do
    api <- P.optionMaybe $ export <* blockSep
    stmts <- (statement <* ws0) `P.sepBy1` blockSep
    return (api, stmts)

blockSep :: Parser ()
blockSep = onLayout nextline (void $ char ';')


------ Expressions ------
sq :: Parser Syx
sq = P.between (openBracket *> ws0) (mws0 <* closeBracket) $
    SqSyx <$> (bare <* mws0) `P.sepBy` comma

xn :: Parser Syx
xn = P.between (openBrace *> ws0) (mws0 <* closeBrace) $
    XnExpr <$> ((pair <|> single) <* mws0) `P.sepBy` comma
    where
    pair = do
        key <- try $ (intern <$> name) <* char ':' <* ws
        val <- bare
        return (key, val)
    single = do
        key <- intern <$> name
        return (key, Lit $ Sy key)


combine :: Parser Syx
combine = onLayout (byIndent <|> byParen) byParen
    where
    byParen = openParen *> ws0 *> bare <* ws0 <* closeParen
    byIndent = indent *> bare <* ws0 <* dedent
    

doExpr :: Parser Syx
doExpr = do
    try (string "do" >> ws)
    whenLayout $ indentFromPos >>= pushImplicit
    (api, stmts) <- block
    onLayout dedent (void $ char ';')
    return $ Do api stmts

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
    string ":(" >> whenLayout (indentFromPos >>= pushExplicit) >> ws0
    key <- name <* char ':'
    ws
    e <- bare
    ws0 >> char ')' >> whenLayout popExplicit
    return . Infix $ Call [Lit $ mkSy "__modify__", Lit $ mkSy key, e]


------ Pseudo-statements ------
bare :: Parser Syx
bare = do
    es <- many2 expr (try $ ws *> expr)
    return $ case es of { [e] -> e; es -> Call es }

export :: Parser Syx
export = do
    try $ string "export" <* ws
    bare

docstring :: Parser (Statement Syx)
docstring = do
        for <- try $ name <* ws <* string "::"
        content <- multiline <|> ws *> oneline
        return $ DocS for content
    where
    multiline = do
        string "<<\n"
        content <- anyChar `manyTill` (string "\n>>" *> (newline <|> eof))
        string "\n>>"
        return content
    oneline = anyChar `manyTill` char '\n'


------ Statements ------
define :: Parser (Statement Syx)
define = do
    --TODO decorators
    var <- try $ expr <* char ':'
    ws
    val <- bare
    return $ Defn [] var val

letrec :: Parser (Statement Syx)
letrec = do
    try $ string "letrec" <* ws
    --TODO mutual letrec
    var <- expr <* char ':' <* ws
    body <- bare
    return $ LRec var body

openStmt :: Parser (Statement Syx)
openStmt = do
    try $ string "open" <* ws
    Open <$> bare

