module Language.Octopus.Parser.Whitespace (
      newline
    , ws, ws0, mws, mws0, blankline
    , indent, nextline, dedent
    ) where

import Language.Octopus.Parser.Import
import qualified Text.Parsec as P

space :: Parser ()
space = void $ onLayout (oneOf " \t") (oneOf " \t\n") --FIXME unicode whitespace

newline :: Parser ()
newline = void $ char '\n'

-- | > lineComment ::= /#(?![<{])[^\n]/
lineComment :: Parser ()
lineComment = void $ do
    try $ char '#' >> P.notFollowedBy (oneOf "<{")
    anyChar `manyTill` (newline P.<|> eof)

-- | > blockComment ::= '#{' (/[^}#]|}(?!#)|#(?!\{)/ | <blockcomment>)* '}#'
blockComment :: Parser ()
blockComment = void $ do 
        string "#{"
        P.skipMany interior
        string "}#"
    where
    interior = P.choice
        [ P.skipMany1 $ noneOf "}#"
        , try $ char '}' >> P.notFollowedBy (char '#')
        , try $ char '#' >> P.notFollowedBy (char '{')
        , blockComment
        ]

-- | > ws ::= (<space> | <lineComment> | <blockComment>)+
ws :: Parser ()
ws = P.skipMany1 $ P.choice
    [ P.skipMany1 space
    , lineComment
    , blockComment
    ]

ws0 :: Parser ()
ws0 = P.optional ws

mws :: Parser ()
mws = P.skipMany1 $ P.choice
    [ P.skipMany1 $ oneOf " \t\n"
    , lineComment
    , blockComment
    ]

mws0 :: Parser ()
mws0 = P.optional mws

-- | > /\n<ws>?(?=\n|$)/
blankline :: Parser ()
blankline = try $ do
    newline *> ws0
    P.lookAhead (void newline <|> eof)


------ Indentation ------
leadingSpaces :: Parser Int
leadingSpaces = layoutOnly . try $ do
    P.skipMany blankline
    char '\n'
    c <- (\p -> if p then '\t' else ' ') <$> asks _isTabbed
    length <$> P.many (char c)

-- | > indent ::= <leadingSpaces> (> PEEK) PUSH
indent :: Parser ()
indent = layoutOnly . try $ do
    n <- leadingSpaces
    n' <- peekIndent
    if n > n'
        then pushImplicit (n - 1)
        else fail $ "not indented far enough (" ++ show n ++ " <= " ++ show n' ++ ")"

-- | > nextline ::= <leadingSpaces> (== PEEK)
nextline :: Parser ()
nextline = layoutOnly . try $ do
    n <- leadingSpaces
    n' <- peekIndent
    if n == n'
        then return ()
        else fail $ if n > n' then "too much indent" else "too little indent"

-- | > dedent ::= /(?=<leadingSpace>)/ (< PEEK) POP
dedent :: Parser ()
dedent = layoutOnly . try $ do
    n <- lookAhead leadingSpaces <|> (eof >> return 0)
    n' <- peekIndent
    if n < n'
        then popImplicit
        else fail $ "not dedented far enough (" ++ show n ++ " >= " ++ show n' ++ ")"

