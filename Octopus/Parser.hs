{-| Parse Octopus source code. The Octopus grammar is:

@
file ::= (\<expr\> | \<defn\>)*
defn ::= _field_ \<expr\>
expr ::= \<atom\> | \<list\> | \<object\>
      |  \<combination\> | \<block\> | \<quotation\>
      |  \/\\.\<expr\>\/ | '(' \<expr\> ')'
      |  \<accessor\> | \<mutator\>

atom ::= _symbol_ | _number_ | _string_ | _heredoc_ | _primitive_
list ::= '[' (\<expr\>+ (',' \<expr\>+)*)? ']'
object ::= '{' (_field_ \<expr\>+ (',' _field_ \<expr\>+)*)? '}'
combination ::= '(' \<expr\> \<expr\>+ ')'
block ::= 'do' (\<expr\> | \<defn\>)+ ';'
accessor ::= \/@\<name\>\/ | \/:\<name\>\/
mutator ::= '@(' _field_ \<expr\>+ ')' | ':(' _field_ \<expr\>+ ')'
quotation ::= '`' \<expr\>

symbol ::= \/\<name\>\/ - reserved
    reserved = {'do'}
primitive ::= \/#\<[a-zA-Z]+\>\/
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
heredoc ::= \/\#\<\<(?\'END\'\\w+)\\n.*?\\n\\g{END}\>\>\/
name ::= \/\<namehead\>\<nametail\>|-(\<namehead\>|-)\<nametail\>|-\/
    nametail = \/[^\#\\\\\"`()[]{}@:;.,]*\/
    namehead = \/[^\#\\\\\"`()[]{}@:;.,0-9-]\/

linecomment ::= \/#(?!\<)\\.*?\\n\/
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
import Language.Desugar

import Octopus.Data
import Octopus.Shortcut
import Octopus.Basis


type Parser = Parsec String ()

parseOctopusExpr :: SourceName -> String -> Either ParseError Val
parseOctopusExpr sourceName input = desugar <$> P.runParser (padded expr <* padded eof) () sourceName input

parseOctopusFile :: SourceName -> String -> Either ParseError Val
parseOctopusFile sourceName input = P.runParser octopusFile () sourceName input
    where
    octopusFile = do
        es <- P.many $ desugarStatement <$> padded statement
        padded eof
        return $ loop es
    loop [] = mkCall getenv (mkOb [])
    loop (Defn s:rest)  = mkCall (mkDefn s) (loop rest)
    loop (Expr e:rest) = mkCall (mkExpr e) (loop rest)
    getenv = (mkCall (Pr Vau) (mkSq [mkSq [mkSy "e", mkOb []], mkSy "e"]))

define :: Parser (Defn Syx)
define = do
    var <- try (expr <* char ':' <* whitespace)
    body <- expr
    return (var, body)

expr :: Parser Syx
expr = composite P.<|> atom
    where
    atom = Lit <$> P.choice [symbol, numberLit, textLit, heredoc, primitive] <?> "atom"
    composite = P.choice [ block, combine, sq, ob, quote, dottedExpr
                         , accessor, mutator, infixAccessor, infixMutator]

statement :: Parser (Statement Syx)
statement = P.choice [Defn <$> define, Expr <$> expr]


------ Sugar ------
data Statement a = Defn (Defn a)
                 | Expr a
                 | Deco a
    deriving (Show)
type Defn a = (a, a)
data Syx = Lit Val
         | Call [Syx]
         | SqSyx [Syx]
         | ObExpr [(Symbol, Syx)]
         | Do [Statement Syx]
         | Infix Syx
    deriving (Show)





desugar :: Syx -> Val
desugar (Lit x) = x
desugar (Call [x]) = desugar x
desugar (Call xs) = loop . (desugar <$>) $ revTripBy isInfix (id, rewrite) xs
    where
    rewrite [] inf rest = error "TODO syntax error: infix :/. needs a subject"
    rewrite subject (Infix inf) rest = inf : Call subject : rest
    loop [e] = e
    loop [f, x] = mkCall f x
    loop es = mkCall (loop $ init es) (last es)
    isInfix (Infix _) = True
    isInfix _ = False
desugar (SqSyx xs) = mkSq $ desugar <$> xs
desugar (ObExpr xs) = mkOb $ desugarField <$> xs
desugar (Do xs) = loop xs
    where
    loop [Defn d]      = mkCall (mkDefn $ desugarDefine d) (mkOb [])
    loop [Expr e]      = desugar e
    loop (Defn d:rest) = mkCall (mkDefn $ desugarDefine d) (loop rest)
    loop (Expr e:rest) = mkCall (mkExpr $ desugar e) (loop rest)
desugar x = error $ "INTERNAL ERROR Octopus.Parser.desugar: " ++ show x

desugarField :: (Symbol, Syx) -> (Symbol, Val)
desugarField (k, e) = (k, desugar e)

desugarDefine :: Defn Syx -> (Val, Val)
desugarDefine (x, e) = (desugar x, desugar e)

desugarStatement :: Statement Syx -> Statement Val
desugarStatement (Defn d) = Defn (desugarDefine d)
desugarStatement (Expr e) = Expr (desugar e)
desugarStatement (Deco f) = Deco (desugar f)


------ Atoms ------
primitive :: Parser Val
primitive = P.choice (map mkPrimParser table)
    where
    mkPrimParser (name, val) = string ("#<" ++ name ++ ">") >> return (Pr val)
    table = [ ("vau", Vau), ("eval", Eval), ("match", Match), ("ifz!", Ifz), ("import", Imp)
            , ("eq", Eq), ("neq", Neq), ("lt", Lt), ("lte", Lte), ("gt", Gt), ("gte", Gte)
            , ("add", Add) , ("mul", Mul) , ("sub", Sub) , ("div", Div)
            , ("numer", Numer) , ("denom", Denom) , ("numParts", NumParts)
            , ("mkTag", MkTag)
            , ("len", Len) , ("cat", Cat) , ("cut", Cut)
            , ("extends", Extends) , ("del", Delete) , ("keys", Keys) , ("get", Get)
            , ("handle", Handle) , ("raise", Raise)
            ]

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
    return $ mkTx content

heredoc :: Parser Val
heredoc = do
    string "#<<"
    end <- P.many1 P.letter <* char '\n'
    let endParser = char '\n' *> P.string (end ++ ">>") <* (void (char '\n') P.<|> eof)
    mkTx <$> anyChar `manyThru` endParser


------ Composites ------
combine :: Parser Syx
combine = do
    postPadded $ char '('
    e <- bareCombination
    padded $ char ')'
    return e

sq :: Parser Syx
sq = do
    postPadded $ char '['
    elems <- bareCombination `P.sepBy` padded comma
    padded $ char ']'
    return $ SqSyx elems

ob :: Parser Syx
ob = do
    postPadded $ char '{'
    elems <- pair `P.sepBy` padded comma
    padded $ char '}'
    return $ ObExpr elems
    where
    pair = do
        key <- intern <$> padded name
        char ':' <* whitespace
        val <- bareCombination
        return (key, val)

block :: Parser Syx
block = do
        try $ string "do" >> whitespace
        states <- P.many1 $ postPadded statement
        char ';'
        return $ Do states

quote :: Parser Syx
quote = do
    char '`'
    e <- expr
    return $ Call [Lit $ mkSy "__quote__", e]

dottedExpr :: Parser Syx
dottedExpr = Infix <$> (char '.' *> expr)

accessor :: Parser Syx
accessor = do
    key <- try $ char '@' *> name
    return $ Call [Lit $ mkSy "__get__", Lit $ mkSy key]

mutator :: Parser Syx
mutator = do
    string "@("
    key <- name <* char ':' <* whitespace
    e <- bareCombination
    char ')'
    return $ Call [Lit $ mkSy "__modify__", Lit $ mkSy key, e]

infixAccessor :: Parser Syx
infixAccessor = do
    key <- try $ char ':' *> name
    return . Infix $ Call [Lit $ mkSy "__get__", Lit $ mkSy key]

infixMutator :: Parser Syx
infixMutator = do
    string ":("
    key <- name <* char ':' <* whitespace
    e <- bareCombination 
    char ')'
    return . Infix $ Call [Lit $ mkSy "__modify__", Lit $ mkSy key, e]


------ Space ------
whitespace :: Parser ()
whitespace = (<?> "space") . P.skipMany1 $ P.choice [spaces1, lineComment, blockComment]

lineComment :: Parser ()
lineComment = void $ do
    try $ char '#' >> P.notFollowedBy (char '<')
    anyChar `manyThru` (void (char '\n') P.<|> eof)

blockComment :: Parser ()
blockComment = parserZero

padded :: Parser a -> Parser a
padded p = try $ P.optional whitespace >> p
postPadded :: Parser a -> Parser a
postPadded p = p <* P.optional whitespace


------ Helpers ------
name :: Parser String
name = P.choice [ (:) <$> namehead <*> nametail
                , (:) <$> char '-' <*> P.option [] ((:) <$> (namehead P.<|> char '-') <*> nametail)
                ]
    where
    namehead = blacklistChar (`elem` reservedFirstChar)
    nametail = P.many $ blacklistChar (`elem` reservedChar)
    reservedChar = "#\\\"`()[]{}@:;.,"
    reservedFirstChar = reservedChar ++ "-0123456789"

comma :: Parser ()
comma = char ',' >> whitespace

bareCombination :: Parser Syx
bareCombination = do
    es <- P.many1 (postPadded expr)
    return $ case es of { [e] -> e; es -> Call es }

mkDefn (x, val) = mkCall (mkCall (mkSy "__let__") x) val
mkExpr e = mkCall (mkCall (mkSy "__let__") (mkOb [])) e



