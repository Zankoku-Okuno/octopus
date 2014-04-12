{-| Parse Octopus source code. The Octopus grammar is:


> file ::= ('export' <expr>)? <stmt>*
> stmt ::= <expr> | _field_ <expr> | 'open' <expr>
> expr ::= <atom> | <list> | <object>
>       |  <combination> | <block> | <quotation>
>       |  <accessor> | <mutator>
>       | '(' <expr> ')' |  /\.<expr>/
> 
> atom ::= _symbol_ | _number_ | _character_ | _string_ | _heredoc_ | _builtin_
> list ::= '[' (<expr>+ (',' <expr>+)*)? ']'
> object ::= '{' (_field_ <expr>+ (',' _field_ <expr>+)*)? '}'
> combination ::= '(' <expr> <expr>+ ')'
> block ::= 'do' <stmt>+ ';'
> accessor ::= /@<name>/ | /:<name>/
> mutator ::= '@(' _field_ <expr>+ ')' | ':(' _field_ <expr>+ ')'
> quotation ::= /`<expr>/
> 
> symbol ::= _name_ - reserved
>     reserved = {'do', 'letrec', 'export', 'open'}
> builtin ::= /#<[a-zA-Z]+>/
> field ::= /<name>:/
> number ::= /[+-]?(0[xX]<hexnum>|0[oO]<octnum>|0[bB]<binnum>|<decnum>)/
>     decnum ::= /\d+(\.\d+<exponent>?|\/\d+)?/
>     hexnum ::= /\x+(\.\x+([hH][+-]?\x+)?|\/\x+)?/
>     octnum ::= /[0-7]+(\.[0-7]+<exponent>?|\/[0-7]+)?/
>     binnum ::= /[01]+(\.[01]+<exponent>?|\/[01]+)?/
>     exponent ::= /[eE][+-]?\d+|[hH][+-]?\x+/
> character ::= /'[^\\]|\\[abefnrtv'"&\\]|\\<numescape>'/
> string ::= /"([^"\\]|\\[abefnrtv'"&\\]|\\<numescape>|\\\s*\n\s*\\)*"/
>         |  /r"([^"]|"")*"/
>         |  'b"' /\x\x/* '"'
>     numescape ::= /[oO][0-7]{3}|[xX]\x{2}|u\x{4}|U0\x{5}|U10x{4}/
> heredoc ::= /#<<(?'END'\w+)\n.*?\n\g{END}>>(\n|$)/
> name ::= /<namehead><nametail>|-<namehead><nametail>|-(-<nametail>)?/
>     namehead = /[^#\\"`()[]{}@:;.,'0-9-]/
>     nametail = /[^#\\"`()[]{}@:;.,]*/
> 
> linecomment ::= /#(?!<)\.*?\n/
> blockcomment ::= /#\{([^#}]+|<blockcomment>|#[^{]|\}[^#])*\}#/

-}
module Language.Octopus.Parser where

import Language.Octopus.Parser.Import

--import qualified Data.ByteString.Lazy as BS
--import qualified Data.Text as T
--import Text.Parsec ( Parsec, SourceName, ParseError
--                   , try, (<?>), unexpected
--                   , char, anyChar, oneOf, noneOf, eof)
import qualified Text.Parsec as P
--import Language.Parse
--import Language.Desugar

import Language.Octopus.Data
import Language.Octopus.Data.Shortcut
import Language.Octopus.Basis
import Language.Octopus.Parser.Preprocess
import Language.Octopus.Parser.Tokens
import Language.Octopus.Parser.Syntax
import Language.Octopus.Parser.Postprocess


type Directive = String

parseOctopusExpr :: SourceName -> String -> Either ParseError Val
parseOctopusExpr sourceName input = desugar <$> P.runParser parser startState sourceName input
    where
        parser = startFile *> bareCombination <* endFile

parseOctopusFile :: SourceName -> String -> Either ParseError ([Directive], Val)
parseOctopusFile sourceName input =
    let (directives, code) = partitionCode input
    in (,) directives . desugar <$> P.runParser parser startState sourceName code
    where
    parser = do
        --TODO parse notation configs
        whitespace0 >> blankLines
        P.optional (newline >> whitespace0 >> startImplicit)
        stmts <- (statement <* blankLines) `P.sepBy` nextLine
        endImplicit 0
        return . Do $ stmts ++ [getenv]
    getenv = Expr . Lit $ mkCall (mkCall (Pr Vau) (mkSq [mkSq [mkSy "e", mkXn []], mkSy "e"])) (mkXn [])

