module Language.Octopus.Parser.Policy where

import Language.Octopus.Data
import Language.Octopus.Parser.Import
import Language.Octopus.Basis


reservedChar :: [Char]
reservedChar = "#\\\"`()[]{}@:;.,"

reservedFirstChar :: [Char]
reservedFirstChar = reservedChar ++ "-'0123456789"

reservedWords :: [String]
reservedWords = ["do", "letrec", "export", "open", "_"]


getBuiltins :: Parser [(String, Val)]
getBuiltins = do
    --TODO ask for extra primitives
    extra <- pure [ ("openFile", Pr OpenFp)
                  , ("stdin", fpStdin), ("stdout", fpStdout), ("stdin", fpStderr)
                  ]
    return $ table ++ extra
    where
    table = [ ("vau", Pr Vau), ("eval", Pr Eval), ("match", Pr Match), ("ifz!", Pr Ifz), ("import", Pr Imp)
            , ("eq", Pr Eq), ("neq", Pr Neq), ("lt", Pr Lt), ("lte", Pr Lte), ("gt", Pr Gt), ("gte", Pr Gte)
            , ("add", Pr Add) , ("mul", Pr Mul) , ("sub", Pr Sub) , ("div", Pr Div)
            , ("numer", Pr Numer) , ("denom", Pr Denom) , ("numParts", Pr NumParts)
            , ("readByte", Pr ReadFp), ("writeByte", Pr WriteFp), ("flush", Pr FlushFp), ("close", Pr CloseFp)
            , ("mkTag", Pr MkTag), ("mkAbstype", Pr MkAbstype), ("typeof", Pr Typeof)
            , ("len", Pr Len) , ("cat", Pr Cat) , ("cut", Pr Cut)
            , ("new", Pr New), ("deref", Pr Deref), ("assign", Pr Assign)
            , ("newArr", Pr NewArr), ("bounds", Pr Bounds), ("index", Pr Index), ("assignIx", Pr AssignIx)
            , ("extends", Pr Extends) , ("del", Pr Delete) , ("keys", Pr Keys) , ("get", Pr Get)
            , ("handle", Pr Handle) , ("raise", Pr Raise)

            , ("Nm", tyNm), ("Fn", tyFn)
            , ("Sy", tySy), ("Tg", tyTg)
            , ("By", tyBy), ("Tx", tyTx)
            , ("Sq", tySq), ("Xn", tyXn)
            , ("Ce", tyCe), ("Ar", tyAr), ("Fp", tyFp)
            
            , ("TypeError", exnTypeError), ("MatchFail", exnMatchFail)
            , ("ScopeError", exnScopeError), ("AttrError", exnAttrError), ("IndexError", exnIndexError)
            , ("DivZero", exnDivZero), ("IOError", exnIOError)
            , ("SyntaxError", exnSyntaxError), ("ImportError", exnImportError)
            ]