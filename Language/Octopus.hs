{-# LANGUAGE OverloadedStrings #-}
module Language.Octopus where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader
import System.IO.Error
import Control.Concurrent.MVar

import Language.Octopus.Data
import Language.Octopus.Parser (parseOctopusFile)
import qualified Language.Octopus.Primitive as Oct
import Language.Octopus.Basis
import Language.Octopus.Data.Shortcut
import Language.Octopus.Libraries


eval :: ImportsCache -> Val -> Val -> IO Val
eval cache env code = evalStateT (runReaderT (reduce code) cache) startState
    where
    startState = MState { environ = env --Ob Map.empty
                        , control = [NormK []]
                        , nextTag = startTag
                        }


reduce :: Val -> Machine Val
reduce x@(Nm _) = done x
reduce x@(By _) = done x
reduce x@(Tx _) = done x
reduce x@(Fp _) = done x
reduce x@(Tg _) = done x
reduce x@(Ab _ _) = done x
reduce x@(Cl _ _ _) = done x
reduce x@(Ce _) = done x
reduce x@(Ar _) = done x
reduce x@(Pr _) = done x
reduce sq@(Sq xs) = case toList xs of
    [] -> done sq
    (x:xs) -> push (Es [] xs) >> reduce x
reduce ob@(Ob m) = case ensureCombination ob of
    Just (f, x) -> push (Op x) >> reduce f
    Nothing -> case Map.toList m of
                [] -> done (mkOb [])
                ((k,v):xs) -> push (Eo k [] xs) >> reduce v
reduce (Sy x) = gets environ >>= \env -> case Oct.resolveSymbol x env of
    Just val -> done val
    Nothing -> do
        env <- gets environ
        raise (getTag exnScopeError, mkSq [exnScopeError, Sy x, env])

combine :: Val -> Val -> Machine Val
combine (Pr Vau) x = case x of
    Sq xs -> case toList xs of
        [var, ast] -> do
            env <- gets environ
            done $ Cl var ast env
        _ -> raise $ mkTypeError (Pr Vau) "(Pat, *)" x
    _ -> raise $ mkTypeError (Pr Vau) "(Pat, *)" x
combine f@(Pr _) x = push (Ap f) >> reduce x
combine f x = case ensureClosure f of --all of these are operatives
    Just (var, ast, env) -> do
        caller <- gets environ
        let x' = mkSq [caller, x]
        case Oct.match var x' of
            Right env' -> swapEnv (env' `Oct.extend` env) >> reduce ast
            Left err -> raise err
    _ -> raise $ mkTypeError (Tx "#<apply>") "* → *" f

apply :: Val -> Val -> Machine Val
apply (Pr Eval) x = case ensureThunk x of
    Just (env, ast) -> swapEnv env >> reduce ast
    _ -> raise $ mkTypeError (Pr Eval) "Thunk *" x
apply (Pr Ifz) x = case x of
    Sq xs -> case toList xs of
        [p, c, a] -> done $ Oct.ifz p c a
        _ -> tyErr
    _ -> tyErr
    where tyErr = raise $ mkTypeError (Pr Ifz) "(*, *, *)" x
apply (Pr Imp) x = impFile x
apply (Pr Extends) x = case x of
    Sq xs -> case toList xs of
        [] -> done $ mkOb []
        xs -> done $ foldr1 Oct.extend xs
    _ -> raise $ mkTypeError (Pr Extends) "[*]" x
apply (Pr MkTag) x = case x of
    Tx spelling -> done =<< mkTag spelling
    _ -> raise $ mkTypeError (Pr MkTag) "Tx" x
apply (Pr MkAbstype) x = case x of
    Tx spelling -> mkAbstype spelling >>= \(tag, ctor, dtor) -> done (mkSq [tag, ctor, dtor])
    _ -> raise $ mkTypeError (Pr MkAbstype) "Tx" x
apply (Pr (Wrap tg)) x = done (Ab tg x)
apply pr@(Pr (Unwrap (n, spelling))) x = case x of
    Ab (n', _) val | n == n' -> done val
    _ -> raise $ mkTypeError pr spelling x
apply (Pr Handle) x = case x of
    Sq xs -> case toList xs of
        [tag, handler, body] -> case tag of
            Tg tg -> pushK (HndlK tg handler) >> reduce body
            _ -> tyErr
        _ -> tyErr
    _ -> tyErr
    where tyErr = raise $ mkTypeError (Pr Handle) "∀ r. (Tg, * → r, `r)" x --FIXME give the type as a monad
apply (Pr Raise) x = case x of
    Sq xs -> case toList xs of
        [tag, payload] -> case tag of
            Tg (i, _) -> curry raise i payload
            _ -> tyErr
        _ -> tyErr
    _ -> tyErr
    where tyErr = raise $ mkTypeError (Pr Raise) "(Tg, *)" x
apply (Pr pr) x | pr `elem` (map fst table) = do
    result <- liftIO $ (fromJust $ lookup pr table) x
    case result of
        Right val -> done val
        Left err -> raise err
    where
    table =
        [ (OpenFp, Oct.openFp)
        , (ReadFp, Oct.readFp)
        , (WriteFp, Oct.writeFp)
        , (FlushFp, Oct.flushFp)
        , (CloseFp, Oct.closeFp)
        ]
apply (Pr pr) x =
    case lookup pr table of
        Just f -> case f pr x of
            Right val -> done val
            Left err -> raise err
        Nothing -> error $ "INTERNAL ERROR (Octopus.apply): unknown primitive " ++ show pr
    where
    table = 
        [ (Match, binary Oct.match "TODO")
        
        , (Delete, binary Oct.delete "TODO")
        , (Keys, unary Oct.keys)
        , (Get, binary Oct.get "TODO")
        
        , (Eq, binary Oct.eq "(*, *)")
        , (Neq, binary Oct.neq "(*, *)")
        , (Lt, binary Oct.lt "(Nm, Nm)")
        , (Lte, binary Oct.lte "(Nm, Nm)")
        , (Gt, binary Oct.gt "(Nm, Nm)")
        , (Gt, binary Oct.gt "(Nm, Nm)")
    
        , (Add, binary Oct.add "(Nm, Nm)")
        , (Sub, binary Oct.sub "(Nm, Nm)")
        , (Mul, binary Oct.mul "(Nm, Nm)")
        , (Div, binary Oct.div "(Nm, Nm)")
    
        , (Typeof, unary Oct.typeof)

        , (Len, unary Oct.len)
        , (Cat, binary Oct.cat "∀ f :: Sq * | Tx | By. (f, f)")
        , (Cut, binary Oct.cut "(Sq * | Tx | By, Nat)")
    
        , (Numer, unary Oct.numer)
        , (Denom, unary Oct.denom)
        , (NumParts, unary Oct.numParts)
        ]
    binary :: (Val -> Val -> Fallible Val) -> Text -> Primitive -> Val -> Fallible Val
    binary op ty pr x = case x of
        Sq xs -> case toList xs of
            [a, b] -> op a b
            _ -> tyErr
        _ -> tyErr
        where tyErr = Left $ mkTypeError (Pr pr) ty x
    unary :: (Val -> Fallible Val) -> Primitive -> Val -> Fallible Val
    unary op pr x = op x
--TODO Ks
--TODO apply objects that have a __call__ field, passing the object as the first parameter
--TODO unwrap Ab and try the __call__ protocol trick
apply f x = error "TODO apply"

done :: Val -> Machine Val
done x = do
    k <- gets control
    case k of
        [] -> return x
        (NormK []):_                        -> pop >> done x
        (NormK (Re _:_)):_                  -> pop >> done x
        (NormK (Es xs []:_)):_              -> pop >> done (Sq . Seq.fromList $ reverse (x:xs))
        (NormK (Es xs (x':xs'):_)):_        -> replace (Es (x:xs) xs') >> reduce x'
        (NormK (Eo k xs []:_)):_            -> pop >> done (Ob . Map.fromList $ (k,x):xs)
        (NormK (Eo k xs ((k',x'):xs'):_)):_ -> replace (Eo k' ((k,x):xs) xs') >> reduce x'
        (NormK (Op arg:ks)):kss             -> pop >> combine x arg
        (NormK (Ap f:ks)):kss               -> pop >> apply f x
        (HndlK _ _):kss                     -> pop >> done x
        (ImptK _ slot):kss                  -> liftIO (slot `putMVar` Right x) >> pop >> done x


raise :: Exn -> Machine Val
raise (tg, payload) = do
    (top, rest) <- splitStack tg
    case rest of
        [] -> error $ "unhandled exception: " ++ show (tg, payload) --return here
        (ImptK file slot):kss -> do
            let err = (getTag exnImportError, mkSq [exnImportError, Tx file, payload])
            liftIO $ slot `putMVar` Left err
            modify $ \s -> s { control = kss }
            raise err
        (HndlK _ handler):kss -> do
            modify $ \s -> s { control = kss }
            reduce $ mkCall handler payload


impFile :: Val -> Machine Val
impFile (Tx pathstr) = do
    let path = pathstr --FIXME normalize path
        path' = unpack path
    --TODO check against builtin files, or else pre-populate the cache
    --TODO check the path against the current imports on stack to avoid circular import
    cache_var <- ask
    cache <- liftIO $ takeMVar cache_var
    case Map.lookup path cache of
        Just loaded_var -> do
            liftIO $ cache_var `putMVar` cache
            val_e <- liftIO $ readMVar loaded_var
            case val_e of
                Right val -> done val
                Left err -> raise err
        Nothing -> do
            loading_var <- liftIO newEmptyMVar
            liftIO $ cache_var `putMVar` Map.insert path loading_var cache
            contents_e <- liftIO $ tryIOError $ readFile path'
            case contents_e of
                Right contents -> do
                    case parseOctopusFile path' contents of
                        Right (_, val) -> do
                            swapEnv initialEnv --TODO consider which env to start a file off with
                            pushK (ImptK path loading_var)
                            reduce val
                        Left raw_err -> do
                            let err = (getTag exnSyntaxError, mkSq [exnSyntaxError, Tx . pack $ show raw_err])
                            liftIO $ loading_var `putMVar` Left err
                            raise err
                Left raw_err -> do
                    --TODO format the IOError the way I want it, not the way Haskell gives it
                    let err = (getTag exnIOError, mkSq [exnIOError, Tx . pack $ show raw_err])
                    liftIO $ loading_var `putMVar` Left err
                    raise err
impFile x = raise $ mkTypeError (Pr Imp) "Tx" x


