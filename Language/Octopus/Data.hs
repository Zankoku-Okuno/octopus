module Language.Octopus.Data where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.MVar (MVar)


type Tag = (Word, Text) -- ^ Integer for comparison, Text for spelling error reports
type Exn = (Word, Val)
type Fallible = Either Exn


data Val = Nm Rational -- ^ Rational number
         | By ByteString -- ^ Bytes
         | Tx Text -- ^ Text data
         | Fp Handle -- ^ Input/output handle
         | Sy Symbol -- ^ Symbol, aka. identifier
         | Tg Tag -- ^ Unique tag
         | Ab Tag Val -- ^ Abstract data
         | Sq (Seq Val) -- ^ Sequence, aka. list
         | Ob (Map Symbol Val) -- ^ Symbol-value map, aka. object
         | Cl Val Val Val -- ^ Operative closure
         | Ce (IORef Val) -- ^ Reference cell
         | Ar (IOArray Int Val) -- ^ Mutable array
         | Pr Primitive -- ^ Primitive operations
         | Ks [Control] -- ^ Control stack 
         --TODO concurrency
    deriving (Eq)
data Primitive = Vau | Eval | Match | Ifz | Imp
               | Eq | Neq | Lt | Lte | Gt | Gte
               | Add | Mul | Sub | Div
               | Numer | Denom | NumParts
               --TODO By data primitives
               --TODO Tx data primitives
               --TODO Fp data primitives
               | OpenFp | ReadFp | WriteFp | FlushFp | CloseFp
               --TODO Sy data primitives
               | MkTag | MkAbstype | Wrap Tag | Unwrap Tag | Typeof
               | Len | Cat | Cut
               | Extends | Delete | Keys | Get
               --TODO Ce data primitives
               --TODO Ar data primitives
               --TODO Pt/Ks data primitives
               | Handle | Raise
    deriving (Eq, Show)



type FileCache = MVar (Fallible Val)
type ImportsCache = MVar (Map Text FileCache)
data MState = MState { environ :: Val
                     , control :: [Control]
                     , nextTag :: Word
                     }

type Machine = ReaderT ImportsCache (StateT MState IO)


data Context = Op Val -- ^ Hole must be a combiner, val is the uneval'd argument
             | Ap Val -- ^ Hole is the argument, apply held value to it
             | Re Val -- ^ Restore an environment before continuing
             | Es [Val] [Val] -- ^ Left-to-right sequence evaluation
             | Eo Symbol [(Symbol, Val)] [(Symbol, Val)] -- ^ Object evaluation
    deriving (Eq, Show)
data Control = NormK [Context]
             | HndlK Tag Val
          --TODO onEnter/onSuccess/onFail
             | ImptK Text (MVar (Fallible Val))
    deriving (Eq)
instance Show Control where
    show (NormK k) = "NormK " ++ show k
    show (HndlK i fn) = "HndlK " ++ show i ++ " " ++ show fn
    show (ImptK path slot) = "ImptK " ++ show path


push :: Context -> Machine ()
push k = do
    (NormK ks):kss <- gets control
    modify $ \s -> s { control = (NormK (k:ks)):kss }
    --FIXME remember that when I push a handler or a winding protect, I also need to push an environment restore
pushK :: Control -> Machine ()
pushK k@(NormK _) = do
    ks <- gets control
    modify $ \s -> s { control = k:ks }
pushK k = do
    ks <- gets control
    modify $ \s -> s { control = (NormK []):k:ks }
pop :: Machine ()
pop = do
    stack <- gets control
    case stack of
        (NormK []):kss -> modify $ \s -> s { control = kss }
        (NormK (Re env:ks)):kss -> modify $ \s -> s { environ = env, control = (NormK ks):kss }
        (NormK (_:ks)):kss -> modify $ \s -> s { control = (NormK ks):kss }
        (HndlK _ _):kss -> modify $ \s -> s { control = kss }
        (ImptK _ _):kss -> modify $ \s -> s { control = kss }
replace :: Context -> Machine ()
replace k = pop >> push k

swapEnv :: Val -> Machine ()
swapEnv env' = do
    env <- gets environ
    (NormK ks):kss <- gets control
    case ks of
        (Re _):_ -> modify $ \s -> s { environ = env' } --allows tail recursion by not restoring environments that will immediately be thrown away by a second restoration
        ks -> modify $ \s -> s { environ = env', control = (NormK ((Re env):ks)):kss }

splitStack :: Word -> Machine ([Control], [Control])
splitStack tg = break isPoint <$> gets control
    where
    isPoint (HndlK (tg', _) _) = tg == tg'
    isPoint (ImptK _ _) = True
    isPoint _ = False



mkTag :: Text -> Machine Val
mkTag spelling = do
    n <- gets nextTag
    modify $ \s -> s { nextTag = n + 1 }
    return $ Tg (n, spelling)

mkAbstype :: Text -> Machine (Val, Val, Val)
mkAbstype spelling = do
    tag <- mkTag spelling
    let (Tg (n, spelling)) = tag
    return (tag, Pr (Wrap (n, spelling)), Pr (Unwrap (n, spelling)))


instance Show Val where
    show (Nm nm) | denominator nm == 1 = show $ numerator nm
                 | otherwise           = show (numerator nm) ++ "/" ++ show (denominator nm)
    show (By bytes) = "b" ++ show bytes --FIXME show with \x?? for non-ascii printable chars
    show (Tx text) = show text --FIXME show using Octopus encoding, not Haskell
    show (Fp _) = "#<handle>" --TODO at least show some metadata
    show (Sy sy) = show sy
    show (Tg (i, spelling)) = "#<tag " ++ show i ++ ": " ++ show spelling ++ ">"
    show (Ab tag x) = "#<box " ++ show tag ++ ": " ++ show x ++ ">"
    show (Sq xs) = "[" ++ intercalate ", " (show <$> toList xs) ++ "]"
    show (Ob m) = case getCombo m of
            Nothing -> "{" ++ intercalate ", " (showPair <$> Map.toList m) ++ "}"
            Just (f, x) -> "(" ++ show f ++ " " ++ show x ++ ")"
        where
        showPair (k,v) = show k ++ ": " ++ show v
        getCombo ob = case (Map.lookup (intern "__car__") ob, Map.lookup (intern "__cdr__") ob) of
            (Just f, Just x) -> if length (Map.keys ob) == 2 then Just (f, x) else Nothing
            _ -> Nothing
    show (Cl var ast env) = "#<closure>"
    show (Ce x) = "<reference cell>" --TODO show contents
    show (Ar xs) = "#<mutable array>" --TODO show contents
    --show (Eh tag fn) = "#<handler " ++ show tag ++ ": " ++ show fn ++ ">"
    show (Pr f) = "#<" ++ show f ++ ">"
