module Octopus.Data where

import Data.List
import Data.Ratio
import Data.Symbol
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Data.Array.IO
import System.IO

import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad
import Control.Monad.State




data Val = Nm Rational -- ^ Rational number
         | Pr Primitive -- ^ Primitive operations
         | Fp Handle -- ^ Input/output handle
         | Pt Int -- ^ Control prompt
         | Ks [Control] -- ^ Control stack 
         | Ab Int Val -- ^ Abstract data
         | Ce (IORef Val) -- ^ Reference cell
         | Ar (IOArray Int Val) -- ^ Mutable array
         | Sq (Seq Val) -- ^ Sequence, aka. list
         | Ob (Map Symbol Val) -- ^ Symbol-value map, aka. object
         | Sy Symbol -- ^ Symbol, aka. identifier
         --TODO bytestream
         --TODO concurrency
    deriving (Eq)
data Primitive = Vau | Eval | MkEnv
    deriving (Eq, Show)




data MState = MState { environ :: Val, control :: [Control] }
type Machine = StateT MState IO



data Context = Op Val -- ^ Hole must be a combiner, val is the uneval'd argument
             | Ap Val -- ^ Hole is the argument, apply held value to it
             | Re Val -- ^ Restore an environment before continuing
             | Es [Val] [Val] -- ^ Left-to-right sequence evaluation
             | Eo Symbol [(Symbol, Val)] [(Symbol, Val)] -- ^ Object evaluation
    deriving (Eq, Show)
data Control = NormK [Context]
             | HndlK Val
          --TODO onEnter/onSuccess/onFail
    deriving (Eq, Show)
push :: Context -> Machine ()
push k = do
    (NormK ks):kss <- gets control
    modify $ \s -> s {control = (NormK (k:ks)):kss}
    --FIXME remember that when I push a handler or a winding protect, I also need to push an environment restore
pop :: Machine ()
pop = do
    stack <- gets control
    case stack of
        (NormK []):kss -> modify $ \s -> s {control = kss}
        (NormK (Re env:ks)):kss -> modify $ \s -> s {environ = env, control = (NormK ks):kss}
        (NormK (_:ks)):kss -> modify $ \s -> s {control = (NormK ks):kss}
replace :: Context -> Machine ()
replace k = do
    stack <- gets control
    case stack of
        (NormK []):kss -> modify $ \s -> s {control = (NormK [k]):kss}
        (NormK (_:ks)):kss -> modify $ \s -> s {control = (NormK (k:ks)):kss}
swapEnv :: Val -> Machine ()
swapEnv env' = do
    env <- gets environ
    (NormK ks):kss <- gets control
    case ks of
        (Re _):ks -> modify $ \s -> s {environ = env', control = (NormK ((Re env):ks)):kss}
        _ -> modify $ \s -> s {environ = env', control = (NormK ((Re env):ks)):kss}




instance Show Val where
    show (Nm nm) | denominator nm == 1 = show $ numerator nm
                 | otherwise           = show (numerator nm) ++ "/" ++ show (denominator nm)
    show (Sy sy) = show sy
    show (Ab tag x) = "<box " ++ show tag ++ ": " ++ show x ++ ">"
    show (Ce x) = "<reference cell>" --TODO show contents
    show (Ar xs) = "<mutable array>" --TODO show contents
    show (Fp _) = "<handle>" --TODO at least show some metadata
    show (Sq xs) = "[" ++ intercalate ", " (show <$> toList xs) ++ "]"
    show (Ob m) = "{" ++ intercalate " " (showPair <$> Map.toList m) ++ "}"
        where
        showPair (k,v) = ":" ++ show k ++ " " ++ show v
    show (Pr f) = "<primitive: " ++ show f ++ ">"
