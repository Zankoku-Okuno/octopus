module Octopus.Data where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Control.Monad.State




data Val = Nm Rational -- ^ Rational number
         | By ByteString -- ^ Bytes
         | Tx Text -- ^ Text data
         | Fp Handle -- ^ Input/output handle
         | Sy Symbol -- ^ Symbol, aka. identifier
         | Tg Int -- ^ Unique tag
         | Sq (Seq Val) -- ^ Sequence, aka. list
         | Ob (Map Symbol Val) -- ^ Symbol-value map, aka. object
         | Cl Val Val Val -- ^ Operative closure
         | Ce (IORef Val) -- ^ Reference cell
         | Ar (IOArray Int Val) -- ^ Mutable array
         | Pr Primitive -- ^ Primitive operations
         | Pt Int -- ^ Control prompt --TODO consider removing this in favor of using tags and a handler protocol
         | Ks [Control] -- ^ Control stack 
         -- | Ab Int Val -- ^ Abstract data --TODO consider eliminating abstract data and instead relying on a tagged protocol (__tag__). could work for dispatch, and I'm not sure data abstraction is with the philosphy. You can even get inside closures.
         --TODO bytestring/buffer/bytes/other name?
         --TODO concurrency
    deriving (Eq)
data Primitive = Vau | Eval | Match
			   --TODO Nm data primitives
			   --TODO By data primitives
			   --TODO Tx data primitives
			   --TODO Fp data primitives
			   --TODO Sy data primitives
			   --TODO Tg data primitives
			   --TODO Sq data primitives
			   --TODO Ob data primitives
			   | Extends | Delete | Keys | Get
			   --TODO Ce data primitives
			   --TODO Ar data primitives
			   --TODO Pt/Ks data primitives
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
    modify $ \s -> s { control = (NormK (k:ks)):kss }
    --FIXME remember that when I push a handler or a winding protect, I also need to push an environment restore
pop :: Machine ()
pop = do
    stack <- gets control
    case stack of
        (NormK []):kss -> modify $ \s -> s { control = kss }
        (NormK (Re env:ks)):kss -> modify $ \s -> s { environ = env, control = (NormK ks):kss }
        (NormK (_:ks)):kss -> modify $ \s -> s { control = (NormK ks):kss }
replace :: Context -> Machine ()
replace k = do
    stack <- gets control
    case stack of
        (NormK []):kss -> modify $ \s -> s { control = (NormK [k]):kss }
        (NormK (_:ks)):kss -> modify $ \s -> s { control = (NormK (k:ks)):kss }
swapEnv :: Val -> Machine ()
swapEnv env' = do
    env <- gets environ
    (NormK ks):kss <- gets control
    case ks of
    	(Re _):_ -> modify $ \s -> s { environ = env' } --allows tail recursion by not restoring environments that will immediately be thrown away by a second restoration
        ks -> modify $ \s -> s { environ = env', control = (NormK ((Re env):ks)):kss }




instance Show Val where
    show (Nm nm) | denominator nm == 1 = show $ numerator nm
                 | otherwise           = show (numerator nm) ++ "/" ++ show (denominator nm)
    show (By bytes) = "b" ++ show bytes --FIXME show with \x?? for non-ascii printable chars
    show (Tx text) = show text --FIXME show using Octopus encoding, not Haskell
    show (Fp _) = "<handle>" --TODO at least show some metadata
    show (Sy sy) = show sy
    show (Tg i) = "<tag: " ++ show i ++ ">"
    show (Sq xs) = "[" ++ intercalate ", " (show <$> toList xs) ++ "]"
    show (Ob m) = case getCombo m of
    		Nothing -> "{" ++ intercalate ", " (showPair <$> Map.toList m) ++ "}"
    		Just (f, x) -> "(" ++ show f ++ " " ++ show x ++ ")"
        where
        showPair (k,v) = show k ++ ": " ++ show v
        getCombo ob = case (Map.lookup (intern "__car__") ob, Map.lookup (intern "__cdr__") ob) of
        	(Just f, Just x) -> if length (Map.keys ob) == 2 then Just (f, x) else Nothing
        	_ -> Nothing
    show (Cl var ast env) = "<closure: var: " ++ show var ++ ", ast: " ++ show ast ++ ", env: " ++ show env ++ ">"
    show (Ce x) = "<reference cell>" --TODO show contents
    show (Ar xs) = "<mutable array>" --TODO show contents
    show (Pr f) = "<" ++ show f ++ ">"
    --show (Ab tag x) = "<box " ++ show tag ++ ": " ++ show x ++ ">"
