module Octopus.Shortcut where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Octopus.Data


mkSym :: String -> Val
mkSym = Sy . intern

mkSeq :: [Val] -> Val
mkSeq = Sq . Seq.fromList

mkObj :: [(Symbol, Val)] -> Val
mkObj = Ob . Map.fromList

fromEnv :: Val -> Map Symbol Val
fromEnv (Ob ob) = ob
fromEnv _ = Map.empty
