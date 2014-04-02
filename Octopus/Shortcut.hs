module Octopus.Shortcut where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Octopus.Data

mkInt :: Integral a => a -> Val
mkInt = Nm . fromIntegral

mkSy :: String -> Val
mkSy = Sy . intern

mkSq :: [Val] -> Val
mkSq = Sq . Seq.fromList

mkOb :: [(Symbol, Val)] -> Val
mkOb = Ob . Map.fromList

fromEnv :: Val -> Map Symbol Val
fromEnv (Ob ob) = ob
fromEnv _ = Map.empty
