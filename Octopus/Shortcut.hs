module Octopus.Shortcut where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Octopus.Data


mkInt :: Integral a => a -> Val
mkInt = Nm . fromIntegral

mkSy :: String -> Val
mkSy = Sy . intern

mkTx :: String -> Val
mkTx = Tx . pack

mkBy :: String -> Val
mkBy = By . encodeUtf8 . pack

mkSq :: [Val] -> Val
mkSq = Sq . Seq.fromList

mkOb :: [(Symbol, Val)] -> Val
mkOb = Ob . Map.fromList


getTag :: Val -> Word
getTag (Tg tg _) = tg

fromEnv :: Val -> Map Symbol Val
fromEnv (Ob ob) = ob
fromEnv _ = Map.empty
