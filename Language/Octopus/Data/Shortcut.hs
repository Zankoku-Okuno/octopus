module Language.Octopus.Data.Shortcut where

import Import
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.ByteString as BS

import Language.Octopus.Data


mkInt :: Integral a => a -> Val
mkInt = Nm . fromIntegral

mkSy :: String -> Val
mkSy = Sy . intern

mkTx :: String -> Val
mkTx = Tx . pack

mkBy :: [Word8] -> Val
mkBy = By . BS.pack

mkSq :: [Val] -> Val
mkSq = Sq . Seq.fromList

mkXn :: [(Symbol, Val)] -> Val
mkXn = Xn . Map.fromList


getTag :: Val -> Word
getTag (Tg (tg, _)) = tg

fromEnv :: Val -> Map Symbol Val
fromEnv (Xn xn) = xn
fromEnv _ = Map.empty
