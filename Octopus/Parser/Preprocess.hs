module Octopus.Parser.Preprocess where

import Import

birdsFeet :: String -> String
birdsFeet input = unlines $ xformLine <$> lines input
    where
    xformLine l | "> " `isPrefixOf` l = "  " ++ drop 2 l
                | otherwise = ""

partitionCode :: String -> ([String], String)
partitionCode input = case lines input of
    [] -> ([], input)
    x:xs -> loop [] [x] xs
    where
    loop dirs acc [] = (reverse dirs, unlines $ reverse acc)
    loop dirs acc (x:xs) = if isDirective x then loop (x:dirs) ("":acc) xs else loop dirs (x:acc) xs
    isDirective = isPrefixOf "#!"