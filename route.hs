module Main where

import Control.Applicative
import Data.Maybe

import qualified Data.Map as M
import System.Environment

import Graph
import Types


stationsToMap = M.fromList . map stationToPair
  where
    stationToPair station@(Station number name _) = (number, station)

showStation (Station number name _) = show number ++ " " ++ name

main = do
    [start, end] <- fmap read <$> getArgs 
    Just paths <- parseJSONFromFile "paths.json"
    let graph = buildGraph paths
        stations = stationsToMap . map spStation $ paths

    case generateRoute graph start end of
        Just route ->
            mapM_ (putStrLn . showStation) .
            catMaybes .
            fmap (flip M.lookup stations) $
            route

        Nothing -> putStrLn "Route not found!!!!!!!!!!!!!!!!!!!!11111111"
