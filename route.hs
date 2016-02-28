module Main where

import Data.Maybe

import qualified Data.Map as M
import System.Environment

import Ceburilo.Graph
import Ceburilo.Types


stationsToMap :: [Station] -> M.Map StationNumber Station
stationsToMap = M.fromList . map stationToPair
  where
    stationToPair station@(Station number _ _) = (number, station)

showStation :: Station -> String
showStation (Station number name _) = show number ++ " " ++ name

main :: IO ()
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
