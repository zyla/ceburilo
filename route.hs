module Main where

import Data.Maybe

import qualified Data.IntMap as IM
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Vector as V
import System.Environment

import Ceburilo.Graph
import Ceburilo.Types.Packed


stationsToMap :: [Station] -> M.Map StationNumber Station
stationsToMap = M.fromList . map stationToPair
  where
    stationToPair station = (stationNumber station, station)

showStation :: Station -> String
showStation station = show (stationNumber station) ++ " " ++ stationName station

main :: IO ()
main = do
--    [start, end] <- fmap read <$> getArgs
    paths <- unpackStationPathsVector <$> B.readFile "paths.dat"

    mapM_ (\sp -> do
       putStrLn $ showStation $ spStation sp
       let toPaths = IM.toList $ spPaths sp
       putStrLn $ " to " ++ show (length toPaths)
       mapM_ (\(to, path) -> do
         putStrLn $ "  " ++ show to
          ++ " " ++ show (pathDistance path)
          ++ " " ++ show (pathTime path)
          ++ " " ++ show (V.take 5 (maybe V.empty id $ pathPoints path))
        ) toPaths
     ) paths

    let graph = buildGraph paths
        stations = stationsToMap . map spStation $ paths

    return ()

{-
    case generateRoute graph start end of
        Just route ->
            mapM_ (putStrLn . showStation) .
            catMaybes .
            fmap (flip M.lookup stations) $
            route

        Nothing -> putStrLn "Route not found!!!!!!!!!!!!!!!!!!!!11111111"
-}
