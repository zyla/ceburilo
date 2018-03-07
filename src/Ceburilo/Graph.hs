{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ceburilo.Graph where

import Data.HashSet as S
import Data.IntMap as IM
import Data.Graph.AStar as AS
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Ceburilo.Types.Packed
import GHC.Generics
import Control.DeepSeq

type Distance = Float
type Graph = IM.IntMap Node
--data Key = Int

type StationNumber = Int

data Node = Node { edges :: IM.IntMap Distance } deriving (Generic, NFData)

-- How much time it takes to change bike (2 minutes)
-- in milliseconds
bikeChangeTime :: Distance
bikeChangeTime = 2 * 60 * 1000

getDistance :: Graph -> Key -> Key -> Distance
getDistance graph start goal = if start == goal then 0 else
-- very unsafe, but a* has proper assumption
  (edges (graph ! start)) ! goal + bikeChangeTime

getNeighbours :: Graph -> Key -> HashSet Key
getNeighbours graph nodeKey =
-- nodeKey will exist in DB!
  S.fromList $ IM.keys $ edges $ (graph ! nodeKey)

-- Maximum allowed time (20 minutes), in milliseconds.
maxTime :: Distance
maxTime = 20 * 60 * 1000

getAllowedTimeNeighbours :: Graph -> Key -> HashSet Key
getAllowedTimeNeighbours graph nodeKey =
  S.fromList $ IM.keys $ IM.filter (<= maxTime) $ edges $ (graph ! nodeKey)

foundGoal :: Key -> Key -> Bool
foundGoal goal node = (goal == node)

generateRoute :: Graph -> Key -> Key -> Maybe [Key]
generateRoute graph start goal =
  AS.aStar (getAllowedTimeNeighbours graph)
           (getDistance graph)
           (getDistance graph goal)
           (foundGoal goal)
           start

parseJSONFromFile :: FromJSON a => FilePath -> IO (Maybe a)
parseJSONFromFile file =
  decode <$> BS.readFile file


buildGraph :: [StationPaths] -> Graph
buildGraph = IM.fromList . fmap stationToPair
  where
    stationToPair sp
     | station <- spStation sp
     , edgez <- fmap pathTime (spPaths sp)
        = (stationNumber station, Node edgez)
