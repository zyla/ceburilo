{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ceburilo.Graph where

import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.IntMap (Key)
import qualified Data.IntMap as IM
import Data.Graph.AStar as AS
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (sort)
import Control.Applicative ((<|>))
import Data.Aeson
import Ceburilo.Types
import GHC.Generics
import Control.DeepSeq
import System.Random

type Distance = Float
type Graph = IM.IntMap Node
--data Key = Int

data Node = Node { edges :: IM.IntMap Distance,
                   longitude :: Distance,
                   latitude :: Distance
                 } deriving (Generic, NFData)

-- How much time it takes to change bike (2 minutes)
-- in milliseconds
bikeChangeTime :: Distance
bikeChangeTime = 2 * 60 * 1000

getDistance :: Graph -> Key -> Key -> Distance
getDistance graph start goal = if start == goal then 0 else
-- very unsafe, but a* has proper assumption
  edges (graph IM.! start) IM.! goal + bikeChangeTime

getNeighbours :: Graph -> Key -> HashSet Key
getNeighbours graph nodeKey =
-- nodeKey will exist in DB!
  S.fromList . IM.keys $ edges (graph IM.! nodeKey)

-- Maximum allowed time (20 minutes), in milliseconds.
maxTime :: Distance
maxTime = 20 * 60 * 1000

getAllowedTimeNeighbours :: Graph -> Key -> HashSet Key
getAllowedTimeNeighbours graph nodeKey =
  S.fromList . IM.keys . IM.filter (<= maxTime) $ edges (graph IM.! nodeKey)

foundGoal :: Key -> Key -> Bool
foundGoal goal node = goal == node

-- In case you needed, change [] case for debugging
addNodeToGraph :: Graph -> Key -> Distance -> Distance -> Graph
addNodeToGraph graph key long lat =
  case IM.lookup key graph of
    Nothing -> graph -- error "Node with given id already exist"
    (Just _) -> IM.insert key (Node IM.empty long lat) graph


-- In case you needed, change [] case for debuggin
addEdgeToGraph :: Graph -> Key -> Key -> Distance -> Graph
addEdgeToGraph graph start goal dist =
  case mNode of
    Nothing -> graph -- error "Path leads to not existing node"
    (Just node) -> IM.insert start (Node (IM.insert goal dist (edges node))
                                      (longitude node)
                                      (latitude node) ) graph
    where
      mNode = IM.lookup start graph


generateRoute :: Graph -> Key -> Key -> Maybe [Key]
generateRoute graph start goal =
  AS.aStar (getAllowedTimeNeighbours graph)
           (getDistance graph)
           (getDistance graph goal)
           (foundGoal goal)
           start

dfsWithLimit :: Distance -> Graph -> Int -> Key -> Key -> Maybe [Key]
dfsWithLimit d g 0 c f =
  if getDistance g c f <= maxTime
     then Just [f]
     else Nothing
-- avg distance, graph, n, current, finish
dfsWithLimit d g n c f =
  -- get neighbours
  let neigh = getAllowedTimeNeighbours g c
  -- filter those which are closer to endpoint
  -- select up to 5 best neighbours
      filteredN =
                 filter (relativeCloseTo g d c f)
                . S.toList $ neigh
  -- run recursively
  in listToMaybe . mapMaybe (\a -> (c:) <$> dfsWithLimit d g (n-1) a f) $ filteredN


-- | Check if given station is closer
-- | to final station comparing to currently
-- | chosen station.
relativeCloseTo :: Graph -> Distance -> Key -> Key -> Key -> Bool
relativeCloseTo g d curr finish x =
  getDistance g curr finish > getDistance g x finish
  && getDistance g curr x <= d


-- | Generate route using heurisitc search
generateHeuristicRoute :: StdGen -> Graph -> Key -> Key -> Maybe [Key]
generateHeuristicRoute gen g start finish = do
  -- generate route using astar
  aroute <- generateRoute g start finish
  -- try to improve using heuristic search
  return . tail . solToPath $ simulatedAnnealing gen g 1000 0 (Solution start (init aroute) finish)

-- | Start, Path, Finish
data Solution = Solution Key [Key] Key
  deriving Show

solToPath :: Solution -> [Key]
solToPath (Solution x lst y) = x:(lst++[y])

-- | Variance of path.
-- | Number of stations is already calculated
-- | by astar earlier. Now we want the time
-- | between stations to be similar, so
-- | that's why variance is chosen for cost/goal function.
costFunc :: Graph -> Solution -> Distance
costFunc g (Solution start lst finish) =
  let
    wages = snd $ foldl (\(prevst, acc) x -> (x, getDistance g prevst x:acc))
      (start, []) (lst ++ [finish])
    avg = (/) <$> sum <*> (fromIntegral . length) $ wages
  in (*(-1)) . sum . map (\x -> (avg - x)**2) $ wages


-- | k_max, k
temperatureFunc :: Int -> Int -> Float
temperatureFunc kmax k = exp (-fromIntegral k/ fromIntegral kmax)


randomListElem :: StdGen -> [a] -> (a, StdGen)
randomListElem _ [] = error "empty list"
randomListElem gen lst =
  let
    (ix, newGen) = randomR (0, length lst - 1) gen
  in (lst !! ix, newGen)

-- | Get surrounding element at given index
getLeftRight :: Int -> [a] -> (a, a)
getLeftRight ix xs = (xs !! (ix-1), xs !! (ix+1))

-- | Randomly change one station in solution
randomNeighbour :: Graph ->  StdGen -> Solution -> (Solution, StdGen)
randomNeighbour g gen sol@(Solution start lst finish)
  | null lst = (sol, gen)
  | otherwise =
    let
      path = solToPath sol
      (elemIx, newGen) = randomR (0, length lst - 1) gen
      elemVal = lst !! elemIx
      (left, right) = getLeftRight (elemIx+1) path
      neighs = S.toList $ S.delete left . S.delete right $
        getAllowedTimeNeighbours g left `S.union` getAllowedTimeNeighbours g right
      -- node always has at least one neighbour, so non empty list is passed
      (next', newGen') = randomListElem newGen neighs
      newSol = Solution start (map (\x -> if x == elemVal
                                             then next'
                                             else x) lst) finish
      in (newSol, newGen')

-- | Probability of choosing worse solution
prob :: Distance -> Distance -> Float -> Float
prob x y t = exp (-abs (y - x) / t)

simulatedAnnealing :: StdGen
                   -> Graph
                   -> Int -- ^ k_max
                   -> Int -- ^ number of iteration
                   -> Solution -- ^ currently best solution
                   -> Solution
simulatedAnnealing gen graph kmax k sol
  | kmax == k = sol
  | otherwise =
    let
      (randNeigh, newGen) = randomNeighbour graph gen sol
      goalX = costFunc graph sol
      goalY = costFunc graph randNeigh
      chooseNewProb = prob goalX goalY (temperatureFunc kmax k)
      (rndnum, newGen') = randomR (0, 1) newGen
      reccall = simulatedAnnealing newGen' graph kmax (k+1)
    in
      if goalY > goalX || chooseNewProb > rndnum
         then reccall randNeigh
         else reccall sol

parseJSONFromFile :: FromJSON a => FilePath -> IO (Maybe a)
parseJSONFromFile file =
  decode <$> BS.readFile file


buildGraph :: [StationPaths] -> Graph
buildGraph = IM.fromList . fmap stationToPair
  where
    stationToPair (StationPaths (Station number _ (Point lat lon)) paths) =
        (number, node)
      where
        node = Node edgez lon lat
        edgez = fmap pathTime paths

readGraphFromFile :: FilePath -> IO (Maybe Graph)
readGraphFromFile fileName =
  fmap buildGraph <$> parseJSONFromFile fileName
