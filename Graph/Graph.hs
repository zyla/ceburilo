import Data.Set as S
import Data.IntMap as IM

type Distance = Float
type Graph = IM.IntMap Node
--data Key = Int

data Node = Node { edges :: IM.IntMap Distance,
                   longitude :: Distance,
                   latitude :: Distance
                 }


getDistance :: Graph -> Key -> Key -> Distance
getDistance graph start goal =
-- very unsafe, but a* has proper assumption
  (edges (graph ! start)) ! goal

getNeighbours :: Graph -> Key -> Set Key
getNeighbours graph nodeKey =
-- nodeKey will exist in DB!
  S.fromList $ IM.keys $ edges $ (graph ! nodeKey)

getStraightDistance :: Graph -> Key -> Key -> Distance
getStraightDistance graph goal start =
  sqrt ( (x1-x2)^2 + (y1-y2)^2 )
    where
      x1 = longitude $ (graph ! goal)
      x2 = longitude $ (graph ! start)
      y1 = latitude $ (graph ! goal)
      y2 = latitude $ (graph ! start)

foundGoal :: Key -> Key -> Bool
foundGoal goal node = (goal == node)

-- In case you needed, change [] case for debugging
addNodeToGraph :: Graph -> Key -> Distance -> Distance -> Graph
addNodeToGraph graph id long lat =
  case IM.lookup id graph of
    Nothing -> graph -- error "Node with given id already exist"
    (Just _) -> IM.insert id (Node IM.empty long lat) graph


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


