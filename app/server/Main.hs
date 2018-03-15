{-# LANGUAGE DataKinds, TypeOperators, RecordWildCards, OverloadedStrings #-}
module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Data.Maybe
import Data.Ord
import Data.Foldable (minimumBy)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import Data.Aeson (FromJSON, ToJSON, toJSON, (.=), object, decodeStrict)
import System.Environment (lookupEnv)
import Control.Exception (evaluate)
import Control.DeepSeq (force)
-------------------------------
import Ceburilo.Types
import Ceburilo.Graph

type IMap = IM.IntMap StationPaths

data RouteView = RouteView
    { rvPath :: Path
    , rvStations :: [Station]
    , rvBegCoord :: Point
    , rvDestCoord :: Point
    }

instance ToJSON RouteView where
  toJSON RouteView{..} = object [ "beg_coord" .= rvBegCoord
                                , "dest_coord" .= rvDestCoord
                                , "path" .= rvPath
                                , "stations" .= rvStations ]


type API = "route" :> QueryParam "beg_lat" Float :> QueryParam "beg_lon" Float
                   :> QueryParam "dest_lat" Float :> QueryParam "dest_lon" Float
                   :> Get '[JSON] RouteView

proxyAPI :: Proxy API
proxyAPI = Proxy

generateRouteView :: Point -- ^Begginning lat,lon
              -> Point -- ^Destination lat.lon
              -> Graph -> IMap -- ^Given graph
              -> Maybe RouteView
generateRouteView begPoint dstPoint graph spath =
  let lookupStation = fmap spStation . flip IM.lookup spath
      lookupPath from to = IM.lookup from spath >>= IM.lookup to . spPaths
      allStations = spStation <$> IM.elems spath
      nearestStation point = stationNumber $
          minimumBy (comparing $ distanceEarth point . stationLocation)
          allStations

      beginStation = nearestStation begPoint
      destStation = nearestStation dstPoint
  in fmap (\stationNumbers ->

    let stations = mapMaybe lookupStation (beginStation:stationNumbers)
        stationPairs = zip (beginStation:stationNumbers) stationNumbers
        path = mconcat $ mapMaybe (uncurry lookupPath) stationPairs

    in RouteView path stations begPoint dstPoint
  ) (generateRoute graph beginStation destStation)

parseInput :: Graph -> IMap
           -> Maybe Float -> Maybe Float
           -> Maybe Float -> Maybe Float
           -> Handler RouteView
parseInput g mp (Just blat) (Just blon) (Just dlat) (Just dlon) =
  case generateRouteView (Point blat blon) (Point dlat dlon) g mp of
    Nothing -> throwError err500
    (Just x) -> return x
parseInput _ _ _ _ _ _ = throwError err400


app :: Graph -> IMap -> Application
app gr mp = serve proxyAPI (parseInput gr mp)


parseJSONFromFile :: FromJSON a => FilePath -> IO [Maybe a]
parseJSONFromFile file =
  fmap decodeStrict . Prelude.filter (not . BS.null) . BS.split newline <$> BS.readFile file
    where
      newline = 10


stationsToMap :: [StationPaths] -> IMap
stationsToMap = IM.fromList . map sspToPair
  where
    sspToPair sp@(StationPaths (Station number _ _) _) = (number, sp)


main :: IO ()
main = do
    port <- maybe 4000 read <$> lookupEnv "PORT"
    putStrLn "Parsing data file..."
    paths <- parseJSONFromFile "paths.json"
    putStrLn "Building graph..."
    graph <- evaluate $ force $ fromMaybe (error "error loading graph") $ buildGraph paths
    imap <- evaluate $ force $ stationsToMap $ fromMaybe (error "error loading graph") $ sequence paths
    putStrLn $ "Running server on port " ++ show port
    run port $ simpleCors $ app graph imap
