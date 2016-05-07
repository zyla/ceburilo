{-# LANGUAGE DataKinds, TypeOperators, RecordWildCards, OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Either
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Data.Maybe
import Data.Ord
import Data.Foldable (minimumBy)
import qualified Data.IntMap as IM
import Data.Aeson (ToJSON, toJSON, (.=), object)
import System.Environment (lookupEnv)
-------------------------------
import Ceburilo.Types
import Ceburilo.Graph

type IMap = IM.IntMap StationPaths

data RouteView = RouteView
    {   rv_path :: Path
    ,   rv_stations :: [Station]
    ,   rv_begCoord :: Point
    ,   rv_destCoord :: Point
    }

instance ToJSON RouteView where
  toJSON RouteView{..} = object [ "beg_coord" .= rv_begCoord
                                , "dest_coord" .= rv_destCoord
                                , "path" .= rv_path
                                , "stations" .= rv_stations ]


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
      allStations = fmap spStation $ IM.elems spath
      nearestStation point = stationNumber $
          minimumBy (comparing $ distanceEarth point . stationLocation) $
          allStations

      beginStation = nearestStation begPoint
      destStation = nearestStation dstPoint
  in (flip fmap) (generateRoute graph beginStation destStation) (\stationNumbers ->

    let stations = mapMaybe lookupStation (beginStation:stationNumbers)
        stationPairs = zip (beginStation:stationNumbers) stationNumbers
        path = mconcat $ mapMaybe (uncurry lookupPath) stationPairs

    in RouteView path stations begPoint dstPoint
  )

parseInput :: Graph -> IMap
           -> Maybe Float -> Maybe Float
           -> Maybe Float -> Maybe Float
           -> EitherT ServantErr IO RouteView
parseInput g mp (Just blat) (Just blon) (Just dlat) (Just dlon) =
  case generateRouteView (Point blat blon) (Point dlat dlon) g mp of
    Nothing -> left err500
    (Just x) -> return x
parseInput _ _ _ _ _ _ = left err400



app :: Graph -> IMap -> Application
app gr mp = serve proxyAPI (parseInput gr mp)


main :: IO ()
main = do
    port <- maybe 4000 read <$> lookupEnv "PORT"
    paths <- fromMaybe (error "error loading graph") <$>
        parseJSONFromFile "paths.json"
    run port $ simpleCors $ (app (buildGraph paths) (stationsToMap paths))

stationsToMap :: [StationPaths] -> IMap
stationsToMap = IM.fromList . map sspToPair
  where
    sspToPair sp@(StationPaths (Station number _ _) _) = (number, sp)
