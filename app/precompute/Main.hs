{-# LANGUAGE OverloadedStrings, LambdaCase, Rank2Types #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.Default
import Data.String
import Data.Maybe

import Control.Concurrent.Async (mapConcurrently)

import Network.HTTP.Client (Manager, parseUrlThrow, newManager,
                            defaultManagerSettings, setQueryString,
                            responseBody, httpLbs)
import Data.Aeson as Aeson

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B8

import Text.XML as XML

import Data.Conduit.Async
import qualified Data.Conduit.List as CL

import Ceburilo.Types

-- Route in GraphHopper response format.
-- Just a wrapper around Path to provide FromJSON instance.
data Route = Route { routePath :: Path }

-- Decode a GraphHopper response.
-- Expects exactly one route in "paths" array.
instance FromJSON Route where
    parseJSON = withObject "Route" $ \obj -> obj .: "paths" >>= parsePaths
      where
        parsePaths = withArray "paths" $ \paths ->
            case V.toList paths of
                [path] -> Route <$> parseJSON path
                _ -> fail "Expected exactly one path"

-- Fetch a route from local GraphHopper instance (assumed http://localhost:8989/route).
-- Returns either the path or error message
getRoute :: Point -- ^ starting point
         -> Point -- ^ end point
         -> Manager
         -> IO (Either String Path)
getRoute start end manager = do
    initReq <- parseUrlThrow "http://localhost:8989/route"
    let req = flip setQueryString initReq
            [ ("point", Just $ formatPoint start)
            , ("point", Just $ formatPoint end)
            , ("vehicle", Just "bike")
            , ("locale", Just "pl")
            , ("points_encoded", Just "false")
            ]
        formatPoint (Point lat lon) = fromString $ show lat ++ "," ++ show lon
    body <- responseBody <$> httpLbs req manager
    return $ fmap (filterPath . routePath) $ Aeson.eitherDecode body

-- Maximum allowed time (20 minutes), in milliseconds.
maxTime :: Float
maxTime = 20 * 60 * 1000

-- Get rid of path's points if it's above maximium allowed time.
filterPath :: Path -> Path
filterPath (Path distance time _) | time > maxTime = Path distance time Nothing
filterPath path = path

getStations :: Document -> [Station]
getStations = mapMaybe getStation . elementNodes . documentRoot
  where
    getStation (NodeElement (Element "place" attrs _)) =
        Station <$> (attr "number" >>= readMay) <*> attr "name" <*>
            (Point <$> (attr "lat" >>= readMay) <*> (attr "lng" >>= readMay))
      where
        attr :: Name -> Maybe String
        attr name = T.unpack <$> M.lookup name attrs

    getStation _ = Nothing

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing


getStationRoutes :: [Station] -> Station -> Manager -> IO (IM.IntMap Path)
getStationRoutes stations beginStation manager =
-- Is it possible to split into chunks? Currently it performs OK
    foldM appendPath IM.empty (filter (/= beginStation) stations)
      where
      appendPath :: IM.IntMap Path -> Station -> IO (IM.IntMap Path)
      appendPath intMap (Station num a b) = do
        route <- routeToStation $ Station num a b
        case route of
          Just (Station dest _ _, path) -> return $ IM.insert dest path intMap
          Nothing -> return intMap
      routeToStation dest = getRoute (stationLocation beginStation) (stationLocation dest) manager >>= \case
            Right route -> return $ Just (dest, route)
            Left _ -> return Nothing


getStationPaths :: Manager -> [Station] -> Station -> IO StationPaths
getStationPaths manager stations station =
  StationPaths station <$> getStationRoutes stations station manager


-- | Outputs in 'JSON object per line' format
main :: IO ()
main = do
  manager <- newManager defaultManagerSettings -- will be closed automatically
  stations <- getStations <$> XML.readFile def "nextbike-live.xml"
  runCConduit $ CL.sourceList stations
           =$=& CL.chunksOf 8
           =$=& CL.concatMapM (liftIO . mapConcurrently (getStationPaths manager stations))
           =$=& CL.mapM_ (B8.putStrLn . Aeson.encode)
