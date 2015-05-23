{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Arrow

import Data.Default
import Data.String
import Data.Maybe

import Data.List.Split (chunksOf)
import Control.Concurrent.Async (mapConcurrently)

import Network.HTTP.Client
import Data.Aeson as Aeson

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as B8

import Text.XML as XML

import Types

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
    initReq <- parseUrl "http://localhost:8989/route"
    let req = flip setQueryString initReq
            [ ("point", Just $ formatPoint start)
            , ("point", Just $ formatPoint end)
            , ("vehicle", Just "bike")
            , ("locale", Just "pl")
            , ("points_encoded", Just "false")
            ]
        formatPoint (Point lat lon) = fromString $ show lat ++ "," ++ show lon
    body <- responseBody <$> httpLbs req manager
    return $ fmap routePath $ Aeson.eitherDecode body

-- Maximum allowed time (20 minutes), in milliseconds.
maxTime = 20 * 60 * 1000

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

readMay s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

-- Split the list into chunks and apply given action in parallel for each chunk.
concurrently action = fmap concat . mapM (mapConcurrently action) . chunksOf 4

getStationRoutes :: [Station] -> Station -> Manager -> IO [StationPath]
getStationRoutes stations beginStation manager =
    let goodPath = (<= maxTime) . pathTime
        routeToStation dest = getRoute (stationLocation beginStation) (stationLocation dest) manager >>= \case
            Right route -> return $ Just (dest, route)
            Left _ -> return Nothing

    in fmap (fmap (uncurry StationPath . first stationNumber) . filter (goodPath . snd) . catMaybes) $
        concurrently routeToStation $
        filter (/= beginStation) $
        stations

processStations :: [Station] -> Manager -> IO [StationPaths]
processStations stations manager = mapM getStationPaths' stations
  where
    getStationPaths' station = StationPaths station <$> getStationRoutes stations station manager

main = withManager defaultManagerSettings $ \manager -> do
    stations <- getStations <$> XML.readFile def "nextbike-live.xml"
    processStations stations manager >>= B8.putStr . Aeson.encode
