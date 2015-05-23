{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Control.Applicative
import Control.Monad

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

getStations :: Document -> [Station]
getStations = mapMaybe getStation . elementNodes . documentRoot
  where
    getStation (NodeElement (Element "place" attrs _)) =
        Station <$> attr "number" <*> attr "name" <*>
            (Point <$> (attr "lat" >>= readMay) <*> (attr "lng" >>= readMay))
      where
        attr :: Name -> Maybe String
        attr name = T.unpack <$> M.lookup name attrs

    getStation _ = Nothing

readMay s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

concurrently action = fmap concat . mapM (mapConcurrently action) . chunksOf 8

main = withManager defaultManagerSettings $ \manager -> do
    stations <- getStations <$> XML.readFile def "nextbike-live.xml"
    let pairs = filter (uncurry (/=)) $ (,) <$> stations <*> stations
    (>>= mapM_ (B8.putStrLn . Aeson.encode)) $ flip concurrently pairs $ \(start, end) ->
        getRoute (stationLocation start) (stationLocation end) manager
