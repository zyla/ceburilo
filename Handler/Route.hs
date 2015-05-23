{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Route where

import Import
import Yesod.Core
import Types
import Utils
import Data.Aeson.TH
import Data.Text
import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad
import Control.Applicative
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Map as M
import Graph

data RouteView = RouteView
    {   rv_path :: Path
    ,   rv_stations :: [Station]
    ,   rv_begining :: Text
    ,   rv_destination :: Text
    }

deriveToJSON jsonOptions ''RouteView

getRouteR :: Handler Value
getRouteR = do
    begName <- fromMaybe "" <$> lookupGetParam "begining"
    destName <- fromMaybe "" <$> lookupGetParam "destination"

    graph <- appGraph <$> getYesod
    stationPaths <- appStationPaths <$> getYesod

    let lookupStation = fmap spStation . flip M.lookup stationPaths
        allStations = fmap spStation $ M.elems stationPaths
        nearestStation point = stationNumber $
            minimumBy (comparing $ distanceSq point . stationLocation) $
            allStations

    beginStation <- nearestStation <$> requirePoint "beg_lat" "beg_lon"
    destStation <- nearestStation <$> requirePoint "dest_lat" "dest_lon"

    case generateRoute graph beginStation destStation of
      Just stationNumbers ->
        let stations = mapMaybe lookupStation (beginStation:stationNumbers)
            path = Path 0 0 Nothing -- FIXME
        in return $ toJSON $
            RouteView path stations begName destName
      
      Nothing -> sendResponseStatus status404 ("GÓWNO" :: Text)

requirePoint :: Text -- ^ latitude field
             -> Text -- ^ longitude field
             -> Handler Point
requirePoint lat lon =
    let readParam :: Read a => Text -> Handler a
        readParam name = requireGetParam name >>=
            maybe (invalidArgs [name <> ": invalid number"]) pure . readMay . unpack
    in Point <$> readParam lat <*> readParam lon

readMay s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing
