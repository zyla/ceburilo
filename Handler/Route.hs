{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Route where

import Import
import Yesod.Core
import Types
import Utils
import Data.Aeson.TH
import Data.Text hiding (zip)
import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad
import Control.Applicative
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.IntMap as IM
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

    let lookupStation = fmap spStation . flip IM.lookup stationPaths
        lookupPath from to = IM.lookup from stationPaths >>= IM.lookup to . spPaths
        allStations = fmap spStation $ IM.elems stationPaths
        nearestStation point = stationNumber $
            minimumBy (comparing $ distanceSq point . stationLocation) $
            allStations

    beginStation <- nearestStation <$> requirePoint "beg_lat" "beg_lon"
    destStation <- nearestStation <$> requirePoint "dest_lat" "dest_lon"

    case generateRoute graph beginStation destStation of
      Just stationNumbers ->
        let stations = mapMaybe lookupStation (beginStation:stationNumbers)
            stationPairs = zip (beginStation:stationNumbers) stationNumbers
            path = mconcat $ mapMaybe (uncurry lookupPath) stationPairs
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
