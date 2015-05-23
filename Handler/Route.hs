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
import Control.Monad
import Control.Applicative
import qualified Data.Vector as V
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
    begName <- lookupGetParam "begining"
    destName <- lookupGetParam "destination"

    begLat <- requireGetParam "beg_lat"
    destLat <- requireGetParam "dest_lat"
    begLng <- requireGetParam "beg_lng"
    destLng <- requireGetParam "dest_lng"

    return $ toJSON $ RouteView path stations (fromMaybe "" begName) (fromMaybe "" destName)
    where path = Path 30 234000 $ Just $ V.fromList
                    [    Point 52.235707713687624 20.996793508529663
                    ,    Point 52.24884675795982 21.005457043647766
                    ,    Point 52.230217725853215 21.01259171962738
                    ,    Point 52.2314251871654 21.0213263332844
                    ,    Point 52.23935090349061 21.01681351661682
                    ]
          stations = [
                         Station 123 "Stacja1" (Point 52.235707713687624 20.996793508529663)
                    ,    Station 234 "Stacja2" (Point 52.24884675795982 21.00545704364776)
                    ,    Station 345 "Stacja2" (Point 52.230217725853215 21.01259171962738)
                    ]
