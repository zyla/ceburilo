-- | Common type definitions for routes.
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Ceburilo.Types (
    module Ceburilo.Types
  , module Ceburilo.Geo
) where

import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Maybe
import qualified Data.Vector.Storable as V
import qualified Data.IntMap as IM
import GHC.Generics
import Control.DeepSeq
import Ceburilo.Geo

data Path = Path
    { pathDistance :: Float
    , pathTime :: Float
    , pathPoints :: Maybe (V.Vector Point)
    } deriving (Show, Generic, NFData)

instance FromJSON Path where
    parseJSON = withObject "Path" $ \obj ->
        Path <$> obj .: "distance"
             <*> obj .: "time"
             <*> (((obj .: "points") >>= (.: "coordinates")) <|> pure Nothing)

instance ToJSON Path where
    toJSON Path{..} = object
        [ "distance" .= pathDistance
        , "time" .= pathTime
        , "points" .= (case pathPoints of
            Just pp -> object [ "coordinates" .= pp ]
            Nothing -> Null
        )
        ]

instance Monoid Path where
    mempty = Path 0 0 Nothing
    (Path d1 t1 p1) `mappend` (Path d2 t2 p2) =
        Path (d1 + d2) (t1 + t2)
             (Just $ fromMaybe V.empty p1 <> fromMaybe V.empty p2)

type StationNumber = Int

data Station = Station
    { stationNumber :: StationNumber
    , stationName :: String
    , stationLocation :: Point
    } deriving (Show, Generic, NFData)

instance Eq Station where
    (Station n1 _ _) == (Station n2 _ _) = n1 == n2

instance ToJSON Station where
    toJSON Station{..} = object
        [ "number" .= stationNumber
        , "name" .= stationName
        , "location" .= stationLocation
        ]

-- Station location together with paths to other stations
data StationPaths = StationPaths
    { spStation :: Station
    , spPaths :: IM.IntMap Path
    -- ^ Paths to other stations
    } deriving (Generic, NFData)

instance FromJSON StationPaths where
    parseJSON = withObject "StationPaths" $ \obj ->
        StationPaths <$> (Station <$> obj .: "number" <*> obj .: "name" <*> obj .: "location")
                     <*> (IM.fromList <$> fmap spToPair <$> obj .: "paths")

instance ToJSON StationPaths where
    toJSON StationPaths{..} = object
        [ "number" .= stationNumber spStation
        , "name" .= stationName spStation
        , "location" .= stationLocation spStation
        , "paths" .= fmap pairToStationPath (IM.toList spPaths)
        ]

data StationPath = StationPath StationNumber Path

spToPair :: StationPath -> (StationNumber, Path)
spToPair (StationPath number path) = (number, path)

pairToStationPath :: (StationNumber, Path) -> StationPath
pairToStationPath (number, path) = StationPath number path

instance ToJSON StationPath where
    toJSON (StationPath number path) = object
        [ "number" .= number
        , "path" .= path
        ]

instance FromJSON StationPath where
    parseJSON = withObject "StationPath" $ \obj ->
        StationPath <$> obj .: "number" <*> obj .: "path"

