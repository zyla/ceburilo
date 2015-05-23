-- | Common type definitions for routes.
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards, OverloadedStrings, TemplateHaskell #-}
module Types where

import Control.Applicative
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Map as M

import Data.Aeson.TH
import Utils


data RouteView = RouteView
    {
        rv_data :: String
--        rv_routePoints :: [LocationPoint]
    }

deriveToJSON jsonOptions ''RouteView

data Point = Point
    { pointLatitude :: Float
    , pointLongitude :: Float
    }

instance Show Point where
    show (Point lat lon) = show (lat, lon)

instance FromJSON Point where
    parseJSON = withArray "Point" $ \case
        (V.toList -> [lat, lon]) -> Point <$> parseJSON lat <*> parseJSON lon
        _ -> fail "Expected a two-element array for Point"

instance ToJSON Point where
    toJSON (Point lat lon) = toJSON (lat, lon)

data Path = Path
    { pathDistance :: Float
    , pathTime :: Float
    , pathInstructions :: [Instruction]
    , pathPoints :: [Point]
    } deriving (Show)

instance FromJSON Path where
    parseJSON = withObject "Path" $ \obj ->
        Path <$> obj .: "distance"
             <*> obj .: "time"
             <*> obj .: "instructions"
             <*> ((obj .: "points") >>= (.: "coordinates"))

instance ToJSON Path where
    toJSON Path{..} = object
        [ "distance" .= pathDistance
        , "time" .= pathTime
        , "instructions" .= pathInstructions
        , "points" .= object [ "coordinates" .= pathPoints ]
        ]

-- One step of a route.
data Instruction = Instruction
    { instructionText :: String
    , instructionInterval :: (Int, Int)
    } deriving (Eq, Show)

instance ToJSON Instruction where
    toJSON Instruction{..} = object
        [ "text" .= instructionText
        , "interval" .= instructionInterval
        ]

instance FromJSON Instruction where
    parseJSON = withObject "Instruction" $ \obj ->
        Instruction <$> obj .: "text"
                    <*> obj .: "interval"

type StationNumber = String

data Station = Station
    { stationNumber :: StationNumber
    , stationName :: String
    , stationLocation :: Point
    } deriving (Show)

instance Eq Station where
    (Station n1 _ _) == (Station n2 _ _) = n1 == n2

-- Station location together with paths to other stations
data StationPaths = StationPaths
    { spStation :: Station
    , spPaths :: [StationPath]
    -- ^ Paths to other stations
    }

instance FromJSON StationPaths where
    parseJSON = withObject "StationPaths" $ \obj ->
        StationPaths <$> (Station <$> obj .: "number" <*> obj .: "name" <*> obj .: "location")
                     <*> obj .: "paths"

instance ToJSON StationPaths where
    toJSON StationPaths{..} = object
        [ "number" .= stationNumber spStation
        , "name" .= stationName spStation
        , "location" .= stationLocation spStation
        , "paths" .= spPaths
        ]

data StationPath = StationPath StationNumber Path

instance ToJSON StationPath where
    toJSON (StationPath number path) = object
        [ "number" .= number
        , "path" .= path
        ]

instance FromJSON StationPath where
    parseJSON = withObject "StationPath" $ \obj ->
        StationPath <$> obj .: "number" <*> obj .: "path"
