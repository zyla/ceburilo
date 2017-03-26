-- | Common type definitions for routes.
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Ceburilo.Types.Packed (
    StationNumber
  
  , Path
  , pathDistance
  , pathTime
  , pathPoints

  , Station
  , stationNumber
  , stationName
  , stationLocation

  , StationPaths
  , spStation
  , spPaths
  , unpackStationPathsVector

  , module Ceburilo.Geo
) where

import Control.Applicative
import Data.Function (on)
import Data.Monoid
import Data.Aeson
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.IntMap as IM
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import GHC.Generics
import Control.DeepSeq
import Ceburilo.Geo
import Ceburilo.Packed
import Data.Packer

data PathSegment = PathSegment Packed -- 12

pathSegmentDistance :: PathSegment -> Float
pathSegmentDistance (PathSegment p) = readPacked getFloat32LE 0 p

pathSegmentTime :: PathSegment -> Float
pathSegmentTime (PathSegment p) = readPacked getFloat32LE 4 p

readPoint :: Unpacking Point
readPoint = Point <$> getFloat32LE <*> getFloat32LE

pathSegmentPoints :: PathSegment -> Maybe (V.Vector Point)
pathSegmentPoints (PathSegment p) =
  V.fromList <$>
  readVector 8 (readPacked readPoint 0) <$>
  readMaybePtr 8 p

data Path = Path { pathSegments :: [PathSegment] }

pathDistance :: Path -> Float
pathDistance = sum . map pathSegmentDistance . pathSegments

pathTime :: Path -> Float
pathTime = sum . map pathSegmentTime . pathSegments

pathPoints :: Path -> Maybe (V.Vector Point)
pathPoints = mconcat . map pathSegmentPoints . pathSegments

instance ToJSON Path where
    toJSON v = object
        [ "distance" .= pathDistance v
        , "time" .= pathTime v
        , "points" .= (case pathPoints v of
            Just pp -> object [ "coordinates" .= pp ]
            Nothing -> Null
        )
        ]
instance Monoid Path where
    mempty = Path []
    Path s1 `mappend` Path s2 = Path (s1 ++ s2)

type StationNumber = Int

data Station = Station Packed -- 16

stationNumber :: Station -> StationNumber
stationNumber (Station p) = fromIntegral $ readPacked getWord32LE 0 p

stationName :: Station -> String
stationName (Station p) = T.unpack $ T.decodeUtf8With T.lenientDecode $ B.pack $
  readVector 1 (readPacked getWord8 0) (readPackedPtr 4 p)

stationLocation :: Station -> Point
stationLocation (Station p) = readPacked readPoint 8 p

instance Eq Station where
    (==) = (==) `on` stationNumber

instance ToJSON Station where
    toJSON station = object
        [ "number" .= stationNumber station
        , "name" .= stationName station
        , "location" .= stationLocation station
        ]

-- Station location together with paths to other stations
data StationPaths = StationPaths Packed -- 20

spStation :: StationPaths -> Station
spStation (StationPaths p) = Station (atOffset 0 p)

spPaths :: StationPaths -> IM.IntMap Path
spPaths (StationPaths p) = IM.fromList $ readVector 16 readStationPath (readPackedPtr 16 p)

instance ToJSON StationPaths where
    toJSON sp = object
        [ "number" .= stationNumber (spStation sp)
        , "name" .= stationName (spStation sp)
        , "location" .= stationLocation (spStation sp)
        , "paths" .= fmap pathToJSON (IM.toList (spPaths sp))
        ]
      where
        pathToJSON (number, path) = object
          [ "number" .= number
          , "path" .= path
          ]

readStationPath :: Packed -> (StationNumber, Path) -- 16
readStationPath p =
  (fromIntegral $ readPacked getWord32LE 0 p, Path [PathSegment (atOffset 4 p)])

unpackStationPathsVector :: ByteString -> [StationPaths]
unpackStationPathsVector bs = readVector 20 StationPaths (Packed bs 0)
