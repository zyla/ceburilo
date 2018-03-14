{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Ceburilo.Geo where

import GHC.Generics
import Control.DeepSeq
import qualified Data.Vector as V
import Data.Aeson
import qualified Foreign.Storable as Storable
import Foreign.Ptr (Ptr, castPtr, plusPtr)

data Point = Point
    { pointLatitude :: !Float
    , pointLongitude :: !Float
    } deriving (Generic, NFData)

instance Show Point where
    show (Point lat lon) = show (lat, lon)

instance FromJSON Point where
    parseJSON = withArray "Point" $ \case
        (V.toList -> [lat, lon]) -> Point <$> parseJSON lat <*> parseJSON lon
        _ -> fail "Expected a two-element array for Point"

instance ToJSON Point where
    toJSON (Point lat lon) = toJSON (lat, lon)

instance Storable.Storable Point where
  sizeOf _ = 2 * sizeOfFloat
  alignment _ = Storable.alignment (undefined :: Float)
  peek ptr =
    let ptrF = castPtr ptr :: Ptr Float
    in Point <$> Storable.peek ptrF <*> Storable.peek (ptrF `plusPtr` 1)
  poke ptr (Point x y) = do
    let ptrF = castPtr ptr :: Ptr Float
    Storable.poke ptrF x
    Storable.poke (ptrF `plusPtr` 1) y

sizeOfFloat = Storable.sizeOf (undefined :: Float)

distanceSq :: Point -> Point -> Float
distanceSq (Point x1 y1) (Point x2 y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

hsin :: Float -> Float
hsin t = sin (t/2) ^ 2

distanceRad :: Float -> Point -> Point -> Float
distanceRad r (Point lat1 lon1) (Point lat2 lon2) =
  2*r*asin(min 1.0 root)
    where
      root = sqrt (hlat + cos lat1 * cos lat2 * hlon)
      hlat = hsin (lat2 - lat1)
      hlon = hsin (lon2 - lon1)

earthRadius :: Float
earthRadius = 6372.8

distanceEarth :: Point -> Point -> Float
distanceEarth a b = distanceRad earthRadius (convToDeg a) (convToDeg b)
  where
    convToDeg (Point lat lon) = Point (lat*pi/180) (lon*pi/180)
