{-# LANGUAGE ScopedTypeVariables, OverloadedLists #-}
module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import qualified Data.IntMap as IM

import Ceburilo.Graph
import Ceburilo.Types

import Data.Packer

type Ptr = Word32

type TwoStagePacking = Packing (Packing ())

putPath :: Path -> Packing (Packing ())
putPath (Path distance time m_points) = do
  let points = maybe [] id m_points 

  putFloat32LE distance
  putFloat32LE time
  hole <- putPtrHole

  return $ do
    fillPtrHole hole
    putVectorWith putPoint points

putPtrHole :: Packing (Hole Ptr)
putPtrHole = putHoleWord32LE

fillPtrHole :: Hole Ptr -> Packing ()
fillPtrHole hole = do
  offset <- fromIntegral <$> packGetPosition
  fillHole hole offset

putVectorWith :: Foldable t => (a -> Packing ()) -> t a -> Packing ()
putVectorWith putItem v = do
  putWord32LE $ fromIntegral $ length v
  mapM_ putItem v

putVectorTwoStageWith :: Traversable t => (a -> TwoStagePacking) -> t a -> Packing ()
putVectorTwoStageWith putItem v = do
  putWord32LE $ fromIntegral $ length v
  stage2 <- mapM putItem v
  sequence_ stage2

putPoint :: Point -> Packing ()
putPoint (Point lat lon) = do
  putFloat32LE lat
  putFloat32LE lon

maxSize :: Int
maxSize = 30 * 1024 * 1024

data Packed a = Packed { packed_storage :: !ByteString, packed_location :: !Int }

readPacked :: Unpacking a -> Int -> Packed b -> a
readPacked unpack offset (Packed bs location) =
  runUnpacking
    (do unpackSetPosition (offset + location); unpack)
    bs

pp_distance :: Packed Path -> Float
pp_distance = readPacked getFloat32LE 0

pp_time :: Packed Path -> Float
pp_time = readPacked getFloat32LE 4

pp_points :: Packed Path -> Packed [Point]
pp_points = readPackedPtr 8

point_latitude :: Packed Point -> Float
point_latitude = readPacked getFloat32LE 0

point_longitude :: Packed Point -> Float
point_longitude = readPacked getFloat32LE 4

readPackedPtr :: Int -> Packed a -> Packed b
readPackedPtr offset packed@(Packed bs _) =
  let location = fromIntegral $ readPacked getWord32LE offset packed
  in Packed bs location

atOffset :: Int -> Packed a -> Packed b
atOffset off (Packed bs location) = Packed bs (location + off)

readVector :: Int -- ^ size
  -> Packed [a] -> [Packed a]
readVector size packed =
  let len = fromIntegral $ readPacked getWord32LE 0 packed
      atIndex index = atOffset (index * size + 4) packed
  in map atIndex [0..len-1]

main :: IO ()
main = do
{-
    Just (paths :: [StationPaths]) <- parseJSONFromFile "paths.json"

    let bs = runPacking maxSize $
          putVectorTwoStageWith putPath $ concatMap (map snd . IM.toList . spPaths) paths
-}

    bs <- B.readFile "paths.dat"

    let packed :: Packed [Path]
        packed = Packed bs 0

        vec = take 10 $ readVector 12 packed

    mapM_ print $ map point_latitude $ readVector 8 $ pp_points $ head vec
