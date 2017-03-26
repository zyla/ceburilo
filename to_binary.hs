{-# LANGUAGE ScopedTypeVariables, OverloadedLists, RecordWildCards #-}
module Main where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Data.Word
import qualified Data.IntMap as IM

import Ceburilo.Graph
import Ceburilo.Types

import Data.Packer

type Ptr = Word32

type TwoStagePacking = Packing (Packing ())

putStationPaths :: StationPaths -> TwoStagePacking
putStationPaths (StationPaths (Station number name location) paths) = do
  putWord32LE $ fromIntegral number
  hole_name <- putPtrHole
  putPoint location
  hole_paths <- putPtrHole

  return $ do
    fillPtrHole hole_name
    putVectorWith putWord8 $ B.unpack $ T.encodeUtf8 $ T.pack name
    fillPtrHole hole_paths
    putVectorTwoStageWith putPath $ IM.toList paths

putPath :: (StationNumber, Path) -> TwoStagePacking
putPath (number, Path distance time m_points) = do
  putWord32LE $ fromIntegral number

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
maxSize = 50 * 1024 * 1024

main :: IO ()
main = do
    Just (paths :: [StationPaths]) <- parseJSONFromFile "paths.json"

    let bs = runPacking maxSize $ putVectorTwoStageWith putStationPaths paths

    B.writeFile "paths.dat" bs
