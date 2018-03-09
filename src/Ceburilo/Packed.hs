module Ceburilo.Packed where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import qualified Data.IntMap as IM

import Data.Packer

import Debug.Trace

data Packed = Packed { packed_storage :: !ByteString, packed_location :: !Int }

readPacked :: Unpacking a -> Int -> Packed -> a
readPacked unpack offset (Packed bs location) =
  runUnpacking
    (do unpackSetPosition (offset + location); unpack)
    bs

readPackedPtr :: Int -> Packed -> Packed
readPackedPtr offset packed@(Packed bs _) =
  let location = fromIntegral $ readPacked getWord32LE offset packed
  in Packed bs location

readMaybePtr :: Int -> Packed ->  Maybe Packed
readMaybePtr offset packed@(Packed bs _) = 
  case fromIntegral (readPacked getWord32LE offset packed) of
    0        -> Nothing
    location -> Just (Packed bs location)

atOffset :: Int -> Packed -> Packed
atOffset off (Packed bs location) = Packed bs (location + off)

readVector
  :: Int -- ^ size
  -> (Packed -> a)
  -> Packed
  -> [a]
--readVector size decode packed | trace ("readVector " ++ show size ++ " " ++ show (packed_location packed)) False = undefined
readVector size decode packed =
  let len = fromIntegral $ readPacked getWord32LE 0 packed
      atIndex index = decode $ atOffset (index * size + 4) packed
  in map atIndex [0..len-1]
