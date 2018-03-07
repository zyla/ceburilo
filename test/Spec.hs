module Main where

import Test.Hspec
import Ceburilo.Graph
import qualified Data.IntMap as IM

main = hspec $ do
  describe "generateRoute" $ do
    it "meh" $ do
      let testGraph = mkGraph
            [ (1, [ (2, 1000) ])
            , (2, [ (3, 1000) ])
            ]

      generateRoute testGraph 1 3 `shouldBe` Just [1,2,3]

mkGraph :: [(StationNumber,
             [ (StationNumber, Distance) ]
            )
           ] -> Graph
mkGraph nodes = _ . _xd IM.fromList
