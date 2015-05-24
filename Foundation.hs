{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Yesod.Core
import Graph
import Types
import qualified Data.IntMap as IM

data App = App
    { appGraph :: Graph
    , appStationPaths :: IM.IntMap StationPaths
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
    makeSessionBackend _ = return Nothing
