{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Yesod.Core
import Graph
import Types
import qualified Data.Map as M

data App = App
    { appGraph :: Graph
    , appStationPaths :: M.Map StationNumber StationPaths
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
    makeSessionBackend _ = return Nothing
