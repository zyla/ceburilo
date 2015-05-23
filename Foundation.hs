{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Yesod.Core
import Graph
import Types

data App = App
    { appGraph :: Graph
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
    makeSessionBackend _ = return Nothing
