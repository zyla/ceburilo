{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Route where

import Import
import Yesod.Core
import Types
import Utils
import Data.Aeson.TH
import Data.Text

data RouteRequest = RouteRequest
    {   rr_begPoint :: (Float, Float)
    ,   rr_destPoint :: (Float, Float)
    ,   rr_begining :: Text
    ,   rr_destination :: Text
    }

deriveFromJSON jsonOptions ''RouteRequest

data RouteView = RouteView
    {   rv_path :: Path
    ,   rv_begining :: Text
    ,   rv_destination :: Text
    }

deriveToJSON jsonOptions ''RouteView

getRouteR :: Handler Value
getRouteR = return $ toJSON $ RouteView path "Lokacja początkowa" "Cel podróży"
    where path = Path 30 234000
                    [   Instruction "Skręć prosto" (0, 3)
                    ,   Instruction "Zawróć" (3, 4)
                    ]
                    [    Point 52.235707713687624 20.996793508529663
                    ,    Point 52.24884675795982 21.005457043647766
                    ,    Point 52.230217725853215 21.01259171962738
                    ,    Point 52.2314251871654 21.0213263332844
                    ,    Point 52.23935090349061 21.01681351661682
                    ]
