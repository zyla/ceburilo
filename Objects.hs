module Objects where

import Data.Aeson.TH
import Utils


data RouteView = RouteView
    {
        rv_data :: String
--        rv_routePoints :: [LocationPoint]
    }

deriveToJSON jsonOptions ''RouteView
