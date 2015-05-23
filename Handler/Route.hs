{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Route where

import Import
import Yesod.Core
import Types

getRouteR :: Handler Value
getRouteR = return $ toJSON $ RouteView "asdasd"
