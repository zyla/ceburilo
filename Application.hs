{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Application where

import Import
import Yesod.Core

import Handler.Home
import Handler.Route

mkYesodDispatch "App" resourcesApp
