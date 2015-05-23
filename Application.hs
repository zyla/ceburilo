{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Application where

import Foundation
import Yesod.Core

import Handler.Home

mkYesodDispatch "App" resourcesApp
