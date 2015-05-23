{-# LANGUAGE OverloadedStrings #-}
module Utils
    (
    jsonOptions,
    requireGetParam
    ) where
import Data.Char (isUpper, toLower)
import Data.Aeson.TH
import Yesod.Core
import Import
import Data.Text
import Data.Maybe
import Control.Monad
import Control.Applicative



-- Options to use with deriveJSON
requireGetParam :: MonadHandler m => Text -> m Text
requireGetParam paramName = do
    param <- lookupGetParam paramName
    fromMaybe
        (sendResponseStatus status400 (append ("Missing parameter " :: Text) paramName))
        (return <$> param)


jsonOptions :: Options
jsonOptions = defaultOptions
    { fieldLabelModifier = fieldNameToJSON
    , allNullaryToStringTag = True }

-- Convert Haskell field name like prefix_fieldName to field_name.
fieldNameToJSON :: String -> String
fieldNameToJSON fieldName = camelToSnake $ dropPrefix fieldName
    where dropPrefix [] = fieldName
          dropPrefix ('_':xs) = xs
          dropPrefix (_:xs) = dropPrefix xs

          camelToSnake (x:xs)
            | isUpper x = '_' : Data.Char.toLower x : camelToSnake xs
            | otherwise = x : camelToSnake xs
          camelToSnake [] = []
