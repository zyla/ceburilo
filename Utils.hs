module Utils
    (
    jsonOptions
    ) where
import Data.Char (isUpper, toLower)
import Data.Aeson.TH



-- Options to use with deriveJSON
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
