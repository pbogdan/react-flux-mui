{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.CircularProgress where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util
import React.Flux.Mui.Types

data CircularProgress = CircularProgress {
    circularProgressColor :: !(Maybe Text)
    , circularProgressMax :: !(Maybe Integer)
    , circularProgressMin :: !(Maybe Integer)
    , circularProgressMode :: !(Maybe (MuiSymbolEnum '["determinate", "indeterminate"]))
    , circularProgressSize :: !(Maybe Integer)
    , circularProgressThickness :: !(Maybe Integer)
    , circularProgressValue :: !(Maybe Integer)
} deriving (Generic, Show)

instance ToJSON CircularProgress where
  toJSON = genericToJSON $ aesonDrop (length ("CircularProgress" :: String)) snakeCase


defCircularProgress ::
    (Maybe (MuiSymbolEnum '["determinate", "indeterminate"])) ->
    (Maybe Integer) ->
 CircularProgress
defCircularProgress circularProgressMode_ circularProgressThickness_  =
  CircularProgress {
      circularProgressColor = Nothing
      , circularProgressMax = Just 100
      , circularProgressMin = Just 0
      , circularProgressMode = circularProgressMode_
      , circularProgressSize = Just 40
      , circularProgressThickness = circularProgressThickness_
      , circularProgressValue = Just 0
  }

circularProgress_ ::
  CircularProgress ->
  [PropertyOrHandler handler] ->
  ReactElementM handler () ->
  ReactElementM handler ()
circularProgress_ args props =
   foreign_
   "CircularProgress"
   (fromMaybe [] (toProps args) ++ props)
