{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.SelectField where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data SelectField = SelectField
  { selectFieldAutoWidth :: !(Maybe Bool)
  , selectFieldDisabled :: !(Maybe Bool)
  , selectFieldFloatingLabelFixed :: !(Maybe Bool)
  , selectFieldFullWidth :: !(Maybe Bool)
  , selectFieldId :: !(Maybe Text)
  , selectFieldMaxHeight :: !(Maybe Integer)
  } deriving (Generic, Show)

instance ToJSON SelectField where
  toJSON =
    genericToJSON $ aesonDrop (length ("SelectField" :: String)) snakeCase

defSelectField :: SelectField
defSelectField =
  SelectField
  { selectFieldAutoWidth = Just False
  , selectFieldDisabled = Just False
  , selectFieldFloatingLabelFixed = Nothing
  , selectFieldFullWidth = Just False
  , selectFieldId = Nothing
  , selectFieldMaxHeight = Nothing
  }

selectField_ ::
     SelectField
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
selectField_ args props =
  foreign_ "SelectField" (fromMaybe [] (toProps args) ++ props)
