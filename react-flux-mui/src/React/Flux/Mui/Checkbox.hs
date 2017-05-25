{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.Checkbox where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util
import React.Flux.Mui.Types

data Checkbox = Checkbox {
    checkboxChecked :: !(Maybe Bool)
    , checkboxDefaultChecked :: !(Maybe Bool)
    , checkboxDisabled :: !(Maybe Bool)
    , checkboxLabelPosition :: !(Maybe (MuiSymbolEnum '["left", "right"]))
} deriving (Generic, Show)

instance ToJSON Checkbox where
  toJSON = genericToJSON $ aesonDrop (length ("Checkbox" :: String)) snakeCase


defCheckbox ::
    (Maybe (MuiSymbolEnum '["left", "right"])) ->
 Checkbox
defCheckbox checkboxLabelPosition_  =
  Checkbox {
      checkboxChecked = Nothing
      , checkboxDefaultChecked = Nothing
      , checkboxDisabled = Just False
      , checkboxLabelPosition = checkboxLabelPosition_
  }

checkbox_ ::
  Checkbox ->
  [PropertyOrHandler handler] ->
  ReactElementM handler () ->
  ReactElementM handler ()
checkbox_ args props =
   foreign_
   "Checkbox"
   (fromMaybe [] (toProps args) ++ props)
