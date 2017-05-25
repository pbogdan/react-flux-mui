{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.TimePicker where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util
import React.Flux.Mui.Types

data TimePicker = TimePicker {
    timePickerAutoOk :: !(Maybe Bool)
    , timePickerDisabled :: !(Maybe Bool)
    , timePickerFormat :: !(Maybe (MuiSymbolEnum '["ampm", "24hr"]))
    , timePickerPedantic :: !(Maybe Bool)
} deriving (Generic, Show)

instance ToJSON TimePicker where
  toJSON = genericToJSON $ aesonDrop (length ("TimePicker" :: String)) snakeCase


defTimePicker ::
    (Maybe (MuiSymbolEnum '["ampm", "24hr"])) ->
 TimePicker
defTimePicker timePickerFormat_  =
  TimePicker {
      timePickerAutoOk = Just False
      , timePickerDisabled = Just False
      , timePickerFormat = timePickerFormat_
      , timePickerPedantic = Just False
  }

timePicker_ ::
  TimePicker ->
  [PropertyOrHandler handler] ->
  ReactElementM handler () ->
  ReactElementM handler ()
timePicker_ args props =
   foreign_
   "TimePicker"
   (fromMaybe [] (toProps args) ++ props)
