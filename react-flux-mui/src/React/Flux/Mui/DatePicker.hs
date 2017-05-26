{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.DatePicker where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data DatePicker = DatePicker
  { datePickerAutoOk :: !(Maybe Bool)
  , datePickerClassName :: !(Maybe Text)
  , datePickerContainer :: !(Maybe (MuiSymbolEnum '[ "dialog", "inline"]))
  , datePickerDisableYearSelection :: !(Maybe Bool)
  , datePickerDisabled :: !(Maybe Bool)
  , datePickerFirstDayOfWeek :: !(Maybe Integer)
  , datePickerLocale :: !(Maybe Text)
  , datePickerMode :: !(Maybe (MuiSymbolEnum '[ "portrait", "landscape"]))
  } deriving (Generic, Show)

instance ToJSON DatePicker where
  toJSON = genericToJSON $ aesonDrop (length ("DatePicker" :: String)) camelCase

defDatePicker :: (Maybe (MuiSymbolEnum '[ "dialog", "inline"])) -> DatePicker
defDatePicker datePickerContainer_ =
  DatePicker
  { datePickerAutoOk = Just False
  , datePickerClassName = Nothing
  , datePickerContainer = datePickerContainer_
  , datePickerDisableYearSelection = Just False
  , datePickerDisabled = Just False
  , datePickerFirstDayOfWeek = Just 1
  , datePickerLocale = Nothing
  , datePickerMode = Nothing
  }

datePicker_ ::
     DatePicker
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
datePicker_ args props =
  foreign_ "DatePicker" (fromMaybe [] (toProps args) ++ props)
