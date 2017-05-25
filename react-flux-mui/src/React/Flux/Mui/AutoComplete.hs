{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.AutoComplete where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data AutoComplete = AutoComplete {
    autoCompleteAnimated :: !(Maybe Bool)
    , autoCompleteDisableFocusRipple :: !(Maybe Bool)
    , autoCompleteFullWidth :: !(Maybe Bool)
    , autoCompleteMaxSearchResults :: !(Maybe Integer)
    , autoCompleteMenuCloseDelay :: !(Maybe Integer)
    , autoCompleteOpen :: !(Maybe Bool)
    , autoCompleteOpenOnFocus :: !(Maybe Bool)
    , autoCompleteSearchText :: !(Maybe Text)
} deriving (Generic, Show)

instance ToJSON AutoComplete where
  toJSON = genericToJSON $ aesonDrop (length ("AutoComplete" :: String)) snakeCase


defAutoComplete ::
 AutoComplete
defAutoComplete  =
  AutoComplete {
      autoCompleteAnimated = Just True
      , autoCompleteDisableFocusRipple = Just True
      , autoCompleteFullWidth = Just False
      , autoCompleteMaxSearchResults = Nothing
      , autoCompleteMenuCloseDelay = Just 300
      , autoCompleteOpen = Just False
      , autoCompleteOpenOnFocus = Just False
      , autoCompleteSearchText = Just ""
  }

autoComplete_ ::
  AutoComplete ->
  [PropertyOrHandler handler] ->
  ReactElementM handler () ->
  ReactElementM handler ()
autoComplete_ args props =
   foreign_
   "AutoComplete"
   (fromMaybe [] (toProps args) ++ props)
