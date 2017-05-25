{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Styles.MuiThemeProvider where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data MuiThemeProvider = MuiThemeProvider {
} deriving (Generic, Show)

instance ToJSON MuiThemeProvider where
  toJSON = genericToJSON $ aesonDrop (length ("MuiThemeProvider" :: String)) snakeCase


defMuiThemeProvider ::
 MuiThemeProvider
defMuiThemeProvider  =
  MuiThemeProvider {
  }

muiThemeProvider_ ::
  MuiThemeProvider ->
  [PropertyOrHandler handler] ->
  ReactElementM handler () ->
  ReactElementM handler ()
muiThemeProvider_ args props =
   foreign_
   "MuiThemeProvider"
   (fromMaybe [] (toProps args) ++ props)
