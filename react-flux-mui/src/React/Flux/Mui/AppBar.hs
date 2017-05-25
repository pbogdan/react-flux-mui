{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.AppBar where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data AppBar = AppBar {
    appBarClassName :: !(Maybe Text)
    , appBarIconClassNameLeft :: !(Maybe Text)
    , appBarIconClassNameRight :: !(Maybe Text)
    , appBarShowMenuIconButton :: !(Maybe Bool)
} deriving (Generic, Show)

instance ToJSON AppBar where
  toJSON = genericToJSON $ aesonDrop (length ("AppBar" :: String)) snakeCase


defAppBar ::
 AppBar
defAppBar  =
  AppBar {
      appBarClassName = Nothing
      , appBarIconClassNameLeft = Nothing
      , appBarIconClassNameRight = Nothing
      , appBarShowMenuIconButton = Just True
  }

appBar_ ::
  AppBar ->
  [PropertyOrHandler handler] ->
  ReactElementM handler () ->
  ReactElementM handler ()
appBar_ args props =
   foreign_
   "AppBar"
   (fromMaybe [] (toProps args) ++ props)
