{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Drawer where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data Drawer = Drawer {
    drawerClassName :: !(Maybe Text)
    , drawerContainerClassName :: !(Maybe Text)
    , drawerDisableSwipeToOpen :: !(Maybe Bool)
    , drawerDocked :: !(Maybe Bool)
    , drawerOpen :: !(Maybe Bool)
    , drawerOpenSecondary :: !(Maybe Bool)
    , drawerOverlayClassName :: !(Maybe Text)
    , drawerSwipeAreaWidth :: !(Maybe Integer)
    , drawerWidth :: !(Maybe Integer)
} deriving (Generic, Show)

instance ToJSON Drawer where
  toJSON = genericToJSON $ aesonDrop (length ("Drawer" :: String)) snakeCase


defDrawer ::
 Drawer
defDrawer  =
  Drawer {
      drawerClassName = Nothing
      , drawerContainerClassName = Nothing
      , drawerDisableSwipeToOpen = Just False
      , drawerDocked = Just True
      , drawerOpen = Nothing
      , drawerOpenSecondary = Just False
      , drawerOverlayClassName = Nothing
      , drawerSwipeAreaWidth = Just 30
      , drawerWidth = Nothing
  }

drawer_ ::
  Drawer ->
  [PropertyOrHandler handler] ->
  ReactElementM handler () ->
  ReactElementM handler ()
drawer_ args props =
   foreign_
   "Drawer"
   (fromMaybe [] (toProps args) ++ props)
