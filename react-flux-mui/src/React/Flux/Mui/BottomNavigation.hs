{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.BottomNavigation where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data BottomNavigation = BottomNavigation {
    bottomNavigationSelectedIndex :: !(Maybe Integer)
} deriving (Generic, Show)

instance ToJSON BottomNavigation where
  toJSON = genericToJSON $ aesonDrop (length ("BottomNavigation" :: String)) snakeCase


defBottomNavigation ::
 BottomNavigation
defBottomNavigation  =
  BottomNavigation {
      bottomNavigationSelectedIndex = Nothing
  }

bottomNavigation_ ::
  BottomNavigation ->
  [PropertyOrHandler handler] ->
  ReactElementM handler () ->
  ReactElementM handler ()
bottomNavigation_ args props =
   foreign_
   "BottomNavigation"
   (fromMaybe [] (toProps args) ++ props)
