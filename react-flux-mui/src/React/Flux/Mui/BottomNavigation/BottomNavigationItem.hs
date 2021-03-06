{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.BottomNavigation.BottomNavigationItem where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data BottomNavigationItem = BottomNavigationItem
  {
  } deriving (Generic, Show)

instance ToJSON BottomNavigationItem where
  toJSON =
    genericToJSON $
    aesonDrop (length ("BottomNavigationItem" :: String)) camelCase

defBottomNavigationItem :: BottomNavigationItem
defBottomNavigationItem = BottomNavigationItem {}

bottomNavigationItem_ ::
     BottomNavigationItem
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
bottomNavigationItem_ args props =
  foreign_ "BottomNavigationItem" (fromMaybe [] (toProps args) ++ props) mempty
