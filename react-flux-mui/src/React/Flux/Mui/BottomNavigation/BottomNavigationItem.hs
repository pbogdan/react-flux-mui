{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.BottomNavigation.BottomNavigationItem where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data BottomNavigationItem = BottomNavigationItem
  {
  } deriving (Generic, Show)

instance ToJSON BottomNavigationItem where
  toJSON =
    genericToJSON $
    aesonDrop (length ("BottomNavigationItem" :: String)) snakeCase

defBottomNavigationItem :: BottomNavigationItem
defBottomNavigationItem = BottomNavigationItem {}

bottomNavigationItem_ ::
     BottomNavigationItem
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
bottomNavigationItem_ args props =
  foreign_ "BottomNavigationItem" (fromMaybe [] (toProps args) ++ props)
