{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.MenuItem where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data MenuItem = MenuItem
  { menuItemChecked :: !(Maybe Bool)
  , menuItemDesktop :: !(Maybe Bool)
  , menuItemDisabled :: !(Maybe Bool)
  , menuItemFocusState :: !(Maybe (MuiSymbolEnum '[ "none", "focused", "keyboard-focused"]))
  , menuItemInsetChildren :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON MenuItem where
  toJSON = genericToJSON $ aesonDrop (length ("MenuItem" :: String)) snakeCase

defMenuItem ::
     (Maybe (MuiSymbolEnum '[ "none", "focused", "keyboard-focused"]))
  -> MenuItem
defMenuItem menuItemFocusState_ =
  MenuItem
  { menuItemChecked = Just False
  , menuItemDesktop = Just False
  , menuItemDisabled = Just False
  , menuItemFocusState = menuItemFocusState_
  , menuItemInsetChildren = Just False
  }

menuItem_ ::
     MenuItem
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
menuItem_ args props =
  foreign_ "MenuItem" (fromMaybe [] (toProps args) ++ props)
