{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Toolbar.ToolbarGroup where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data ToolbarGroup = ToolbarGroup {
    toolbarGroupClassName :: !(Maybe Text)
    , toolbarGroupFirstChild :: !(Maybe Bool)
    , toolbarGroupLastChild :: !(Maybe Bool)
} deriving (Generic, Show)

instance ToJSON ToolbarGroup where
  toJSON = genericToJSON $ aesonDrop (length ("ToolbarGroup" :: String)) snakeCase


defToolbarGroup ::
 ToolbarGroup
defToolbarGroup  =
  ToolbarGroup {
      toolbarGroupClassName = Nothing
      , toolbarGroupFirstChild = Just False
      , toolbarGroupLastChild = Just False
  }

toolbarGroup_ ::
  ToolbarGroup ->
  [PropertyOrHandler handler] ->
  ReactElementM handler () ->
  ReactElementM handler ()
toolbarGroup_ args props =
   foreign_
   "ToolbarGroup"
   (fromMaybe [] (toProps args) ++ props)
