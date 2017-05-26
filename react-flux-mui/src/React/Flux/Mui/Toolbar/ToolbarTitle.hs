{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Toolbar.ToolbarTitle where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data ToolbarTitle = ToolbarTitle
  { toolbarTitleClassName :: !(Maybe Text)
  , toolbarTitleText :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON ToolbarTitle where
  toJSON =
    genericToJSON $ aesonDrop (length ("ToolbarTitle" :: String)) camelCase

defToolbarTitle :: ToolbarTitle
defToolbarTitle =
  ToolbarTitle {toolbarTitleClassName = Nothing, toolbarTitleText = Nothing}

toolbarTitle_ ::
     ToolbarTitle
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
toolbarTitle_ args props =
  foreign_ "ToolbarTitle" (fromMaybe [] (toProps args) ++ props)
