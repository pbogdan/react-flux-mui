{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Toolbar.ToolbarSeparator where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data ToolbarSeparator = ToolbarSeparator
  { toolbarSeparatorClassName :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON ToolbarSeparator where
  toJSON =
    genericToJSON $ aesonDrop (length ("ToolbarSeparator" :: String)) snakeCase

defToolbarSeparator :: ToolbarSeparator
defToolbarSeparator = ToolbarSeparator {toolbarSeparatorClassName = Nothing}

toolbarSeparator_ ::
     ToolbarSeparator
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
toolbarSeparator_ args props =
  foreign_ "ToolbarSeparator" (fromMaybe [] (toProps args) ++ props)
