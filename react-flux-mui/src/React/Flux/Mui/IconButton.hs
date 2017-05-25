{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.IconButton where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data IconButton = IconButton
  { iconButtonClassName :: !(Maybe Text)
  , iconButtonDisableTouchRipple :: !(Maybe Bool)
  , iconButtonDisabled :: !(Maybe Bool)
  , iconButtonHref :: !(Maybe Text)
  , iconButtonIconClassName :: !(Maybe Text)
  , iconButtonTouch :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON IconButton where
  toJSON = genericToJSON $ aesonDrop (length ("IconButton" :: String)) snakeCase

defIconButton :: IconButton
defIconButton =
  IconButton
  { iconButtonClassName = Nothing
  , iconButtonDisableTouchRipple = Just False
  , iconButtonDisabled = Just False
  , iconButtonHref = Nothing
  , iconButtonIconClassName = Nothing
  , iconButtonTouch = Just False
  }

iconButton_ ::
     IconButton
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
iconButton_ args props =
  foreign_ "IconButton" (fromMaybe [] (toProps args) ++ props)
