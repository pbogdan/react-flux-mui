{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.IconMenu where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data IconMenu = IconMenu
  { iconMenuAnimated :: !(Maybe Bool)
  , iconMenuClassName :: !(Maybe Text)
  , iconMenuMultiple :: !(Maybe Bool)
  , iconMenuOpen :: !(Maybe Bool)
  , iconMenuTouchTapCloseDelay :: !(Maybe Integer)
  , iconMenuUseLayerForClickAway :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON IconMenu where
  toJSON = genericToJSON $ aesonDrop (length ("IconMenu" :: String)) snakeCase

defIconMenu :: IconMenu
defIconMenu =
  IconMenu
  { iconMenuAnimated = Just True
  , iconMenuClassName = Nothing
  , iconMenuMultiple = Just False
  , iconMenuOpen = Nothing
  , iconMenuTouchTapCloseDelay = Just 200
  , iconMenuUseLayerForClickAway = Just False
  }

iconMenu_ ::
     IconMenu
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
iconMenu_ args props =
  foreign_ "IconMenu" (fromMaybe [] (toProps args) ++ props)
