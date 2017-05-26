{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Popover where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data Popover = Popover
  { popoverAnimated :: !(Maybe Bool)
  , popoverAutoCloseWhenOffScreen :: !(Maybe Bool)
  , popoverCanAutoPosition :: !(Maybe Bool)
  , popoverClassName :: !(Maybe Text)
  , popoverOpen :: !(Maybe Bool)
  , popoverUseLayerForClickAway :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON Popover where
  toJSON = genericToJSON $ aesonDrop (length ("Popover" :: String)) camelCase

defPopover :: Popover
defPopover =
  Popover
  { popoverAnimated = Just True
  , popoverAutoCloseWhenOffScreen = Just True
  , popoverCanAutoPosition = Just True
  , popoverClassName = Nothing
  , popoverOpen = Just False
  , popoverUseLayerForClickAway = Just True
  }

popover_ ::
     Popover
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
popover_ args props = foreign_ "Popover" (fromMaybe [] (toProps args) ++ props)
