{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.Toggle where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data Toggle = Toggle
  { toggleDefaultToggled :: !(Maybe Bool)
  , toggleDisabled :: !(Maybe Bool)
  , toggleLabelPosition :: !(Maybe (MuiSymbolEnum '[ "left", "right"]))
  , toggleToggled :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON Toggle where
  toJSON = genericToJSON $ aesonDrop (length ("Toggle" :: String)) camelCase

defToggle :: Toggle
defToggle =
  Toggle
  { toggleDefaultToggled = Just False
  , toggleDisabled = Just False
  , toggleLabelPosition = Just (MuiSymbolEnum (Proxy :: Proxy "left"))
  , toggleToggled = Nothing
  }

toggle_ ::
     Toggle
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
toggle_ args props = foreign_ "Toggle" (fromMaybe [] (toProps args) ++ props)
