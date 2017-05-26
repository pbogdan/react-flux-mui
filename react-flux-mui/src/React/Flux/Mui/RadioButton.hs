{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.RadioButton where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data RadioButton = RadioButton
  { radioButtonChecked :: !(Maybe Bool)
  , radioButtonDisabled :: !(Maybe Bool)
  , radioButtonLabelPosition :: !(Maybe (MuiSymbolEnum '[ "left", "right"]))
  } deriving (Generic, Show)

instance ToJSON RadioButton where
  toJSON =
    genericToJSON $ aesonDrop (length ("RadioButton" :: String)) camelCase

defRadioButton :: RadioButton
defRadioButton =
  RadioButton
  { radioButtonChecked = Just False
  , radioButtonDisabled = Just False
  , radioButtonLabelPosition = Just (MuiSymbolEnum (Proxy :: Proxy "right"))
  }

radioButton_ ::
     RadioButton
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
radioButton_ args props =
  foreign_ "RadioButton" (fromMaybe [] (toProps args) ++ props)
