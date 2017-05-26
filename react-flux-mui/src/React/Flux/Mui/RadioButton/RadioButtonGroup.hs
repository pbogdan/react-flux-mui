{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.RadioButton.RadioButtonGroup where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data RadioButtonGroup = RadioButtonGroup
  { radioButtonGroupClassName :: !(Maybe Text)
  , radioButtonGroupLabelPosition :: !(Maybe (MuiSymbolEnum '[ "left", "right"]))
  , radioButtonGroupName :: !Text
  } deriving (Generic, Show)

instance ToJSON RadioButtonGroup where
  toJSON =
    genericToJSON $ aesonDrop (length ("RadioButtonGroup" :: String)) camelCase

defRadioButtonGroup :: Text -> RadioButtonGroup
defRadioButtonGroup radioButtonGroupName_ =
  RadioButtonGroup
  { radioButtonGroupClassName = Nothing
  , radioButtonGroupLabelPosition = Nothing
  , radioButtonGroupName = radioButtonGroupName_
  }

radioButtonGroup_ ::
     RadioButtonGroup
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
radioButtonGroup_ args props =
  foreign_ "RadioButtonGroup" (fromMaybe [] (toProps args) ++ props)
