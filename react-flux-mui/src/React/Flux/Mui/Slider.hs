{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.Slider where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data Slider = Slider
  { sliderAxis :: !(Maybe (MuiSymbolEnum '[ "x", "x-reverse", "y", "y-reverse"]))
  , sliderDisableFocusRipple :: !(Maybe Bool)
  , sliderDisabled :: !(Maybe Bool)
  , sliderName :: !(Maybe Text)
  , sliderRequired :: !(Maybe Bool)
  , sliderStep :: !(Maybe Integer)
  } deriving (Generic, Show)

instance ToJSON Slider where
  toJSON = genericToJSON $ aesonDrop (length ("Slider" :: String)) camelCase

defSlider ::
     (Maybe (MuiSymbolEnum '[ "x", "x-reverse", "y", "y-reverse"]))
  -> (Maybe Integer)
  -> Slider
defSlider sliderAxis_ sliderStep_ =
  Slider
  { sliderAxis = sliderAxis_
  , sliderDisableFocusRipple = Just False
  , sliderDisabled = Just False
  , sliderName = Nothing
  , sliderRequired = Just True
  , sliderStep = sliderStep_
  }

slider_ ::
     Slider
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
slider_ args props = foreign_ "Slider" (fromMaybe [] (toProps args) ++ props)
