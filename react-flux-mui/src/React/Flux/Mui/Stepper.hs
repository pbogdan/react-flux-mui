{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.Stepper where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data Stepper = Stepper
  { stepperActiveStep :: !(Maybe Integer)
  , stepperLinear :: !(Maybe Bool)
  , stepperOrientation :: !(Maybe (MuiSymbolEnum '[ "horizontal", "vertical"]))
  } deriving (Generic, Show)

instance ToJSON Stepper where
  toJSON = genericToJSON $ aesonDrop (length ("Stepper" :: String)) snakeCase

defStepper :: (Maybe (MuiSymbolEnum '[ "horizontal", "vertical"])) -> Stepper
defStepper stepperOrientation_ =
  Stepper
  { stepperActiveStep = Nothing
  , stepperLinear = Just True
  , stepperOrientation = stepperOrientation_
  }

stepper_ ::
     Stepper
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
stepper_ args props = foreign_ "Stepper" (fromMaybe [] (toProps args) ++ props)
