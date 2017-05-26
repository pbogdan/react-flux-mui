{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Stepper.StepLabel where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data StepLabel = StepLabel
  { stepLabelActive :: !(Maybe Bool)
  , stepLabelCompleted :: !(Maybe Bool)
  , stepLabelDisabled :: !(Maybe Bool)
  , stepLabelLast :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON StepLabel where
  toJSON = genericToJSON $ aesonDrop (length ("StepLabel" :: String)) camelCase

defStepLabel :: StepLabel
defStepLabel =
  StepLabel
  { stepLabelActive = Nothing
  , stepLabelCompleted = Nothing
  , stepLabelDisabled = Nothing
  , stepLabelLast = Nothing
  }

stepLabel_ ::
     StepLabel
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
stepLabel_ args props =
  foreign_ "StepLabel" (fromMaybe [] (toProps args) ++ props)
