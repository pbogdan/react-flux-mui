{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.Stepper.StepButton where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data StepButton = StepButton
  { stepButtonActive :: !(Maybe Bool)
  , stepButtonCompleted :: !(Maybe Bool)
  , stepButtonDisabled :: !(Maybe Bool)
  , stepButtonLast :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON StepButton where
  toJSON = genericToJSON $ aesonDrop (length ("StepButton" :: String)) snakeCase

defStepButton :: StepButton
defStepButton =
  StepButton
  { stepButtonActive = Nothing
  , stepButtonCompleted = Nothing
  , stepButtonDisabled = Nothing
  , stepButtonLast = Nothing
  }

stepButton_ ::
     StepButton
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
stepButton_ args props =
  foreign_ "StepButton" (fromMaybe [] (toProps args) ++ props)
