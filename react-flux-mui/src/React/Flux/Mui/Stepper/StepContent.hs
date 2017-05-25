{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Stepper.StepContent where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data StepContent = StepContent
  { stepContentActive :: !(Maybe Bool)
  , stepContentCompleted :: !(Maybe Bool)
  , stepContentLast :: !(Maybe Bool)
  , stepContentTransitionDuration :: !(Maybe Integer)
  } deriving (Generic, Show)

instance ToJSON StepContent where
  toJSON =
    genericToJSON $ aesonDrop (length ("StepContent" :: String)) snakeCase

defStepContent :: StepContent
defStepContent =
  StepContent
  { stepContentActive = Nothing
  , stepContentCompleted = Nothing
  , stepContentLast = Nothing
  , stepContentTransitionDuration = Just 450
  }

stepContent_ ::
     StepContent
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
stepContent_ args props =
  foreign_ "StepContent" (fromMaybe [] (toProps args) ++ props)
