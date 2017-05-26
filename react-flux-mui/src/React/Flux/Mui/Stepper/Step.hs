{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Stepper.Step where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data Step = Step
  { stepActive :: !(Maybe Bool)
  , stepCompleted :: !(Maybe Bool)
  , stepDisabled :: !(Maybe Bool)
  , stepIndex :: !(Maybe Integer)
  , stepLast :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON Step where
  toJSON = genericToJSON $ aesonDrop (length ("Step" :: String)) camelCase

defStep :: Step
defStep =
  Step
  { stepActive = Nothing
  , stepCompleted = Nothing
  , stepDisabled = Nothing
  , stepIndex = Nothing
  , stepLast = Nothing
  }

step_ ::
     Step
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
step_ args props = foreign_ "Step" (fromMaybe [] (toProps args) ++ props)
