{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Chip where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data Chip = Chip
  { chipBackgroundColor :: !(Maybe Text)
  , chipLabelColor :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON Chip where
  toJSON = genericToJSON $ aesonDrop (length ("Chip" :: String)) snakeCase

defChip :: Chip
defChip = Chip {chipBackgroundColor = Nothing, chipLabelColor = Nothing}

chip_ ::
     Chip
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
chip_ args props = foreign_ "Chip" (fromMaybe [] (toProps args) ++ props)
