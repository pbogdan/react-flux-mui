{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.Paper where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data Paper = Paper
  { paperCircle :: !(Maybe Bool)
  , paperRounded :: !(Maybe Bool)
  , paperTransitionEnabled :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON Paper where
  toJSON = genericToJSON $ aesonDrop (length ("Paper" :: String)) snakeCase

defPaper :: Paper
defPaper =
  Paper
  { paperCircle = Just False
  , paperRounded = Just True
  , paperTransitionEnabled = Just True
  }

paper_ ::
     Paper
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
paper_ args props = foreign_ "Paper" (fromMaybe [] (toProps args) ++ props)
