{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.Badge where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data Badge = Badge
  { badgeClassName :: !(Maybe Text)
  , badgePrimary :: !(Maybe Bool)
  , badgeSecondary :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON Badge where
  toJSON = genericToJSON $ aesonDrop (length ("Badge" :: String)) snakeCase

defBadge :: Badge
defBadge =
  Badge
  { badgeClassName = Nothing
  , badgePrimary = Just False
  , badgeSecondary = Just False
  }

badge_ ::
     Badge
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
badge_ args props = foreign_ "Badge" (fromMaybe [] (toProps args) ++ props)
