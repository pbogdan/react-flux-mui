{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.Divider where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data Divider = Divider
  { dividerInset :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON Divider where
  toJSON = genericToJSON $ aesonDrop (length ("Divider" :: String)) snakeCase

defDivider :: Divider
defDivider = Divider {dividerInset = Just False}

divider_ ::
     Divider
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
divider_ args props = foreign_ "Divider" (fromMaybe [] (toProps args) ++ props)
