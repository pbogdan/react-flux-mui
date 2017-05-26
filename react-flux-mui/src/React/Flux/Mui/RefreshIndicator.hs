{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.RefreshIndicator where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data RefreshIndicator = RefreshIndicator
  { refreshIndicatorColor :: !(Maybe Text)
  , refreshIndicatorLeft :: !Integer
  , refreshIndicatorLoadingColor :: !(Maybe Text)
  , refreshIndicatorPercentage :: !(Maybe Integer)
  , refreshIndicatorSize :: !(Maybe Integer)
  , refreshIndicatorStatus :: !(Maybe (MuiSymbolEnum '[ "ready", "loading", "hide"]))
  , refreshIndicatorTop :: !Integer
  } deriving (Generic, Show)

instance ToJSON RefreshIndicator where
  toJSON =
    genericToJSON $ aesonDrop (length ("RefreshIndicator" :: String)) camelCase

defRefreshIndicator ::
     Integer
  -> (Maybe (MuiSymbolEnum '[ "ready", "loading", "hide"]))
  -> Integer
  -> RefreshIndicator
defRefreshIndicator refreshIndicatorLeft_ refreshIndicatorStatus_ refreshIndicatorTop_ =
  RefreshIndicator
  { refreshIndicatorColor = Nothing
  , refreshIndicatorLeft = refreshIndicatorLeft_
  , refreshIndicatorLoadingColor = Nothing
  , refreshIndicatorPercentage = Just 0
  , refreshIndicatorSize = Just 40
  , refreshIndicatorStatus = refreshIndicatorStatus_
  , refreshIndicatorTop = refreshIndicatorTop_
  }

refreshIndicator_ ::
     RefreshIndicator
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
refreshIndicator_ args props =
  foreign_ "RefreshIndicator" (fromMaybe [] (toProps args) ++ props)
