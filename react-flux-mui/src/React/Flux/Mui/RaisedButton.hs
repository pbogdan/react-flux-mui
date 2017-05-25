{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.RaisedButton where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data RaisedButton = RaisedButton
  { raisedButtonBackgroundColor :: !(Maybe Text)
  , raisedButtonClassName :: !(Maybe Text)
  , raisedButtonDisabled :: !(Maybe Bool)
  , raisedButtonDisabledBackgroundColor :: !(Maybe Text)
  , raisedButtonDisabledLabelColor :: !(Maybe Text)
  , raisedButtonFullWidth :: !(Maybe Bool)
  , raisedButtonHref :: !(Maybe Text)
  , raisedButtonLabelColor :: !(Maybe Text)
  , raisedButtonLabelPosition :: !(Maybe (MuiSymbolEnum '[ "before", "after"]))
  , raisedButtonPrimary :: !(Maybe Bool)
  , raisedButtonSecondary :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON RaisedButton where
  toJSON =
    genericToJSON $ aesonDrop (length ("RaisedButton" :: String)) snakeCase

defRaisedButton :: (Maybe (MuiSymbolEnum '[ "before", "after"])) -> RaisedButton
defRaisedButton raisedButtonLabelPosition_ =
  RaisedButton
  { raisedButtonBackgroundColor = Nothing
  , raisedButtonClassName = Nothing
  , raisedButtonDisabled = Just False
  , raisedButtonDisabledBackgroundColor = Nothing
  , raisedButtonDisabledLabelColor = Nothing
  , raisedButtonFullWidth = Just False
  , raisedButtonHref = Nothing
  , raisedButtonLabelColor = Nothing
  , raisedButtonLabelPosition = raisedButtonLabelPosition_
  , raisedButtonPrimary = Just False
  , raisedButtonSecondary = Just False
  }

raisedButton_ ::
     RaisedButton
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
raisedButton_ args props =
  foreign_ "RaisedButton" (fromMaybe [] (toProps args) ++ props)
