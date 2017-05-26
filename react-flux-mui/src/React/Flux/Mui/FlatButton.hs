{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.FlatButton where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data FlatButton = FlatButton
  { flatButtonBackgroundColor :: !(Maybe Text)
  , flatButtonDisabled :: !(Maybe Bool)
  , flatButtonHoverColor :: !(Maybe Text)
  , flatButtonHref :: !(Maybe Text)
  , flatButtonLabelPosition :: !(Maybe (MuiSymbolEnum '[ "before", "after"]))
  , flatButtonPrimary :: !(Maybe Bool)
  , flatButtonRippleColor :: !(Maybe Text)
  , flatButtonSecondary :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON FlatButton where
  toJSON = genericToJSON $ aesonDrop (length ("FlatButton" :: String)) camelCase

defFlatButton :: (Maybe (MuiSymbolEnum '[ "before", "after"])) -> FlatButton
defFlatButton flatButtonLabelPosition_ =
  FlatButton
  { flatButtonBackgroundColor = Nothing
  , flatButtonDisabled = Just False
  , flatButtonHoverColor = Nothing
  , flatButtonHref = Nothing
  , flatButtonLabelPosition = flatButtonLabelPosition_
  , flatButtonPrimary = Just False
  , flatButtonRippleColor = Nothing
  , flatButtonSecondary = Just False
  }

flatButton_ ::
     FlatButton
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
flatButton_ args props =
  foreign_ "FlatButton" (fromMaybe [] (toProps args) ++ props)
