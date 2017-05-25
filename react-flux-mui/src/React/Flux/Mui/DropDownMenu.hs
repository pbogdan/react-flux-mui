{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.DropDownMenu where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data DropDownMenu = DropDownMenu
  { dropDownMenuAnimated :: !(Maybe Bool)
  , dropDownMenuAutoWidth :: !(Maybe Bool)
  , dropDownMenuClassName :: !(Maybe Text)
  , dropDownMenuDisabled :: !(Maybe Bool)
  , dropDownMenuMaxHeight :: !(Maybe Integer)
  , dropDownMenuOpenImmediately :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON DropDownMenu where
  toJSON =
    genericToJSON $ aesonDrop (length ("DropDownMenu" :: String)) snakeCase

defDropDownMenu :: DropDownMenu
defDropDownMenu =
  DropDownMenu
  { dropDownMenuAnimated = Just True
  , dropDownMenuAutoWidth = Just True
  , dropDownMenuClassName = Nothing
  , dropDownMenuDisabled = Just False
  , dropDownMenuMaxHeight = Just 500
  , dropDownMenuOpenImmediately = Just False
  }

dropDownMenu_ ::
     DropDownMenu
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
dropDownMenu_ args props =
  foreign_ "DropDownMenu" (fromMaybe [] (toProps args) ++ props)
