{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Dialog where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data Dialog = Dialog
  { dialogActionsContainerClassName :: !(Maybe Text)
  , dialogAutoDetectWindowHeight :: !(Maybe Bool)
  , dialogAutoScrollBodyContent :: !(Maybe Bool)
  , dialogBodyClassName :: !(Maybe Text)
  , dialogClassName :: !(Maybe Text)
  , dialogContentClassName :: !(Maybe Text)
  , dialogModal :: !(Maybe Bool)
  , dialogOpen :: !Bool
  , dialogOverlayClassName :: !(Maybe Text)
  , dialogRepositionOnUpdate :: !(Maybe Bool)
  , dialogTitleClassName :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON Dialog where
  toJSON = genericToJSON $ aesonDrop (length ("Dialog" :: String)) snakeCase

defDialog :: Bool -> Dialog
defDialog dialogOpen_ =
  Dialog
  { dialogActionsContainerClassName = Nothing
  , dialogAutoDetectWindowHeight = Just True
  , dialogAutoScrollBodyContent = Just False
  , dialogBodyClassName = Nothing
  , dialogClassName = Nothing
  , dialogContentClassName = Nothing
  , dialogModal = Just False
  , dialogOpen = dialogOpen_
  , dialogOverlayClassName = Nothing
  , dialogRepositionOnUpdate = Just True
  , dialogTitleClassName = Nothing
  }

dialog_ ::
     Dialog
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
dialog_ args props = foreign_ "Dialog" (fromMaybe [] (toProps args) ++ props)
