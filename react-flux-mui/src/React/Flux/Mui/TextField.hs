{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.TextField where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data TextField = TextField
  { textFieldClassName :: !(Maybe Text)
  , textFieldDisabled :: !(Maybe Bool)
  , textFieldFloatingLabelFixed :: !(Maybe Bool)
  , textFieldFullWidth :: !(Maybe Bool)
  , textFieldId :: !(Maybe Text)
  , textFieldMultiLine :: !(Maybe Bool)
  , textFieldName :: !(Maybe Text)
  , textFieldRows :: !(Maybe Integer)
  , textFieldRowsMax :: !(Maybe Integer)
  , textFieldType :: !(Maybe Text)
  , textFieldUnderlineShow :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON TextField where
  toJSON = genericToJSON $ aesonDrop (length ("TextField" :: String)) camelCase

defTextField :: TextField
defTextField =
  TextField
  { textFieldClassName = Nothing
  , textFieldDisabled = Just False
  , textFieldFloatingLabelFixed = Just False
  , textFieldFullWidth = Just False
  , textFieldId = Nothing
  , textFieldMultiLine = Just False
  , textFieldName = Nothing
  , textFieldRows = Just 1
  , textFieldRowsMax = Nothing
  , textFieldType = Just "text"
  , textFieldUnderlineShow = Just True
  }

textField_ ::
     TextField
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
textField_ args props =
  foreign_ "TextField" (fromMaybe [] (toProps args) ++ props)
