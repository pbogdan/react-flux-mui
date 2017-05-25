{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Table where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data Table = Table
  { tableAllRowsSelected :: !(Maybe Bool)
  , tableClassName :: !(Maybe Text)
  , tableFixedFooter :: !(Maybe Bool)
  , tableFixedHeader :: !(Maybe Bool)
  , tableHeight :: !(Maybe Text)
  , tableMultiSelectable :: !(Maybe Bool)
  , tableSelectable :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON Table where
  toJSON = genericToJSON $ aesonDrop (length ("Table" :: String)) snakeCase

defTable :: Table
defTable =
  Table
  { tableAllRowsSelected = Just False
  , tableClassName = Nothing
  , tableFixedFooter = Just True
  , tableFixedHeader = Just True
  , tableHeight = Just "inherit"
  , tableMultiSelectable = Just False
  , tableSelectable = Just True
  }

table_ ::
     Table
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
table_ args props = foreign_ "Table" (fromMaybe [] (toProps args) ++ props)
