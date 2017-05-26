{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.List.ListItem where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data ListItem = ListItem
  { listItemAutoGenerateNestedIndicator :: !(Maybe Bool)
  , listItemDisableKeyboardFocus :: !(Maybe Bool)
  , listItemDisabled :: !(Maybe Bool)
  , listItemHoverColor :: !(Maybe Text)
  , listItemInitiallyOpen :: !(Maybe Bool)
  , listItemInsetChildren :: !(Maybe Bool)
  , listItemNestedLevel :: !(Maybe Integer)
  , listItemOpen :: !(Maybe Bool)
  , listItemPrimaryTogglesNestedList :: !(Maybe Bool)
  , listItemSecondaryTextLines :: !(Maybe (MuiNatEnum '[ 1, 2]))
  } deriving (Generic, Show)

instance ToJSON ListItem where
  toJSON = genericToJSON $ aesonDrop (length ("ListItem" :: String)) camelCase

defListItem :: (Maybe (MuiNatEnum '[ 1, 2])) -> ListItem
defListItem listItemSecondaryTextLines_ =
  ListItem
  { listItemAutoGenerateNestedIndicator = Just True
  , listItemDisableKeyboardFocus = Just False
  , listItemDisabled = Just False
  , listItemHoverColor = Nothing
  , listItemInitiallyOpen = Just False
  , listItemInsetChildren = Just False
  , listItemNestedLevel = Just 0
  , listItemOpen = Nothing
  , listItemPrimaryTogglesNestedList = Just False
  , listItemSecondaryTextLines = listItemSecondaryTextLines_
  }

listItem_ ::
     ListItem
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
listItem_ args props =
  foreign_ "ListItem" (fromMaybe [] (toProps args) ++ props)
