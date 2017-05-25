{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Popover.PopoverAnimationVertical where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data PopoverAnimationVertical = PopoverAnimationVertical
  { popoverAnimationVerticalClassName :: !(Maybe Text)
  , popoverAnimationVerticalOpen :: !Bool
  } deriving (Generic, Show)

instance ToJSON PopoverAnimationVertical where
  toJSON =
    genericToJSON $
    aesonDrop (length ("PopoverAnimationVertical" :: String)) snakeCase

defPopoverAnimationVertical :: Bool -> PopoverAnimationVertical
defPopoverAnimationVertical popoverAnimationVerticalOpen_ =
  PopoverAnimationVertical
  { popoverAnimationVerticalClassName = Nothing
  , popoverAnimationVerticalOpen = popoverAnimationVerticalOpen_
  }

popoverAnimationVertical_ ::
     PopoverAnimationVertical
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
popoverAnimationVertical_ args props =
  foreign_ "PopoverAnimationVertical" (fromMaybe [] (toProps args) ++ props)
