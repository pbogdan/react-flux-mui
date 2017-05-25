{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.Avatar where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data Avatar = Avatar
  { avatarBackgroundColor :: !(Maybe Text)
  , avatarClassName :: !(Maybe Text)
  , avatarColor :: !(Maybe Text)
  , avatarSize :: !(Maybe Integer)
  , avatarSrc :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON Avatar where
  toJSON = genericToJSON $ aesonDrop (length ("Avatar" :: String)) snakeCase

defAvatar :: Avatar
defAvatar =
  Avatar
  { avatarBackgroundColor = Nothing
  , avatarClassName = Nothing
  , avatarColor = Nothing
  , avatarSize = Just 40
  , avatarSrc = Nothing
  }

avatar_ ::
     Avatar
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
avatar_ args props = foreign_ "Avatar" (fromMaybe [] (toProps args) ++ props)
