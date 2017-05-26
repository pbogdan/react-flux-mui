{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.LinearProgress where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data LinearProgress = LinearProgress
  { linearProgressColor :: !(Maybe Text)
  , linearProgressMax :: !(Maybe Integer)
  , linearProgressMin :: !(Maybe Integer)
  , linearProgressMode :: !(Maybe (MuiSymbolEnum '[ "determinate", "indeterminate"]))
  , linearProgressValue :: !(Maybe Integer)
  } deriving (Generic, Show)

instance ToJSON LinearProgress where
  toJSON =
    genericToJSON $ aesonDrop (length ("LinearProgress" :: String)) camelCase

defLinearProgress :: LinearProgress
defLinearProgress =
  LinearProgress
  { linearProgressColor = Nothing
  , linearProgressMax = Just 100
  , linearProgressMin = Just 0
  , linearProgressMode = Just (MuiSymbolEnum (Proxy :: Proxy "indeterminate"))
  , linearProgressValue = Just 0
  }

linearProgress_ ::
     LinearProgress -> [PropertyOrHandler handler] -> ReactElementM handler ()
linearProgress_ args props =
  foreign_ "LinearProgress" (fromMaybe [] (toProps args) ++ props) mempty
