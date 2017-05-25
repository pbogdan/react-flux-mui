{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module React.Flux.Mui.Gen.Types where

import Protolude

import Data.Aeson

data MuiProp = MuiProp
  { muiPropName :: Text
  , muiPropSig :: Text
  , muiPropArgSig :: Text
  , muiPropDefault :: Maybe Text
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON MuiProp where
instance ToJSON MuiProp where


newtype ModuleName = ModuleName
  { unModuleName :: Text
  } deriving (Eq, Generic, IsString, Ord, Show)

instance FromJSON ModuleName where
  parseJSON (String s) = pure . ModuleName $ s
  parseJSON _ = mzero

instance ToJSON ModuleName where
  toJSON (ModuleName n) = String n

data MuiModule = MuiModule
  { muiModuleName :: ModuleName
  , muiModuleComponentName :: Text
  , muiModuleProps :: [MuiProp]
  , muiModuleExtraImports :: [Text]
  , muiModuleExtraExtensions :: [Text]
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON MuiModule where
instance ToJSON MuiModule where

  
