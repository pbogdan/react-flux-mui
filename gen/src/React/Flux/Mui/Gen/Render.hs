{-# LANGUAGE FlexibleContexts #-}

module React.Flux.Mui.Gen.Render
  ( renderModule
  , renderCabal
  , renderMainModule
  , renderIndexJs
  ) where

import           Protolude

import           Control.Arrow ((&&&))
import           Control.Lens hiding ((&))
import           Data.Aeson
import qualified Data.ByteString as Bytes
import qualified Data.HashMap.Strict as HashMap
import           Data.String (String)
import           React.Flux.Mui.Gen.Types
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath.Lens
import           Text.EDE

renderModule
  :: (MonadIO m, ToJSON a)
  => FilePath -> a -> m (Either String ())
renderModule path module_ = do
  liftIO $
    createDirectoryIfMissing True $
    "../react-flux-mui/" <> toS path ^. directory
  let Just env = fromValue (toJSON module_)
  tpl <- liftIO $ eitherParseFile "templates/module.ede"
  let ret = flip eitherRender env =<< tpl
  case ret of
    Left e -> return . Left $ "Rendering " <> toS path <> " failed:" <> e
    Right out -> do
      liftIO $ Bytes.writeFile ("../react-flux-mui/" <> path) (toS out)
      return . Right $ ()


renderCabal
  :: MonadIO m
  => Text -> [MuiModule] -> m (Either String ())
renderCabal path modules = do
  let cabalModules =
        sort
          ("React.Flux.Mui" :
           "React.Flux.Mui.Util" :
           "React.Flux.Mui.Types" : map (unModuleName . muiModuleName) modules)
      Just env =
        fromValue . toJSON . HashMap.fromList $
        [("modules" :: Text, cabalModules)]
  tpl <- liftIO $ eitherParseFile "templates/cabal.ede"
  let ret = flip eitherRender env =<< tpl
  case ret of
    Left e -> return . Left $ "Rendering " <> toS path <> "failed: " <> e
    Right out -> do
      liftIO $ Bytes.writeFile (toS path) (toS out)
      return . Right $ ()

renderMainModule
  :: MonadIO m
  => Text -> [MuiModule] -> m (Either String ())
renderMainModule path modules = do
  let muiModules =
        sort
          ("React.Flux.Mui.Util" :
           "React.Flux.Mui.Types" : map (unModuleName . muiModuleName) modules)
      Just env =
        fromValue . toJSON . HashMap.fromList $
        [("modules" :: Text, muiModules)]
  tpl <- liftIO $ eitherParseFile "templates/mui.ede"
  let ret = flip eitherRender env =<< tpl
  case ret of
    Left e -> return . Left $ "Rendering " <> toS path <> "failed: " <> e
    Right out -> do
      liftIO $ Bytes.writeFile (toS path) (toS out)
      return . Right $ ()

renderIndexJs
  :: MonadIO m
  => Text -> [MuiModule] -> m (Either String ())
renderIndexJs path modules = do
  let Just env =
        fromValue
          (toJSON
             (HashMap.fromList
                [ ( "props" :: Text
                  , HashMap.fromList
                      (map
                         (muiModuleComponentName &&&
                          ("MaterialUI." <>) . muiModuleComponentName)
                         modules))
                ]))
  tpl <- liftIO $ eitherParseFile "templates/index.js.ede"
  let ret = flip eitherRender env =<< tpl
  case ret of
    Left e -> return . Left $ "Rendering " <> toS path <> "failed: " <> e
    Right out -> do
      liftIO $ Bytes.writeFile (toS path) (toS out)
      return . Right $ ()
