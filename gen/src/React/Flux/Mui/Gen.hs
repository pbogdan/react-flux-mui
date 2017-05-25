{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module React.Flux.Mui.Gen
  ( generate
  )
where

import           Protolude

import           Control.Arrow ((>>>))
import           Control.Lens hiding ((.=), parts)
import           Data.Aeson
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8 as CharBytes
import           Data.Char (toLower, toUpper)
import           Data.List (nub)
import qualified Data.Text as Text
import           React.Docgen.Types
import           React.Flux.Mui.Gen.Render
import           React.Flux.Mui.Gen.Types
import           System.Directory

simplePropToMuiProp ::  Text -> Prop TypeSimple -> MuiProp
simplePropToMuiProp name prop =
  let propName_ = lcFirst name <> (ucFirst . view propName $ prop)
  in MuiProp
     { muiPropName = propName_
     , muiPropSig = strictType . propTypeHsType . view propType $ prop
     , muiPropArgSig =
         Text.replace "!" "" . propTypeHsType . view propType $ prop
     , muiPropDefault = resolveDefault prop
     }

liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

generate :: IO ()
generate = do
  s <- Bytes.readFile "../react-flux-mui/js/material-ui/material-ui/docgen.json"
  exposedModules <-
    map toS . CharBytes.lines <$>
    Bytes.readFile "../react-flux-mui/js/material-ui/module.list"
  let componentsOrErr = eitherDecodeStrict s
  case componentsOrErr of
    Left _ -> undefined
    Right (Components components) -> do
      modules <-
        for
          (filter
             (\component -> component ^. componentName `elem` exposedModules)
             components) $ \component -> do
          let moduleName = ModuleName . componentModuleName $ component
              props =
                sort .
                map (simplePropToMuiProp (component ^. componentName)) .
                altLefts . view componentProps $
                component
              module_ =
                MuiModule
                { muiModuleName = moduleName
                , muiModuleComponentName = component ^. componentName
                , muiModuleProps = props
                }
          return (componentOutputFile component, module_)
      ret <-
        runExceptT $ do
          liftEither =<<
            second mconcat . sequenceA <$>
            for modules (uncurry renderModule . first toS)
          liftEither =<<
            renderCabal
              "../react-flux-mui/react-flux-mui.cabal"
              (map snd modules)
          liftEither =<<
            renderMainModule
              "../react-flux-mui/src/React/Flux/Mui.hs"
              (map snd modules)
          liftEither =<<
            renderIndexJs "../react-flux-mui/js/index.js" (map snd modules)
          liftIO $
            copyFile
              "templates/Util.hs"
              "../react-flux-mui/src/React/Flux/Mui/Util.hs"
          liftIO $
            copyFile
              "templates/Types.hs"
              "../react-flux-mui/src/React/Flux/Mui/Types.hs"
      case ret of
        Left e -> print e
        Right _ -> return ()

componentOutputFile :: Component -> Text
componentOutputFile =
  view componentSourceFile >>>
  Text.replace "src/" "" >>>
  Text.replace ".js" "" >>>
  Text.splitOn "/" >>>
  map ucFirst >>>
  nub >>> Text.intercalate "/" >>> ("src/React/Flux/Mui/" <>) >>> (<> ".hs")

componentModuleName :: Component -> Text
componentModuleName =
  componentOutputFile >>>
  Text.replace "src/" "" >>>
  Text.replace ".hs" "" >>>
  Text.replace "/" "." >>>
  Text.splitOn "." >>> map ucFirst >>> intersperse "." >>> Text.concat

-- | Capitalize the first character of the given string.
ucFirst :: Text -> Text
ucFirst "" = ""
ucFirst t = Text.cons (toUpper $ Text.head t) (Text.tail t)

-- | Make the first character of the given string lowercase.
lcFirst :: Text -> Text
lcFirst "" = ""
lcFirst t = Text.cons (toLower $ Text.head t) (Text.tail t)

propTypeHsType :: PropType TypeSimple -> Text
propTypeHsType TyBool =  "Bool"
propTypeHsType TyNumber =  "Integer"
propTypeHsType TyString = "Text"
propTypeHsType (TySymbol _) =  "Text"
propTypeHsType (TyEnum xs)
  | areStringsStringy xs =
    let vals = map fromStringyString xs
    in "MuiSymbolEnum '[" <> (Text.intercalate ", " . map enquote $ vals) <> "]"
  | areStringsNumbery xs =
    let vals = mapMaybe (fmap show . readMaybe @Int . toS) xs
    in "MuiNatEnum '[" <> Text.intercalate ", " vals <> "]"
  | otherwise = panic "encountered enum of neither numbers or strings"
propTypeHsType (TyUnion xs) =
  parenthesise (Text.intercalate " :|: " (map propTypeHsType . altLefts $ xs))
propTypeHsType (TyArrayOf _) = "ARRAY OF"
propTypeHsType (TyObjectOf _) = "OBJECT OF"
propTypeHsType (TyShape _) = "SHAPE"
propTypeHsType (TyNullable x) =
  let ty = propTypeHsType x
  in if Text.any (== ' ') ty
       then parenthesise ("Maybe " <> parenthesise ty)
       else parenthesise ("Maybe " <> ty)

enquote :: Text -> Text
enquote s = "\"" <> s <> "\""

parenthesise
  :: (Semigroup a, IsString a)
  => a -> a
parenthesise s = "(" <> s <> ")"

strictType :: Text -> Text
strictType = ("!" <>)

resolveDefault :: Prop TypeSimple -> Maybe Text
resolveDefault prop =
  case prop ^. propDefaultValue of
    Nothing ->
      case prop ^. propType of
        (TyNullable _) -> Just "Nothing"
        _ -> Nothing
    Just def ->
      let val = def ^. defaultValueValue
          go :: PropType TypeSimple -> Maybe Text
          go ty =
            case ty of
              TyNumber ->
                case val of
                  Number n -> Just . show @Integer . truncate $ n
                  String "null" -> Just "Nothing"
                  String s -> fmap (show @Integer) . readMaybe . toS $ s
                  _ -> Nothing
              TyString ->
                case val of
                  String "null" -> Just "Nothing"
                  String s -> Just $ Text.filter (/= '\'') . show $ s
                  _ -> Nothing
              TyBool ->
                case val of
                  Bool b -> Just . show $ b
                  String "null" -> Just "Nothing"
                  String s -> fmap (show @Bool) . readMaybe . toS . ucFirst $ s
                  _ -> Nothing
              (TySymbol s) -> Just s
              (TyNullable t) ->
                case go t of
                  Just v ->
                    case v of
                      "Nothing" -> Just "Nothing"
                      _ -> Just $ "Just " <> v
                  Nothing -> Nothing
              (TyEnum _) -> Nothing
              (TyUnion _) -> Nothing
              (TyArrayOf _) -> Nothing
              (TyObjectOf _) -> Nothing
              (TyShape _) -> Nothing
      in go (prop ^. propType)

-- | How do you know the string within the string is a string? It's because it's
-- enclosed within single quotes.. (╯°□°）╯︵ ┻━┻
--
-- >>> isStringString ""
-- False
-- >>> isStringString "'"
-- False
-- >>> isStringString "''"
-- True
-- >>> isStringString " '' "
-- False
isStringStringy :: Text -> Bool
isStringStringy "" = False
isStringStringy s =
  Text.head s == '\'' &&
  Text.length s > 1 && ((Text.head . Text.reverse $ s) == '\'')

areStringsStringy :: [Text] -> Bool
areStringsStringy = all isStringStringy

fromStringyString :: Text -> Text
fromStringyString = Text.init . Text.tail

isStringNumbery :: Text -> Bool
isStringNumbery "" = False
isStringNumbery x = isJust . readMaybe @ Int . toS $ x

areStringsNumbery :: [Text] -> Bool
areStringsNumbery = all isStringNumbery
