{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hackup.Config.Parser where

import Hackup.Types
import Hackup.Config.Types
import Hackup.Config.Fields
import Data.ByteString (readFile)
import Data.Maybe (mapMaybe)
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Yaml hiding ((.:), (.:?))
import Control.Applicative
import Control.Monad

extractSelectorType :: Text -> RawFileSelector
extractSelectorType fs = Prelude.head $ mapMaybe matchSelector [ ("glob:", Glob), ("regex:", Regex), ("", Glob) ]  
  where matchSelector (s, st) 
          | s `isPrefixOf` fs = Just . st . Text.unpack $ Text.drop (Text.length s) fs
          | otherwise         = Nothing

instance FromJSON RawFileSelector where
  parseJSON (String s) = return $ extractSelectorType s
  parseJSON _ = mzero

instance FromJSON Command where
  parseJSON (Object v) = Command <$>
                           v .: commandField <*>
                           v .:? commandWorkingDirField <*>
                           v .:? commandIgnoreFailureField .!= False
  parseJSON _          = mzero


instance FromJSON a => FromJSON (Section' a) where
  parseJSON (Object v) = Section' <$>
                                           v .:? sectionArchiveNameField <*>
                                           v .:? sectionArchiveDirField <*>
                                           v .:? sectionKeepField <*>
                                           v .: sectionItemsField <*>
                                           v .:? sectionBeforeField .!= [] <*>
                                           v .:? sectionAfterField .!= []
  parseJSON _          = mzero


instance FromJSON a => FromJSON (Config' a) where
  parseJSON (Object v) = Config' <$>
                           v .: rootDirField <*>
                           v .:? defaultKeepField .!= 7 <*>
                           sectionsFromJSON v
    where sectionsFromJSON = fmap Map.fromList 
                               . mapM (\(t, v') -> (Text.unpack t,) <$> parseJSON v') 
                               . HashMap.toList 
                               . HashMap.filterWithKey (\ x _ -> x `notElem` [asText rootDirField, asText defaultKeepField])
          
  parseJSON _          = mzero



parseConfig :: FilePath -> TryIO RawConfig
parseConfig = ErrorT . liftM decodeEither . Data.ByteString.readFile
