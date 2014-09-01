{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}

module Hackup.Config.Parser(fileSelectorFromJSON, fileSelectorPrefixes,
                            commandFromJSON, sectionFromJSON,
                            configFromJSON, parseConfig, V) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson.Lens
import           Data.ByteString      (ByteString)
import           Data.Foldable        (fold)
import           Data.List.Lens
import           Data.List.NonEmpty   (NonEmpty ((:|)), intersperse)
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as Text
import           Data.Text.Lens
import           Data.Validation
import           Data.Vector.Lens
import           Data.Yaml            (Value, decodeEither)

import           Hackup.Config.Fields
import           Hackup.Config.Types
import           Hackup.Selectors


type V = AccValidation (NonEmpty String)

failV :: String -> V a
failV s = _Failure # (s :| [])

bindV :: (a -> V b) -> V a -> V b
bindV f v = case v ^? _Success of Nothing -> fmap undefined v
                                  Just a  -> f a
asV :: String -> Maybe a -> V a
asV err = maybe (failV err) pure

-- field combinators

-- 1. Value -> Maybe Value

fieldKey :: (AsValue s) => ConfigField -> Getter s (Maybe Value)
fieldKey field = pre . key $ fieldText field


-- 2. Maybe Value -> V (m Value) (m being [], Maybe or Identity)

required_ :: ConfigField -> Maybe Value -> V Value
required_ field = asV ("missing key '" ++ fieldName field ++ "'")

requiredField :: ConfigField -> Maybe Value -> V (Identity Value)
requiredField field = fmap Identity . required_ field

optionalField :: ConfigField -> Maybe Value -> V (Maybe Value)
optionalField _ = pure

optionalRepeatedField :: ConfigField -> Maybe Value -> V [Value]
optionalRepeatedField _ = maybe (pure []) array

-- 3. apply (Value -> V b) parser to get V (t a) -> V (t b)

-- note: will not add index for 0 or 1 element array, is that ok?

qualifyErrors :: Traversable t => ConfigField -> t (V a) -> t (V a)
qualifyErrors field vs = vs & traversed %@~ inField'
  where n = lengthOf folded vs
        inField' i v = v & _Failure . mapped %~ (("in " ++ fieldName field ++ loc ++ ", ") ++)
          where loc | n > 1     = " (" ++ show (i + 1) ++ " of " ++ show n ++ ")"
                    | otherwise = ""

parsedFieldM :: Traversable t => ConfigField -> (a -> V b) -> V (t a) -> V (t b)
parsedFieldM field p = bindV $ sequenceAOf traverse . qualifyErrors field . fmap p

-- 4. shake

getField :: (AsValue a) => ConfigField -> (Value -> V b) -> Getter a (V b)
getField field p = fieldKey field . to (
                     fmap runIdentity .
                     parsedFieldM field p .
                     requiredField field)

getFieldOpt :: AsValue a => ConfigField -> (Value -> V b) -> Getter a (V (Maybe b))
getFieldOpt field p = fieldKey field . to (
                        parsedFieldM field p .
                        optionalField field)

getFieldRep :: AsValue a => ConfigField -> (Value -> V b) -> Getter a (V [b])
getFieldRep field p = fieldKey field . to (
                        parsedFieldM field p .
                        optionalRepeatedField field)

withDefault :: a -> V (Maybe a) -> V a
withDefault a vma = fromMaybe a <$> vma

(.|) :: Getter a (V (Maybe b)) -> b -> Getter a (V b)
(.|) g a = g . to (withDefault a)

-- validators

array :: Value -> V [Value]
array = asV "array expected" . (^? _Array . from vector)

bool :: Value -> V Bool
bool = asV "boolean expected" . (^? _Bool)

nonEmptyString :: Value -> V String
nonEmptyString v =
  asV "non-empty string expected" $
    case v ^? _String . unpacked of Just "" -> Nothing
                                    r       -> r

positiveInt :: Value -> V Integer
positiveInt v =
  asV "positive integer expected" $
    case v ^? _Integer of Just x | x <= 0 -> Nothing
                          r               -> r

-- concrete stuff

-- FileSelector

fileSelectorPrefixes :: [ (String, String -> FileSelector) ]
fileSelectorPrefixes = [ ("glob:", Glob), ("regex:", Regex), ("", Glob) ]

parseRawFileSelector :: AsValue s => s -> Maybe FileSelector
parseRawFileSelector t = (rawFileSelector' <$> fileSelectorPrefixes) ^? (traverse . _Just)
  where rawFileSelector' (p, fs) = case t ^? (_String . unpacked . prefixed p)
                                     of Just "" -> Nothing
                                        r       -> fs <$> r


fileSelectorFromJSON :: AsValue s => s -> V FileSelector
fileSelectorFromJSON =
  asV "string matching (glob:|regex:)?.+ expected" .
    parseRawFileSelector


-- Command

commandFromJSON :: AsValue s => s -> V Command
commandFromJSON v = Command <$>
                      v ^. getField commandField nonEmptyString <*>
                      v ^. getFieldOpt commandWorkingDirField nonEmptyString .| "." <*>
                      v ^. getFieldOpt commandIgnoreFailureField bool .| False

-- Section

sectionFromJSON :: AsValue s => s -> V Section
sectionFromJSON v = Section <$>
                      v ^. getFieldOpt sectionArchiveNameField nonEmptyString <*>
                      v ^. getFieldOpt sectionArchiveDirField nonEmptyString <*>
                      v ^. getFieldOpt sectionKeepField positiveInt <*>
                      v ^. getField sectionBaseDirField nonEmptyString <*>
                      v ^. getFieldRep sectionItemsField fileSelectorFromJSON <*>
                      v ^. getFieldRep sectionBeforeField commandFromJSON <*>
                      v ^. getFieldRep sectionAfterField commandFromJSON

-- Config

configFromJSON :: AsValue s => s -> V Config
configFromJSON v = Config <$>
                     v ^. getField rootDirField nonEmptyString <*>
                     v ^. getFieldOpt defaultKeepField positiveInt .| 7 <*>
                     traverse sectionFromJSON fieldsMap
  where fields = v ^@.. members & traverse . _1 %~ Text.unpack
        userField k _ = k `notElem` (fieldName <$> [rootDirField, defaultKeepField])
        fieldsMap = Map.filterWithKey userField $ Map.fromList fields


-- IO stuff

parseValue :: ByteString -> Either String Value
parseValue = decodeEither

parseConfig :: ByteString -> Either String Config
parseConfig = over _Failure (fold . intersperse "; ") . view isoAccValidationEither . configFromJSON <=< parseValue

