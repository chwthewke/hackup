{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TupleSections #-}


module Hackup.Config.ParserTest where

import Test.Framework

import Prelude hiding (concat)
import Hackup.Config.Fields
import Hackup.Config.Types
import Hackup.Config.Parser
import Hackup.Config.Selectors
import Control.Applicative
import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Foldable
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text.Lens
import Data.Validation
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap

data ValueResult a = ValueResult { getValue :: Value
                                 , getResult :: V a 
                                 } deriving (Show, Eq)


defParseProp :: (Eq b) => (Value -> V b) -> ValueResult b -> Bool
defParseProp p va = p v == a
  where v = getValue va
        a = getResult va

newtype NonEmptyString = NonEmptyString { getNonEmptyString :: String } deriving (Eq)

instance Arbitrary NonEmptyString where
  arbitrary = NonEmptyString <$> listOf1 arbitrary 

newtype PositiveInteger = PositiveInteger { getPositiveInteger :: Integer } deriving (Eq)

instance Arbitrary PositiveInteger where
  arbitrary = PositiveInteger <$> arbitrary `suchThat` (0 <) 

 
class ArbFromValue a where
  arbFromValue :: Gen (ValueResult a)

instance ArbFromValue a => Arbitrary (ValueResult a) where
  arbitrary = arbFromValue

maybeField :: ConfigField -> (a -> Value) -> Maybe a -> [(String, Value)]
maybeField f v = toList . fmap (\x -> (fieldName f, v x))


mkString :: String -> Value
mkString = (_String . unpacked #)

mkArray :: [Value] -> Value
mkArray = (_Array #) . Vector.fromList 

mkObject :: [(String, Value)] -> Value
mkObject = (_Object #) . HashMap.fromList . (traverse . _1 %~ Text.pack) 

-- FileSelector

instance ArbFromValue FileSelector where
  arbFromValue = do m      <- listOf1 arbitrary
                    (p, g) <- Test.Framework.elements fileSelectorPrefixes
                    return $ ValueResult (mkString (p ++ m)) (_Success # fileSelector (g m))
                    
prop_fileSelectorFromJSON :: ValueResult FileSelector -> Bool
prop_fileSelectorFromJSON = defParseProp fileSelectorFromJSON

-- Item

instance ArbFromValue Item where
  arbFromValue = do fsv          <- arbitrary
                    itemBaseDir' <- getNonEmptyString <$> arbitrary
                    return $ ValueResult (
                      mkObject $ (fieldName itemBaseDirField, mkString itemBaseDir') :
                                 maybeField itemFilesField getValue fsv) (
                      Item itemBaseDir' <$> traverse getResult fsv)

prop_itemFromJSON :: ValueResult Item -> Bool
prop_itemFromJSON = defParseProp itemFromJSON

-- Command

instance ArbFromValue Command where
  arbFromValue = do command'       <- getNonEmptyString <$> arbitrary
                    workingDir'    <- fmap getNonEmptyString <$> arbitrary
                    ignoreFailure' <- arbitrary
                    return $ ValueResult (
                      mkObject $ [(fieldName commandField, _String # Text.pack command')] ++
                                 maybeField commandWorkingDirField mkString workingDir' ++
                                 maybeField commandIgnoreFailureField (_Bool #) ignoreFailure') (
                      _Success # Command command' workingDir' (fromMaybe False ignoreFailure'))

prop_commandFromJSON :: ValueResult Command -> Bool
prop_commandFromJSON = defParseProp commandFromJSON

-- Section

instance ArbFromValue Section where
  arbFromValue = do archiveName' <- fmap getNonEmptyString <$> arbitrary
                    archiveDir'  <- fmap getNonEmptyString <$> arbitrary
                    keep'        <- fmap getPositiveInteger <$> arbitrary
                    itemsv       <- resize 4 arbitrary
                    beforev      <- resize 4 arbitrary
                    afterv       <- resize 4 arbitrary
                    return $ ValueResult (
                      mkObject $ maybeField sectionArchiveNameField mkString archiveName' ++
                                 maybeField sectionArchiveDirField mkString archiveDir' ++
                                 maybeField sectionKeepField (_Integer #) keep' ++
                                 [(fieldName sectionItemsField,
                                   mkArray $ map getValue itemsv),
                                  (fieldName sectionBeforeField,
                                   mkArray $ map getValue beforev),
                                  (fieldName sectionAfterField,
                                   mkArray $ map getValue afterv)]) (
                      Section archiveName' archiveDir' keep' <$>
                        traverse getResult itemsv <*>
                        traverse getResult beforev <*>
                        traverse getResult afterv)
      
prop_sectionFromJSON :: ValueResult Section -> Bool
prop_sectionFromJSON = defParseProp sectionFromJSON

-- Config

instance ArbFromValue Config where
  arbFromValue = do backupRootDir' <- getNonEmptyString <$> arbitrary
                    defaultKeep'   <- getPositiveInteger <$> arbitrary
                    sectionsv      <- resize 4 arbitrary
                    sectionNames   <- map getNonEmptyString <$> arbitrary `suchThat` (\l -> l == nub l)
                    return $ ValueResult (
                      mkObject $ [(fieldName rootDirField, mkString backupRootDir'),
                                  (fieldName defaultKeepField, _Integer # defaultKeep')] ++
                                  zip sectionNames (map getValue sectionsv)) (
                      Config backupRootDir' defaultKeep' <$> Map.fromList <$> (zip sectionNames <$> traverse getResult sectionsv))

prop_configFromJSON :: ValueResult Config -> Bool
prop_configFromJSON = defParseProp configFromJSON
                      