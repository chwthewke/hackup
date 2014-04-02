{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Hackup.ConfigTest where
import Hackup.Config

import Prelude hiding (mapM, sequence)
import Test.Framework
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Traversable
import Data.Text
import Data.Yaml
import Data.Aeson.Types (Pair)
import Control.Lens hiding ((.=))
import Control.Applicative
import Control.Monad hiding (mapM, sequence)

-- arbitrary instances
class ToJSONG a where
  toJSONG :: Gen Bool -> a -> Gen Value

instance ToJSONG a => ToJSONG [a] where
  toJSONG = (fmap array .) . mapM . toJSONG 

newtype WithJSON a = WithJSON (a, Value) deriving Show
  
instance (Arbitrary a, ToJSONG a) => Arbitrary (WithJSON a) where
  arbitrary = do a <- arbitrary
                 v <- toJSONG arbitrary a
                 return $ WithJSON (a, v)

defPropDecode :: (Show a, Eq a, FromJSON a, ToJSONG a, Arbitrary a) => WithJSON a -> Bool
defPropDecode (WithJSON (a, v)) = parseEither parseJSON v == Right a 

-- ToJSON helper

mkPair :: ToJSONG a => Gen Bool -> Text -> a -> Gen (Maybe Pair)
mkPair bools k = liftM (Just . (k .=)) . toJSONG bools  

mkPair_ :: ToJSON a => Text -> a -> Gen (Maybe Pair)
mkPair_ k v = return . Just $ k .= v

mkPairMaybe :: ToJSONG a => Gen Bool -> Text -> Maybe a -> Gen (Maybe Pair)
mkPairMaybe bools k = sequence . fmap (fmap (k .=) . toJSONG bools)

mkPairMaybe_ :: ToJSON a => Text -> Maybe a -> Gen (Maybe Pair)
mkPairMaybe_ k = return . liftM (k .=)

mkPairMaybe' :: ToJSON a => Gen Bool -> Text -> (Bool -> Maybe a) -> Gen (Maybe Pair)
mkPairMaybe' bools k f = fmap (k .=) . f <$> bools 

mkJSONG :: [Gen (Maybe Pair)] -> Gen Value 
mkJSONG = liftM (object . catMaybes) . sequence

-- FileSelector instances

prefixFileSelector :: String -> String -> Value
prefixFileSelector p fs = String . pack $ p ++ fs

fileSelectorToJSON :: FileSelector -> Bool -> Value
fileSelectorToJSON (Glob x) True  = prefixFileSelector "glob:" x
fileSelectorToJSON (Glob x) False = prefixFileSelector "" x
fileSelectorToJSON (Regex x) _    = prefixFileSelector "regex:" x

instance ToJSONG FileSelector where
  toJSONG bs fs = liftM (fileSelectorToJSON fs) bs   
    
instance Arbitrary FileSelector where
  arbitrary = oneof [ fmap Glob arbitrary, fmap Regex arbitrary ]

prop_fileSelectorDecode :: WithJSON FileSelector -> Bool
prop_fileSelectorDecode = defPropDecode

-- Command instances

instance ToJSONG Command where
  toJSONG bools cmd = mkJSONG [ 
    mkPair_ "command" $ cmd ^. command,
    mkPairMaybe_ "workingDir" $ cmd ^. workingDir,
    mkPairMaybe' bools "ignoreFailure" $ ignoreFailure' cmd ]
    where ignoreFailure' c False = Just $ c ^. ignoreFailure
          ignoreFailure' c True  = if c ^. ignoreFailure then Just True else Nothing

instance Arbitrary Command where
  arbitrary = Command <$> arbitrary <*> arbitrary <*> arbitrary
  
prop_commandDecode :: WithJSON Command -> Bool
prop_commandDecode = defPropDecode

-- Section instances

instance ToJSONG Section where
  toJSONG bools section = mkJSONG
      [mkPairMaybe_ "archive" $ section ^. archiveName,
       mkPairMaybe_ "targetDir" $ section ^. archiveDir,
       mkPairMaybe_ "keep" $ section ^. keep,
       mkPair bools "contents" $ section ^. items,
       mkPair bools "before" $ section ^. before,
       mkPair bools "after" $ section ^. after
       ]

instance Arbitrary Section where
  arbitrary = Section <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> 
                resize 4 arbitrary <*> 
                resize 4 arbitrary <*> 
                resize 4 arbitrary 

prop_sectionDecode :: WithJSON Section -> Bool
prop_sectionDecode (WithJSON (section, json)) = parseEither (sectionFromJSON . pack $ section ^. name) json == Right (section ^. name, section) 

-- Config instances


instance ToJSONG Config where
  toJSONG bools config = do rs <- rootSec
                            sj <- sectionsJSON
                            return . object $ (rs ++ sj) 
    where rootSec = liftM catMaybes $ sequence [
                       mkPair_ "rootDir" $ config ^. backupRootDir,
                       mkPair_ "keep" $ config ^. defaultKeep
                       ] 
          sectionsJSON = mapM sectionJSONPair $ Map.elems (config ^. sections)
          sectionJSONPair section = ((pack $ section ^. name) .=) <$> toJSONG bools section

    
instance Arbitrary Config where
  arbitrary = Config <$> arbitrary <*> arbitrary <*> (resize 4 . liftM sectionMapFromList) arbitrary
    where sectionMapFromList sxs = Map.fromList [(s ^. name, s) | s <- sxs]

prop_configDecode :: WithJSON Config -> Bool
prop_configDecode = defPropDecode



