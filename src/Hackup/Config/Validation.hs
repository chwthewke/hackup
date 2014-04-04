{-# LANGUAGE Rank2Types #-}
module Hackup.Config.Validation(validate) where

import Data.Validation

import Prelude hiding (foldl1)
import Data.Foldable 
import Data.List.NonEmpty
import Data.List.Lens
import Control.Lens
import Hackup.Types
import Hackup.Config.Types
import Hackup.Config.Fields
import System.FilePath.Glob


type V = AccValidation (NonEmpty String)

qualify :: Validate f => String -> f (NonEmpty String) a -> f (NonEmpty String) a
qualify s = over (_Failure . mapped) (s ++)  

singleError :: forall a. String -> V a
singleError s = _Failure # (s :| [])

nonEmptyString :: ConfigField -> String -> V ()
nonEmptyString (ConfigField n) "" = _Failure # (("Empty " ++ n) :| [])
nonEmptyString _               _  = _Success # () 

positiveInt :: (Integral a, Show a) => ConfigField -> a -> V ()
positiveInt (ConfigField n) x | x <= 0 = singleError $ n ++ " must be > 0, was " ++ show x
                              | otherwise = _Success # ()
                              
validateFileSelector :: RawFileSelector -> V FileSelector
validateFileSelector (Glob "") = undefined
                              
validate' :: RawConfig -> V Config
validate' = undefined 

validate :: RawConfig -> Try Config
validate = tryT . view isoAccValidationEither . (_Failure %~ (fold . intersperse ", ")) . validate'